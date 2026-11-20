library(dampack)


# test return object
## setup
data("cancer_psa_data")
psa_obj <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)

test_that("evppi runs", {
  expect_silent(calc_evppi(wtp = 1e5,
                           psa = psa_obj,
                           params = c("pFailSurg", "pFailChemo"),
                           outcome = "nmb",
                           progress = FALSE,
                           k = 3))
})

psa_obj$cost <- psa_obj$cost[1, ]
psa_obj$effectiveness <- psa_obj$effectiveness[1, ]
psa_obj$parameters <- psa_obj$parameters[1, ]
psa_obj$n_sim <- 1

test_that("evppi produces error when nsamps < nparams", {
  expect_error(calc_evppi(wtp = 1e5,
                          psa = psa_obj,
                          params = c("pFailSurg", "pFailChemo"),
                          outcome = "nmb",
                          progress = FALSE,
                          k = 3),
               "The number of parameters to be estimated by the metamodel
                     cannot be greater than the number of PSA samples")
})

# default basis dimensions ------------------------------------------------------
psa_k <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                      cancer_psa_data$parameters, cancer_psa_data$strategies)
v_params_k <- c("pFailSurg", "pFailChemo")

test_that("calc_evppi() with the default k fits mgcv's default metamodel", {
  expect_no_warning(
    evppi_k <- calc_evppi(psa_k, wtp = 1e5, params = v_params_k, progress = FALSE)
  )
  df_loss <- calculate_outcome("nmb_loss_voi", psa_k$cost, psa_k$effectiveness, 1e5)
  m_fitted <- sapply(df_loss, function(y) {
    fitted(mgcv::gam(y ~ s(pFailSurg) + s(pFailChemo) + ti(pFailSurg, pFailChemo),
                     data = data.frame(y, psa_k$parameters)))
  })
  expect_equal(evppi_k$df_evppi$EVPPI, mean(apply(m_fitted, 1, max)))
})

test_that("an explicit k below 3 is still raised to 3", {
  expect_warning(calc_evppi(psa_k, wtp = 1e5, params = v_params_k, k = 2, progress = FALSE),
                 "minimum value of 3")
})

test_that("mm_gam_spec() includes all interactions unless the metamodel is too large", {
  # all interactions: 1 + 3 * 9 + 3 * 4^2 + 4^3 = 140 coefficients
  expect_equal(mm_gam_spec(-1, 3), list(k = -1, max_order = 3))
  # 645 coefficients with all interactions; pairwise only, 133
  expect_warning(l_spec <- mm_gam_spec(-1, 4), "includes only pairwise interactions",
                 class = "dampack_metamodel_spec")
  expect_equal(l_spec, list(k = -1, max_order = 2))
  # with k = 3, all interactions of 5 parameters fit (243 coefficients)
  expect_equal(mm_gam_spec(3, 5), list(k = 3, max_order = 5))
  # pairwise with the default k is too large for 8 parameters, so k = 3 is used
  expect_warning(l_spec <- mm_gam_spec(-1, 8), "with k = 3")
  expect_equal(l_spec, list(k = 3, max_order = 2))
  expect_error(mm_gam_spec(10, 8), "even with pairwise interactions only")
  expect_equal(mm_gam_spec(-1, 1), list(k = -1, max_order = 1))
})

test_that("the metamodel formula has s() of each parameter and ti() of every subset", {
  mm <- metamodel("multiway", psa_k, c("pFailSurg", "pFailChemo", "pDieSurg"),
                  outcome = "nmb_loss_voi", wtp = 1e5, type = "gam", k = 4)
  v_labels <- vapply(mm$mods[[1]]$smooth, `[[`, character(1), "label")
  expect_equal(v_labels, c("s(pFailSurg)", "s(pFailChemo)", "s(pDieSurg)",
                           "ti(pFailSurg,pFailChemo)", "ti(pFailSurg,pDieSurg)",
                           "ti(pFailChemo,pDieSurg)", "ti(pFailSurg,pFailChemo,pDieSurg)"))
})

test_that("the warning about a reduced metamodel is given once per call", {
  v_params_4 <- c("pFailSurg", "pFailChemo", "pDieSurg", "muDieCancer")
  v_warnings <- capture_warnings(
    calc_evppi(psa_k, wtp = c(5e4, 1e5), params = v_params_4, progress = FALSE)
  )
  expect_length(v_warnings, 1)
  expect_match(v_warnings, "includes only pairwise interactions")
})

# EVPPI of several parameters with known values ---------------------------------
# Two strategies; the net benefit of S2 is f(a, b, cc) plus noise from d, a
# parameter not of interest, and the net benefit of S1 is 0. With independent
# standard normal parameters, EVPPI({a, b, cc}) = E[max(0, f)].
make_psa_known <- function(f, n_sim = 3000, seed = 1) {
  set.seed(seed)
  df_par <- data.frame(a = rnorm(n_sim), b = rnorm(n_sim), cc = rnorm(n_sim),
                       d = rnorm(n_sim))
  make_psa_obj(cost = data.frame(S1 = rep(0, n_sim), S2 = rep(0, n_sim)),
               effectiveness = data.frame(S1 = rep(0, n_sim),
                                          S2 = f(df_par) + df_par$d),
               parameters = df_par, strategies = c("S1", "S2"))
}
v_abc <- c("a", "b", "cc")

test_that("EVPPI of three parameters includes their pairwise interactions", {
  psa_ab <- make_psa_known(function(x) x$a * x$b)
  evppi <- calc_evppi(psa_ab, wtp = 1, params = v_abc, progress = FALSE)
  expect_equal(evppi$df_evppi$EVPPI, 1 / pi, tolerance = 0.1)  # E|ab| / 2
})

test_that("EVPPI of three parameters includes their three-way interaction", {
  psa_abc <- make_psa_known(function(x) x$a * x$b * x$cc)
  evppi <- calc_evppi(psa_abc, wtp = 1, params = v_abc, progress = FALSE)
  expect_equal(evppi$df_evppi$EVPPI, (2 / pi)^1.5 / 2, tolerance = 0.1)  # E|abc| / 2
})

test_that("polynomial EVPPI of several parameters includes their interactions", {
  psa_ab <- make_psa_known(function(x) x$a * x$b)
  evppi <- calc_evppi(psa_ab, wtp = 1, params = c("a", "b"), type = "poly",
                      progress = FALSE)
  expect_equal(evppi$df_evppi$EVPPI, 1 / pi, tolerance = 0.1)
})

test_that("EVSI of three parameters applies each sample size to its own parameter", {
  # f depends on a and cc only, so the EVSI is the EVPPI when both are fully
  # informed and about 0 when cc is not informed, whatever happens to b
  psa_ac <- make_psa_known(function(x) x$a * x$cc)
  evppi <- calc_evppi(psa_ac, wtp = 1, params = v_abc, progress = FALSE)$df_evppi$EVPPI
  evsi_ac <- calc_evsi(psa_ac, wtp = 1, params = v_abc, outcome = "nmb", n_by_param = TRUE,
                       n = c(a = 1e8, b = 0, cc = 1e8), n0 = 1, progress = FALSE)
  evsi_a <- calc_evsi(psa_ac, wtp = 1, params = v_abc, outcome = "nmb", n_by_param = TRUE,
                      n = c(a = 1e8, b = 1e8, cc = 0), n0 = 1, progress = FALSE)
  # (not exact: with b uninformed, the noise fitted by the terms of b is shrunk)
  expect_equal(evsi_ac$df_evsi$EVSI, evppi, tolerance = 0.02)
  expect_lt(evsi_a$df_evsi$EVSI, 0.1 * evppi)
})
