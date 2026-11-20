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

test_that("the default k falls back to 3 when the default metamodel is too large", {
  v_params_5 <- names(psa_k$parameters)[1:5]
  expect_warning(mm <- metamodel("multiway", psa_k, v_params_5, outcome = "nmb_loss_voi",
                                 wtp = 1e5, type = "gam", k = -1),
                 "k has been set to 3")
  expect_match(deparse1(formula(mm$mods[[1]])), "k = 3", fixed = TRUE)
})
