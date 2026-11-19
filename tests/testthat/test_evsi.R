library(dampack)

# test return object
## setup
data("cancer_psa_data")
psa_obj <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)

test_that("evsi runs", {
  expect_silent(calc_evsi(wtp = 1e5,
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

test_that("evsi produces error when nsamps < nparams", {
  expect_error(calc_evsi(wtp = 1e5,
                         psa = psa_obj,
                         params = c("pFailSurg", "pFailChemo"),
                         outcome = "nmb",
                         progress = FALSE,
                         k = 3),
               "The number of parameters to be estimated by the metamodel
                     cannot be greater than the number of PSA samples")
})

# predict_ga() matches n and n0 to parameters by name --------------------------
psa_ga <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                       cancer_psa_data$parameters, cancer_psa_data$strategies)
df_ga <- data.frame(y = calculate_outcome("nmb_loss_voi", psa_ga$cost,
                                          psa_ga$effectiveness, 1e5)[[1]],
                    psa_ga$parameters)
gam_ab <- mgcv::gam(y ~ s(pFailSurg, k = 4) + s(pFailChemo, k = 4) +
                      ti(pFailSurg, pFailChemo, k = 4), data = df_ga)
gam_ba <- mgcv::gam(y ~ s(pFailChemo, k = 4) + s(pFailSurg, k = 4) +
                      ti(pFailChemo, pFailSurg, k = 4), data = df_ga)

test_that("predict_ga() matches a named n and n0 to parameters by name", {
  v_n <- c(pFailSurg = 500, pFailChemo = 20)
  v_n0 <- c(pFailSurg = 10, pFailChemo = 40)
  expect_equal(predict_ga(gam_ab, v_n, v_n0), predict_ga(gam_ab, c(500, 20), c(10, 40)))
  expect_equal(predict_ga(gam_ab, rev(v_n), rev(v_n0)), predict_ga(gam_ab, v_n, v_n0))
  # the result does not depend on the order of the terms in the formula
  expect_equal(predict_ga(gam_ba, v_n, v_n0), predict_ga(gam_ab, v_n, v_n0),
               tolerance = 1e-6)
})

test_that("predict_ga() gives informative errors for a wrong n or n0", {
  expect_error(predict_ga(gam_ab, c(pFailSurg = 100), 10), "Missing: pFailChemo")
  expect_error(predict_ga(gam_ab, c(pFailSurg = 100, pFail = 1), 10), "Unknown: pFail")
  expect_error(predict_ga(gam_ab, c(100, 200, 300), 10), "one value per parameter")
  expect_error(predict_ga(gam_ab, 100, -1), "non-negative")
})

test_that("predict_ga() shrinks the basis of a one-parameter smooth toward its mean", {
  gam_a <- mgcv::gam(y ~ s(pFailChemo), data = df_ga)
  m_x <- predict(gam_a, type = "lpmatrix")
  vrf <- sqrt(100 / (100 + 10))
  m_x_ga <- vrf * m_x + (1 - vrf) * matrix(colMeans(m_x), nrow(m_x), ncol(m_x), byrow = TRUE)
  expect_equal(c(predict_ga(gam_a, 100, 10)), unname(c(m_x_ga %*% coef(gam_a))))
})

test_that("calc_evsi() matches a named n and n0 to params by name", {
  evsi_by_name <- calc_evsi(psa_ga, wtp = 1e5, params = c("pFailSurg", "pFailChemo"),
                            outcome = "nmb", k = 4, n_by_param = TRUE,
                            n = c(pFailChemo = 20, pFailSurg = 500),
                            n0 = c(pFailChemo = 40, pFailSurg = 10), progress = FALSE)
  evsi_by_pos <- calc_evsi(psa_ga, wtp = 1e5, params = c("pFailSurg", "pFailChemo"),
                           outcome = "nmb", k = 4, n_by_param = TRUE,
                           n = c(500, 20), n0 = c(10, 40), progress = FALSE)
  expect_equal(evsi_by_name$df_evsi, evsi_by_pos$df_evsi)
  # names on a vector of study sizes are not taken as parameter names
  evsi_sizes <- calc_evsi(psa_ga, wtp = 1e5, params = "pFailChemo", outcome = "nmb",
                          n = c(small = 10, large = 100), progress = FALSE)
  expect_equal(evsi_sizes$df_evsi$n, c(10, 100), ignore_attr = TRUE)
})

test_that("NHB at a willingness-to-pay threshold of 0 gives an informative error", {
  psa_nhb <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                          cancer_psa_data$parameters, cancer_psa_data$strategies)
  expect_error(calc_evsi(psa_nhb, wtp = c(0, 1e5), params = "pFailChemo", outcome = "nhb",
                         progress = FALSE),
               "not defined at a willingness-to-pay threshold of 0")
  expect_error(calc_evppi(psa_nhb, wtp = 0, params = "pFailChemo", outcome = "nhb",
                          progress = FALSE),
               "not defined at a willingness-to-pay threshold of 0")
  # NMB at a threshold of 0 is defined
  expect_no_error(calc_evppi(psa_nhb, wtp = 0, params = "pFailChemo", progress = FALSE))
})
