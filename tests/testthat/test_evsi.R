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

test_that("calc_evsi() computes the EVSI in net monetary benefit by default", {
  psa_def <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                          cancer_psa_data$parameters, cancer_psa_data$strategies)
  evsi_default <- calc_evsi(psa_def, wtp = 1e5, params = "pFailChemo", progress = FALSE)
  evsi_nmb <- calc_evsi(psa_def, wtp = 1e5, params = "pFailChemo", outcome = "nmb",
                        progress = FALSE)
  expect_equal(evsi_default$df_evsi, evsi_nmb$df_evsi)
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
