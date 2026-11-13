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
