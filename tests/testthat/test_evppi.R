context("evppi")
library(dampack)


# test return object
## setup
data("example_psa")
psa_obj <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

test_that("evppi runs", {
  expect_silent(calc_evppi(wtp = 1e5,
                           psa = psa_obj,
                           params = c("pFailSurg", "pFailChemo"),
                           outcome = "nmb",
                           progress = FALSE))
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
                         progress = FALSE),
              "The number of parameters to be estimated by the metamodel
                     cannot be greater than the number of PSA samples")
})
