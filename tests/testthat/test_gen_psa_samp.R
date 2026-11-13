context("gen_psa_samp")
library(dampack)



test_that("gen_psa_samp runs", {
  expect_silent(test <- gen_psa_samp(params = c("normal_param", "lognorm_param", "beta_param",
                                                "gamma_param", "dirichlet_param", "bootstrap_param"),
                                     dists = c("normal", "log-normal", "beta", "gamma", "dirichlet", "bootstrap"),
                                     parameterization_types = c("mean, sd", "mean, sd", "mean, sd", "mean, sd",
                                                                "value, mean_prop, sd", "value, weight"),
                                     dists_params = list(c(1, 2), c(1, 3), c(.5, .2), c(100, 1),
                                                         data.frame(value = c("level1", "level2", "level3"),
                                                                    mean_prop = c(.1, .4, .5), sd = c(.05, .01, .1)),
                                                         data.frame(value = c(1, 2, 4, 6, 7, 8),
                                                                    weight = c(1, 1, 1, 1, 1, 4))),
                                     nsamp = 100))
})

# "value, weight" used to be missing from the parameterization choices, so it was
# silently dropped and every later parameterization shifted by one position (#175)
test_that("a bootstrap parameter before others keeps their parameterizations aligned", {
  set.seed(175)
  df_boot <- data.frame(value = c(1, 2, 4), weight = c(1, 1, 2))
  samp <- gen_psa_samp(params = c("boot_param", "beta_param", "gamma_param"),
                       dists = c("bootstrap", "beta", "gamma"),
                       parameterization_types = c("value, weight", "a, b", "shape, scale"),
                       dists_params = list(df_boot, c(2, 8), c(4, 0.5)),
                       nsamp = 5000)
  expect_true(all(samp$boot_param >= 1 & samp$boot_param <= 4))
  # beta(a = 2, b = 8) has mean 0.2; a shifted parameterization would not
  expect_equal(mean(samp$beta_param), 0.2, tolerance = 0.02)
  expect_true(all(samp$beta_param > 0 & samp$beta_param < 1))
  # gamma(shape = 4, scale = 0.5) has mean 2
  expect_equal(mean(samp$gamma_param), 2, tolerance = 0.1)
})

test_that("unknown distributions or parameterizations give informative errors", {
  expect_error(gen_psa_samp(params = c("a", "b"), dists = c("beta", "betta"),
                            parameterization_types = c("a, b", "a, b"),
                            dists_params = list(c(1, 1), c(1, 1))),
               "Unknown `dists`: \"betta\"")
  expect_error(gen_psa_samp(params = "a", dists = "beta",
                            parameterization_types = "alpha, beta",
                            dists_params = list(c(1, 1))),
               "Unknown `parameterization_types`: \"alpha, beta\"")
})

test_that("arguments of different lengths give an informative error", {
  expect_error(gen_psa_samp(params = c("a", "b"), dists = "beta",
                            parameterization_types = c("a, b", "a, b"),
                            dists_params = list(c(1, 1), c(1, 1))),
               "must all have the same length")
})
