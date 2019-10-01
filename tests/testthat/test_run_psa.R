context("run_psa")
library(dampack)

test_func <- function(params, extra_param) {
  normal_param <- params[["normal_param"]]
  lognorm_param <- params[["lognorm_param"]]
  beta_param <- params[["beta_param"]]
  gamma_param <- params[["gamma_param"]]
  level1 <- params[["level1"]]
  level2 <- params[["level2"]]
  level3 <- params[["level3"]]
  bootstrap_param <- params[["bootstrap_param"]]

  effect1 <- normal_param + lognorm_param * extra_param + beta_param + gamma_param +
    level1 + level2 + level3 + bootstrap_param
  cost1 <- - normal_param - lognorm_param - beta_param - gamma_param - level1 - level2 - level3 - bootstrap_param

  effect2 <- normal_param - lognorm_param * extra_param + beta_param - gamma_param +
    level1 - level2 + level3 - bootstrap_param
  cost2 <- - normal_param + lognorm_param - beta_param + gamma_param - level1 + level2 - level3 + bootstrap_param

  output <- data.frame(strategies = c("mystrat1", "mystrat2"),
                       effect = c(effect1, effect2),
                       cost = c(cost1, cost2))

  return(output)
}

test <- gen_psa_samp(params = c("normal_param", "lognorm_param", "beta_param",
                                "gamma_param", "dirichlet_param", "bootstrap_param"),
                     dist = c("normal", "log-normal", "beta", "gamma", "dirichlet", "bootstrap"),
                     parameterization_type = c("mean, sd", "mean, sd", "mean, sd", "mean, sd",
                                               "value, mean_prop, sd", "value, weight"),
                     dist_params = list(c(1, 2), c(1, 3), c(.5, .2), c(100, 1),
                                        data.frame(value = c("level1", "level2", "level3"),
                                                   mean_prop = c(.1, .4, .5), sd = c(.05, .01, .1)),
                                        data.frame(value = c(1, 2, 4, 6, 7, 8), weight = c(1, 1, 1, 1, 1, 4))),
                     nsamp = 100)

test_that("run_psa runs", {
  expect_silent( runtest <- run_psa(test, test_func, outcomes = c("cost", "effect"),
                                  strategies = c("customstrat1", "customstrat2"), extra_param = 1.5))
})
