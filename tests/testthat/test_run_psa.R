context("run_psa")
library(dampack)

test_func <- function(params, extra_param){
  normal_param <- params[["normal_param"]]
  lognorm_param <- params[["lognorm_param"]]

  effect <- normal_param + lognorm_param * extra_param
  cost <- - normal_param - lognorm_param

  output <- data.frame(strategy = "mystrat",
                       effect = effect,
                       cost = cost)
  return(output)
}

test <- gen_psa_samp(params = c("normal_param", "lognorm_param","beta_param","gamma_param","dirichlet_param"),
                     dist = c("normal", "log-normal","beta","gamma","dirichlet"),
                     parameterization_type = c("mean, sd", "mean, sd","mean, sd","mean, sd", "value, mean_prop, sd"),
                     dist_params = list(c(1,2),c(1,3), c(.5,.2), c(100,1),
                                        data.frame(value = c("level1","level2","level3"),
                                                   mean_prop = c(.1,.4,.5), sd = c(.05, .01, .1))),
                     n_samp =100)
test_that("run_psa runs", {
  expect_silent(run_psa(test, test_func, outcomes = c("cost","effect"), cost_outcome = "cost",
                        effectiveness_outcome = "effect",
                        strategies = "customstrat", currency = "$", extra_param = 1.5))
})





