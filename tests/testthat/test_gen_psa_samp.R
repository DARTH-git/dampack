context("gen_psa_samp")
library(dampack)



test_that("gen_psa_samp runs", {
  expect_silent(test <- gen_psa_samp(params = c("normal_param", "lognorm_param","beta_param","gamma_param","dirichlet_param"),
                     dist = c("normal", "log-normal","beta","gamma","dirichlet"),
                     parameterization_type = c("mean, sd", "mean, sd","mean, sd","mean, sd", "value, mean_prop, sd"),
                     dist_params = list(c(1,2),c(1,3), c(.5,.2), c(100,1),
                                        data.frame(value = c("level1","level2","level3"),
                                                   mean_prop = c(.1,.4,.5), sd = c(.05, .01, .1))),
                     n_samp =100))
})


