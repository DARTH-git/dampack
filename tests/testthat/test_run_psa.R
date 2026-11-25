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

  output
}

test <- gen_psa_samp(params = c("normal_param", "lognorm_param", "beta_param",
                                "gamma_param", "dirichlet_param", "bootstrap_param"),
                     dists = c("normal", "log-normal", "beta", "gamma", "dirichlet", "bootstrap"),
                     parameterization_types = c("mean, sd", "mean, sd", "mean, sd", "mean, sd",
                                                "value, mean_prop, sd", "value, weight"),
                     dists_params = list(c(1, 2), c(1, 3), c(.5, .2), c(100, 1),
                                         data.frame(value = c("level1", "level2", "level3"),
                                                    mean_prop = c(.1, .4, .5), sd = c(.05, .01, .1)),
                                         data.frame(value = c(1, 2, 4, 6, 7, 8), weight = c(1, 1, 1, 1, 1, 4))),
                     nsamp = 100)

test_that("run_psa runs", {
  expect_silent(runtest <- run_psa(psa_samp = test,
                                   FUN = test_func,
                                   outcomes = c("cost", "effect"),
                                   strategies = c("customstrat1", "customstrat2"),
                                   extra_param = 1.5,
                                   progress = FALSE))
})

# run_psa() always dropped the first column of psa_samp, assuming it was the nsamp index
# that gen_psa_samp() adds, so a hand-built sample lost a real parameter (#174)
simple_fun <- function(params) {
  data.frame(strategy = c("A", "B"),
             cost = c(params[["c_a"]], params[["c_b"]]),
             effect = c(1, params[["e_b"]]))
}

test_that("run_psa() keeps every parameter of a hand-built PSA sample", {
  psa_hand <- data.frame(c_a = c(10, 11, 12), c_b = c(20, 21, 22), e_b = c(1.5, 1.6, 1.7))
  out <- run_psa(psa_hand, FUN = simple_fun, outcomes = c("cost", "effect"),
                 strategies = c("A", "B"), progress = FALSE)
  expect_identical(out$cost$parnames, c("c_a", "c_b", "e_b"))
  expect_equal(out$cost$parameters, psa_hand)
  expect_equal(out$cost$other_outcome$A, psa_hand$c_a)
})

test_that("run_psa() still drops the nsamp index added by gen_psa_samp()", {
  out <- run_psa(psa_samp = test, FUN = test_func, outcomes = c("cost", "effect"),
                 strategies = c("customstrat1", "customstrat2"), extra_param = 1.5, progress = FALSE)
  expect_false("nsamp" %in% out$cost$parnames)
  expect_identical(out$cost$parnames, setdiff(names(test), "nsamp"))
})

test_that("run_psa() reports a FUN that fails on the PSA parameters", {
  bad_fun <- function(params) stop("this model cannot run")
  psa_hand <- data.frame(c_a = 1, c_b = 2, e_b = 3)
  expect_error(run_psa(psa_hand, FUN = bad_fun, strategies = c("A", "B"), progress = FALSE),
               "FUN is not well defined.*this model cannot run")
})

# remaining run_psa() paths (#215)
test_that("run_psa() fills in parameters from params_basecase", {
  basecase <- list(c_a = 10, c_b = 20, e_b = 1.5)
  psa_part <- data.frame(c_b = c(25, 30))
  out <- run_psa(psa_part, params_basecase = basecase, FUN = simple_fun,
                 outcomes = "cost", strategies = c("A", "B"), progress = FALSE)
  # c_a comes from the base case; c_b from the PSA sample
  expect_equal(out$cost$other_outcome$A, c(10, 10))
  expect_equal(out$cost$other_outcome$B, c(25, 30))
})

test_that("run_psa() names strategies from FUN when strategies is NULL", {
  psa_hand <- data.frame(c_a = 1, c_b = 2, e_b = 3)
  out <- run_psa(psa_hand, FUN = simple_fun, outcomes = "cost", progress = FALSE)
  expect_identical(out$cost$strategies, c("st_A", "st_B"))
})

test_that("run_psa() checks FUN's output and the requested outcomes", {
  psa_hand <- data.frame(c_a = 1, c_b = 2, e_b = 3)
  not_df <- function(params) c(1, 2)
  expect_error(run_psa(psa_hand, FUN = not_df, strategies = c("A", "B"), progress = FALSE),
               "should return a data.frame")
  expect_error(run_psa(psa_hand, FUN = simple_fun, strategies = c("A", "B", "C"), progress = FALSE),
               "number of strategies")
  expect_error(run_psa(psa_hand, FUN = simple_fun, outcomes = "qaly", strategies = c("A", "B"),
                       progress = FALSE),
               "not in FUN outcomes")
})

test_that("run_psa() reports progress when asked", {
  psa_hand <- data.frame(c_a = 1:10, c_b = 1:10, e_b = 1:10)
  expect_output(run_psa(psa_hand, FUN = simple_fun, outcomes = "cost", strategies = c("A", "B"),
                        progress = TRUE), "100 % done")
})
