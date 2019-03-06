context("dsa twoway")
library(dampack)

# create of example data
nsamps <- 20
params <- data.frame(expand.grid("parm1" = seq(0, 1, length.out = nsamps),
                                 "parm2" = seq(4, 5, length.out = nsamps)))
effect <- data.frame("s1" = 5 * params$parm1 + 4 * params$parm2 ,
                     "s2" = 2 * params$parm1 ^ 2 - 3 * params$parm2 + 10)
cost <- data.frame("s1" = 1e4 + 1000 * params$parm1 - 900 * params$parm2,
                   "s2" = 1e5 + 1000 * params$parm1 - 900 * params$parm2)

strategies <- c("s1", "s2")

# class creation
test_that("dsa class creation", {
  dsa <- create_dsa_twoway(params, effect, strategies, cost)
  expect_is(dsa, "dsa_twoway")
  expect_is(dsa, "sa")
})

# members within class
test_that("dsa class creation", {
  dsa <- create_dsa_twoway(params, effect, strategies, cost)
  expect_equal(dsa$parameters, params)
  expect_equal(dsa$strategies, strategies)
  expect_equal(dsa$n_strategies, 2)
  expect_equal(dsa$cost, cost)
  expect_equal(dsa$effectiveness, effect)
  expect_equal(dsa$n_sim, 400)
  expect_equal(dsa$parnames, c("parm1", "parm2"))
})

# print
test_that("dsa prints", {
  dsa <- create_dsa_twoway(params, effect, strategies, cost)
  output <- capture.output(print(dsa))
  expect_equal(output[2], "Two-way Deterministic SA Object ")
})
