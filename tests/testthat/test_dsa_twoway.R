context("dsa twoway")
library(dampack)

# create of example data
nsamps <- 20
params <- data.frame(expand.grid("param1" = seq(0, 1, length.out = nsamps),
                                 "param2" = seq(4, 5, length.out = nsamps)))
effect <- data.frame("s1" = 5 * params$param1 + 4 * params$param2,
                     "s2" = 2 * params$param1 ^ 2 - 3 * params$param2 + 10)
cost <- data.frame("s1" = 1e4 + 1000 * params$param1 - 900 * params$param2,
                   "s2" = 1e5 + 1000 * params$param1 - 900 * params$param2)

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
  expect_equal(dsa$parnames, c("param1", "param2"))
})

# print
test_that("dsa prints", {
  dsa <- create_dsa_twoway(params, effect, strategies, cost)
  output <- capture.output(print(dsa))
  expect_equal(output[2], "Two-way Deterministic SA Object ")
})
