context("psa")
library(dampack)

# test the class
test_that("psa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "psa"))
  expected_methods <- c("plot.psa", "print.psa", "summary.psa")
  expect_setequal(current_methods, expected_methods)
})

# class creation

## setup
sm_costs <- data.frame(rbind(c(10e3, 20e3, 100e3),
               c(20e3, 40e3, 200e3)))
sm_effectiveness <- data.frame(rbind(c(100, 50, 900),
                       c(56, 89, 700)))
sm_strategies <- c("test", "notest", "treat")
names(sm_costs) <- names(sm_effectiveness) <- sm_strategies
psa_small <- make_psa_obj(sm_costs, sm_effectiveness, sm_strategies)
test_that("psa returns correct object", {
  expect_equal(psa_small$cost, sm_costs)
  expect_equal(psa_small$effectiveness, sm_effectiveness)
  expect_equal(psa_small$strategies, sm_strategies)
  expect_equal(psa_small$n_strategies, 3)
  expect_equal(psa_small$n_sim, 2)
})

## methods

# plot
source("load_test_data.R")
psa_big <- make_psa_obj(costs, effectiveness, strategies)
test_that("plot.psa runs", {
  expect_silent({
    plot(psa_big)
    plot(psa_big, center = FALSE)
    plot(psa_big, ellipse = FALSE)
    plot(psa_big, center = FALSE, ellipse = FALSE)
  })
})

# print
# use small example from above
test_that("print.psa returns correct output", {
  msg <- capture.output(print(psa_small))
  expected <- c("",
                "PSA object ",
                "------------------------------------------------- ",
                "number of strategies (n_strategies): 3 ",
                "strategies: test, notest, treat  ",
                "number of simulations (n_sim): 2 ",
                "cost: a data frame with 2 rows and 3 columns. ",
                "effectiveness: a data frame with 2 rows and 3 columns. ",
                "currency: $ ")
  expect_equal(msg, expected)
})

# summary
test_that("summary.psa returns correct output", {
  # no sds
  expected_df <- data.frame("Strategy" = sm_strategies,
                            "Cost" = colMeans(sm_costs),
                            "Effect" = colMeans(sm_effectiveness),
                            stringsAsFactors = FALSE,
                            row.names = NULL)
  calc_df <- summary(psa_small)
  expect_equal(expected = expected_df, calc_df)
})
