context('psa')
library(dampack)

# test the class
test_that("psa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "psa"))
  expected_methods <- c("plot.psa", "print.psa")
  expect_equal(current_methods, expected_methods)
})

# class creation

## setup
costs <- data.frame(rbind(c(10e3, 20e3, 100e3),
               c(20e3, 40e3, 200e3)))
effectiveness <- data.frame(rbind(c(100, 50, 900),
                       c(56, 89, 700)))
strategies <- c("test", "notest", "treat")
names(costs) <- names(effectiveness) <- strategies
psa_small <- make_psa_obj(costs, effectiveness, strategies)
test_that('psa returns correct object', {
  expect_equal(psa_small$cost, costs)
  expect_equal(psa_small$effectiveness, effectiveness)
  expect_equal(psa_small$strategies, strategies)
  expect_equal(psa_small$n.strategies, 3)
  expect_equal(psa_small$n.sim, 2)
})

## methods

# plot
source('load_test_data.R')
psa_big <- make_psa_obj(costs, effectiveness, strategies)
test_that('plot.psa runs', {
  plot(psa_big)
  plot(psa_big, center=FALSE)
  plot(psa_big, ellipse=FALSE)
  plot(psa_big, center=FALSE, ellipse=FALSE)
})

# print
# use small example from above
test_that("print.psa returns correct output", {
  msg <- capture.output(print(psa_small))
  expected <- c("",
                "PSA object ",
                "------------------------------------------------- ",
                "cost: a data frame with 2 rows and 3 columns. ",
                "currency: $ ",
                "effectiveness: a data frame with 2 rows and 3 columns. ",
                "number of strategies (n.strategies): 3 ",
                "number of simulations (n.sim): 2 " ,
                "strategies: test, notest, treat  ")
  expect_equal(msg, expected)
})
