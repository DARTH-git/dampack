context('psa_results')
library(dampack)

# test the class
# todo

# class creation

## setup
costs <- rbind(c(10e3, 20e3, 100e3),
               c(20e3, 40e3, 200e3))
effectiveness <- rbind(c(100, 50, 900),
                       c(56, 89, 700))
strategies <- c("test", "notest", "treat")

test_that('psa_results returns correct object', {
  psa_obj <- psa_results(costs, effectiveness, strategies)
  expect_equal(psa_obj$cost, costs)
  expect_equal(psa_obj$effectiveness, effectiveness)
  expect_equal(psa_obj$strategies, strategies)
  expect_equal(psa_obj$n.strategies, 3)
  expect_equal(psa_obj$n.sim, 2)
})

