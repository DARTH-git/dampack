context('psa')
library(dampack)

# test the class
# todo

# class creation

## setup
costs <- data.frame(rbind(c(10e3, 20e3, 100e3),
               c(20e3, 40e3, 200e3)))
effectiveness <- data.frame(rbind(c(100, 50, 900),
                       c(56, 89, 700)))
strategies <- c("test", "notest", "treat")
names(costs) <- names(effectiveness) <- strategies

test_that('psa returns correct object', {
  psa_obj <- psa(costs, effectiveness, strategies)
  expect_equal(psa_obj$cost, costs)
  expect_equal(psa_obj$effectiveness, effectiveness)
  expect_equal(psa_obj$strategies, strategies)
  expect_equal(psa_obj$n.strategies, 3)
  expect_equal(psa_obj$n.sim, 2)
})

## methods
context("ceac")

## setup
source('load_test_data.R')
psa_obj <- psa(costs, effectiveness, strategies)

