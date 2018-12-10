context("ceac")
library(dampack)

# setup
source('load_test_data.R')
psa_obj <- psa_results(costs, effectiveness, strategies)

# tests
test_that("result has classes 'data.frame' and 'ceac'", {
  c <- ceac(wtp, psa_obj)
  expect_true(inherits(c, "data.frame"))
  expect_true(inherits(c, "ceac"))
})

test_that("handles missing strategy", {
  psa_missing <- psa_results(costs, effectiveness)
  c_missing <- ceac(wtp, psa_missing)
  expected_generic_strat <- c("Strategy_1", "Strategy_2", "Strategy_3")
  obtained_generic_strat <- sort(unique(c_missing$Strategy))
  expect_equal(expected_generic_strat, obtained_generic_strat)
})

