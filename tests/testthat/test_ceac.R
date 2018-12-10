context("ceac")
library(dampack)

source('load_test_data.R')

## tests

test_that("result has classes 'data.frame' and 'ceac'", {
  c <- ceac(wtp, costs, effectiveness, strategies)
  expect_true(inherits(c, "data.frame"))
  expect_true(inherits(c, "ceac"))
})

test_that("handles missing strategy", {
  c <- ceac(wtp, costs, effectiveness)
  expected_generic_strat <- c("Strategy_1", "Strategy_2", "Strategy_3")
  obtained_generic_strat <- sort(unique(c$Strategy))
  expect_equal(expected_generic_strat, obtained_generic_strat)
})

