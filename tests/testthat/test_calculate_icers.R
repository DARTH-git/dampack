context("cea")

data("hund_strat")

test_that("calc_cea runs", {
  calculate_icers(hund_strat)
})
