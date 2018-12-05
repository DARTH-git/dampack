context("ceac")
library(dampack)

## test data
wtp <- c(0, 10, 20)
strategies <- c("good", "bad", "worse")
costs <- matrix(c(30, 20, 100,
                     10, 10, 5),
                   byrow = TRUE,
                   nrow = 2)
effectiveness <- matrix(c(20, 10, 50,
                          2, 11, 3),
                        byrow = TRUE,
                        nrow = 2)

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
