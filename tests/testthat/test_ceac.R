context("ceac")
library(dampack)

# test the class
test_that("ceac has all methods we'd expect", {
  current_methods <- as.vector(methods(class = ceac))
  expected_methods <- c("plot.ceac", "summary.ceac")
  expect_equal(current_methods, expected_methods)
})

# test class creation

## setup
source('load_test_data.R')
psa_obj <- psa_results(costs, effectiveness, strategies)

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


# test methods

## summary
test_that("message is correct in summary.ceac", {
  c <- ceac(wtp, psa_obj)
  msg <- capture.output(summary(c))
  expected <- c("There was one switch in the optimal strategy.",
                "At WTP = 51000 the optimal strategy changed from Radio to Chemo")
  expect_equal(expected, msg)
})

## plot
test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, psa_obj)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_true(inherits(gf, "ggplot"))
  gnof <- plot(ceac_obj, rontier = FALSE)
  expect_true(inherits(gnof, "ggplot"))
})
