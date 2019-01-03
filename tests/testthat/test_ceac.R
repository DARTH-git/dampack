context("ceac")
library(dampack)

# test the class
test_that("ceac has all methods we'd expect", {
  current_methods <- as.vector(methods(class = ceac))
  expected_methods <- c("plot.ceac", "summary.ceac", "print.ceac", "head.ceac")
  expect_setequal(current_methods, expected_methods)
})

# test class creation

## setup
data("example_psa")
wtp <- example_psa$wtp
psa_obj <- make_psa_obj(example_psa$costs,
                        example_psa$effectiveness,
                        example_psa$strategies)

test_that("result has class 'ceac'", {
  c <- ceac(wtp, psa_obj)
  expect_true(inherits(c, "data.frame"))
  expect_true(inherits(c, "ceac"))
})

test_that("handles missing strategy", {
  psa_missing <- make_psa_obj(example_psa$costs,
                              example_psa$effectiveness)
  c_missing <- ceac(wtp, psa_missing)
  expected_generic_strat <- c("Strategy_1", "Strategy_2", "Strategy_3")
  obtained_generic_strat <- sort(unique(c_missing$Strategy))
  expect_equal(expected_generic_strat, obtained_generic_strat)
})


# test methods

## summary
test_that("message is correct in summary.ceac", {
  c <- ceac(wtp, psa_obj)
  sum_df <- summary(c)
  expect_equal(sum_df$cost_eff_strat, c("Radio", "Chemo"))
})

## plot
test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, psa_obj)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_true(inherits(gf, "ggplot"))
  gnof <- plot(ceac_obj, frontier = FALSE)
  expect_true(inherits(gnof, "ggplot"))
})
