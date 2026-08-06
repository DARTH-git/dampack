context("ceac")
library(dampack)

# test the class
test_that("ceac has all methods we'd expect", {
  current_methods <- as.vector(methods(class = ceac))
  expected_methods <- c("plot.ceac", "summary.ceac")
  expect_setequal(current_methods, expected_methods)
})

# test class creation

## setup
data("example_psa")
wtp <- example_psa$wtp
psa_obj <- make_psa_obj(example_psa$cost,
                        example_psa$effectiveness,
                        example_psa$parameters,
                        example_psa$strategies)

test_that("result has class 'ceac'", {
  c <- ceac(wtp, psa_obj)
  expect_true(inherits(c, "data.frame"))
  expect_true(inherits(c, "ceac"))
})

test_that("handles missing strategy", {
  psa_missing <- make_psa_obj(example_psa$cost,
                              example_psa$effectiveness,
                              example_psa$parameters)
  c_missing <- ceac(wtp, psa_missing)
  expected_generic_strat <- factor(c("Strategy_1", "Strategy_2", "Strategy_3"), ordered = TRUE)
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

test_that("summary.ceac is correct when the frontier switches twice", {
  # frontier optimal on A -> B -> C, so two switches and three intervals
  ceac_obj <- data.frame(WTP = c(10000, 30000, 50000, 70000, 90000),
                         Strategy = c("A", "A", "B", "C", "C"),
                         On_Frontier = TRUE,
                         stringsAsFactors = FALSE)
  class(ceac_obj) <- c("ceac", "data.frame")
  sum_df <- summary(ceac_obj)
  expect_equal(nrow(sum_df), 3)
  expect_equal(sum_df$range_min, c(10000, 50000, 70000))
  expect_equal(sum_df$range_max, c(50000, 70000, 90000))
  expect_equal(sum_df$cost_eff_strat, c("A", "B", "C"))
})

## plot
test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, psa_obj)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_is(gf, "ggplot")

  gnof <- plot(ceac_obj, frontier = FALSE)
  expect_is(gnof, "ggplot")

  custom_breaks <- plot(ceac_obj, xbreaks = seq(0, 140))
  expect_is(custom_breaks, "ggplot")

  # black and white
  expect_is(plot(ceac_obj, col = "bw"), "ggplot")
})
