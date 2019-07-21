context("calculate_icers")
library(dampack)

data("hund_strat")

# test the class
test_that("icers has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "icers"))
  expected_methods <- c("plot.icers")
  expect_equal(current_methods, expected_methods)
})

# class creation

# setup
# example from herc.research.va.gov/include/page.asp?id=cost-effectiveness-analysis
cost <- c(55000, 35000, 25000, 10000, 12000, 5000)
effect <- c(5, 4, 3, 2, 1.5, 1)
strat <- c("E", "D", "C", "B", "A", "UC")
icer <- calculate_icers(cost = cost, effect = effect, strategies = strat)

test_that("calculate_icers returns correct object", {
  expected_df <- data.frame("Strategy" = c("UC", "B", "D", "E", "C", "A"),
                            "Cost" = c(5000, 10000, 35000, 55000, 25000, 12000),
                            "Effect" = c(1, 2, 4, 5, 3, 1.5),
                            "Inc_Cost" = c(NA, 5000, 25000, 20000, NA, NA),
                            "Inc_Effect" = c(NA, 1, 2, 1, NA, NA),
                            "ICER" = c(NA, 5000, 12500, 20000, NA, NA),
                            "Status" = c("ND", "ND", "ND", "ND", "ED", "D"),
                            stringsAsFactors = FALSE)
  class(expected_df) <- c("icers", "data.frame")
  expect_equal(expected_df, icer)
})

## methods

big_icer <- calculate_icers(cost = hund_strat$Cost, effect = hund_strat$QALYs, strategies = hund_strat$Strategy)
# plot
test_that("plot.icers runs", {
  expect_silent(plot(big_icer, xbreaks = seq(0, 1, by = 0.1), ylim = c(0, 20000)))
})

# three strategies
test_that("default reference strategy", {
  cea <- calculate_icers(cost = c(163771.0, 164848.7, 163765.1),
                         effect = c(6.48273, 6.52861, 6.52861),
                         strategies = c("No Treat",   "Treat all", "Test & treat"))
  expect_equal(cea$Strategy[1], "Test & treat")
  }
)

# one strategy
test_that("one strategy runs", {
  cea <- calculate_icers(1, 2, "s")
  exp_df <- data.frame("Strategy" = "s",
                       "Cost" = 1,
                       "Effect" = 2,
                       "ICER" = NA,
                       "Inc_Cost" = NA,
                       "Inc_Effect" = NA,
                       stringsAsFactors = FALSE)
  expect_equal(cea, exp_df)
})
