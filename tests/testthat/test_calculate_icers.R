context("cea")

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
  expected_df <- data.frame("Strategy" = c("UC", "B", "D", "E", "A", "C"),
                            "Cost" = c(5000, 10000, 35000, 55000, 12000, 25000),
                            "Effect" = c(1, 2, 4, 5, 1.5, 3),
                            "ICER" = c(NA, 5000, 12500, 20000, NA, NA),
                            "Status" = c("ND", "ND", "ND", "ND", "D", "ED"),
                            stringsAsFactors = FALSE)
  class(expected_df) <- c("icers", "data.frame")
  expect_equal(expected_df, icer)
})

## methods
# plot
test_that("plot.psa runs", {
  expect_silent(plot(icer))
})
