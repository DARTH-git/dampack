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
  expect_silent(plot(big_icer, xbreaks = seq(0, 1, by = 0.1), ylim = c(-500, 20000)))
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

# import psa
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

# function runs w/o error (uncertainty = FALSE)
test_that("calculate_icers_psa runs (no uncertainty)", {
  expect_silent(calculate_icers_psa(psa_big))
})

# function runs w/o error (uncertainty = TRUE)
test_that("calculate_icers_psa runs (w/ uncertainty)", {
  expect_silent(calculate_icers_psa(psa_big, uncertainty = TRUE))
})

# mean costs and effect extracted as base case
test_that("psa icers returns icers df corresponding to means", {
  psa_icers <- calculate_icers_psa(psa_big)
  psa_sum <- summary(psa_big)
  exp_icers <- calculate_icers(cost = psa_sum$meanCost,
                               effect = psa_sum$meanEffect,
                               strategies = psa_sum$Strategy)
  expect_equal(psa_icers, exp_icers)
})

# 95% quantiles are correct
test_that("quantiles returned are correct", {
  psa_icers_unc <- calculate_icers_psa(psa_big, uncertainty = TRUE)
  quantiles <- as.data.frame(psa_icers_unc[, c("Lower_95_Cost", "Upper_95_Cost",
                                               "Lower_95_Effect", "Upper_95_Effect")])
  quantiles_exp <- data.frame(Lower_95_Cost = c(quantile(psa_big$cost$Radio, 0.025),
                                                quantile(psa_big$cost$Chemo, 0.025),
                                                quantile(psa_big$cost$Surg, 0.025)),
                              Upper_95_Cost = c(quantile(psa_big$cost$Radio, 0.975),
                                                quantile(psa_big$cost$Chemo, 0.975),
                                                quantile(psa_big$cost$Surg, 0.975)),
                              Lower_95_Effect = c(quantile(psa_big$effectiveness$Radio, 0.025),
                                                  quantile(psa_big$effectiveness$Chemo, 0.025),
                                                  quantile(psa_big$effectiveness$Surg, 0.025)),
                              Upper_95_Effect = c(quantile(psa_big$effectiveness$Radio, 0.975),
                                                  quantile(psa_big$effectiveness$Chemo, 0.975),
                                                  quantile(psa_big$effectiveness$Surg, 0.975)))
  expect_equal(quantiles, quantiles_exp)
})

test_that("plot.icers runs for PSA icers", {
  psa_icers <- calculate_icers_psa(psa_big)
  psa_icers_unc <- calculate_icers_psa(psa_big, uncertainty = TRUE)
  expect_silent({
    plot(psa_icers)
    plot(psa_icers_unc)
  })
})

test_that("plot.icers produces identical results for uncertainty and w/o", {
  psa_icers <- calculate_icers_psa(psa_big)
  psa_icers_unc <- calculate_icers_psa(psa_big, uncertainty = TRUE)
  icers1 <- plot(psa_icers)
  icers2 <- plot(psa_icers_unc)
  expect_equal(icers1, icers2)
})

