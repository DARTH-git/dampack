context("cea")

data("hund_strat")

test_that("calc_cea runs", {
  calculate_icers(cost = hund_strat$Cost,
                  strategies = hund_strat$Strategy,
                  effect = hund_strat$QALYs)
})
