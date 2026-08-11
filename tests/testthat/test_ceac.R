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
  expect_equal(sum_df$optimal_strategy, c("Radio", "Chemo"))
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
  expect_equal(sum_df$optimal_strategy, c("A", "B", "C"))
})

test_that("summary.ceac summarizes a real psa whose frontier switches many times", {
  # df_example_psa_elc switches optimal strategy three times, so the summary must
  # have four intervals. built through the public api - make_psa_obj() then
  # ceac() - so the test also proves the bad shape was reachable from real data,
  # which a hand-built ceac object cannot.
  data("df_example_psa_elc")
  l_psa <- make_psa_obj(cost = df_example_psa_elc[, 1:6],
                        effectiveness = df_example_psa_elc[, 7:12])
  df_ceac <- ceac(seq(1000, 150000, 1000), l_psa)
  df_sum <- summary(df_ceac)

  # expected intervals derived independently, by run-length encoding the frontier
  df_front <- df_ceac[df_ceac$On_Frontier == TRUE, ]
  l_runs <- rle(as.character(df_front$Strategy))
  v_starts <- c(1, head(cumsum(l_runs$lengths), -1) + 1)

  expect_equal(nrow(df_sum), length(l_runs$values))
  expect_equal(df_sum$optimal_strategy, l_runs$values)
  expect_equal(df_sum$range_min, df_front$WTP[v_starts])
  expect_equal(df_sum$range_max, c(df_front$WTP[v_starts[-1]], max(df_front$WTP)))
  expect_false(anyNA(df_sum))
  expect_equal(anyDuplicated(df_sum), 0L)
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
