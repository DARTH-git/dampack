context("calculate_icers() against a reference strategy (#209) and net benefit (#200)")

# Toy example with one strategy in each position relative to the reference "Ref":
#   strategy  cost  effect   vs Ref: inc cost  inc effect  status
#   Ref        100   1.0             -          -          Reference
#   More       200   2.0           100        1.0          More costly, more effective (ICER 100)
#   Cheaper     50   1.5           -50        0.5          Dominant
#   Worse      300   0.5           200       -0.5          Dominated
#   Less        80   0.8           -20       -0.2          Less costly, less effective (ICER 100)
#   Same       100   1.0             0        0.0          Equivalent
df_toy <- data.frame(strategy = c("Ref", "More", "Cheaper", "Worse", "Less", "Same"),
                     cost = c(100, 200, 50, 300, 80, 100),
                     effect = c(1, 2, 1.5, 0.5, 0.8, 1),
                     stringsAsFactors = FALSE)
toy_icers <- function(...) {
  calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy, ...)
}
row_of <- function(x, s) x[x$Strategy == s, ]

test_that("without ref_str or wtp the output is unchanged", {
  x <- toy_icers()
  expect_identical(names(x), c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "ICER", "Status"))
  expect_null(attr(x, "ref_str"))
})

test_that("reference columns match hand calculations", {
  x <- toy_icers(ref_str = "Ref")
  # sequential (frontier) results are unchanged by ref_str
  x0 <- toy_icers()
  expect_equal(as.data.frame(x)[, names(x0)], as.data.frame(x0))
  expect_identical(attr(x, "ref_str"), "Ref")

  expected <- list(
    Ref     = list(NA_real_, NA_real_, NA_real_, "Reference"),
    More    = list(100, 1, 100, "More costly, more effective"),
    Cheaper = list(-50, 0.5, NA_real_, "Dominant"),
    Worse   = list(200, -0.5, NA_real_, "Dominated"),
    Less    = list(-20, -0.2, 100, "Less costly, less effective"),
    Same    = list(0, 0, NA_real_, "Equivalent")
  )
  for (s in names(expected)) {
    r <- row_of(x, s)
    expect_equal(r$Inc_Cost_ref, expected[[s]][[1]], info = s)
    expect_equal(r$Inc_Effect_ref, expected[[s]][[2]], info = s)
    expect_equal(r$ICER_ref, expected[[s]][[3]], info = s)
    expect_equal(r$Status_ref, expected[[s]][[4]], info = s)
  }
  expect_type(x$ICER_ref, "double")
})

test_that("a dominated or the most expensive strategy can be the reference", {
  x <- toy_icers(ref_str = "Worse")
  expect_equal(row_of(x, "Worse")$Status_ref, "Reference")
  expect_equal(row_of(x, "Cheaper")$Status_ref, "Dominant")
  expect_equal(row_of(x, "Cheaper")$Inc_Cost_ref, 50 - 300)
  # the frontier itself does not depend on which strategy is the reference
  expect_equal(x$Status, toy_icers()$Status)

  y <- toy_icers(ref_str = "Worse")  # Worse is also the most expensive strategy
  expect_true(all(y$Inc_Cost_ref[y$Strategy != "Worse"] < 0))
})

test_that("NMB is added by default with wtp, NHB only on request", {
  x <- toy_icers(wtp = 150)
  expect_equal(x$NMB, 150 * x$Effect - x$Cost)
  expect_false(any(c("NHB", "iNMB_ref", "iNHB_ref") %in% names(x)))
  expect_identical(attr(x, "wtp"), 150)

  y <- toy_icers(ref_str = "Ref", wtp = 150)
  expect_equal(row_of(y, "More")$iNMB_ref, (150 * 2 - 200) - (150 * 1 - 100))
  expect_true(is.na(row_of(y, "Ref")$iNMB_ref))
  expect_false("NHB" %in% names(y))

  z <- toy_icers(ref_str = "Ref", wtp = 150, nhb = TRUE)
  expect_equal(z$NHB, z$Effect - z$Cost / 150)
  expect_equal(row_of(z, "Cheaper")$iNHB_ref, (1.5 - 50 / 150) - (1 - 100 / 150))
})

test_that("hund_strat: reference columns equal the difference from the reference", {
  data("hund_strat")
  ref <- hund_strat$Strategy[3]
  x <- calculate_icers(hund_strat$Cost, hund_strat$QALYs, hund_strat$Strategy, ref_str = ref)
  r0 <- row_of(x, ref)
  others <- x$Strategy != ref
  expect_equal(x$Inc_Cost_ref[others], x$Cost[others] - r0$Cost)
  expect_equal(x$Inc_Effect_ref[others], x$Effect[others] - r0$Effect)
})

test_that("edge cases: one strategy, two strategies, names with spaces", {
  one <- calculate_icers(10, 1, "Standard of care", ref_str = "Standard of care", wtp = 100)
  expect_equal(one$Status_ref, "Reference")
  expect_equal(one$NMB, 90)

  two <- calculate_icers(c(10, 30), c(1, 2), c("Standard of care", "New drug (A)"),
                         ref_str = "Standard of care")
  expect_equal(row_of(two, "New drug (A)")$ICER_ref, 20)
  expect_equal(row_of(two, "New drug (A)")$Status_ref, "More costly, more effective")
})

test_that("invalid ref_str, wtp, and nhb give informative errors", {
  expect_error(toy_icers(ref_str = "Nope"), "must be one of the strategies")
  expect_error(toy_icers(ref_str = c("Ref", "More")), "single strategy name")
  expect_error(toy_icers(wtp = c(100, 200)), "single non-negative number")
  expect_error(toy_icers(wtp = -1), "single non-negative number")
  expect_error(toy_icers(nhb = TRUE), "requires `wtp`")
})

test_that("print.icers shows the sequential table by default and the reference views on request", {
  x <- toy_icers(ref_str = "Ref", wtp = 150, currency = "€")
  out_seq <- capture.output(print(x))
  expect_true(any(grepl("ICER (€/QALY)", out_seq, fixed = TRUE)))
  expect_false(any(grepl("vs Ref", out_seq, fixed = TRUE)))

  out_ref <- capture.output(print(x, ref = "reference"))
  expect_true(any(grepl("vs Ref", out_ref, fixed = TRUE)))
  expect_false(any(grepl("Incremental Costs (€)", out_ref, fixed = TRUE)))

  out_both <- capture.output(print(x, ref = "both"))
  expect_true(any(grepl("vs Ref", out_both, fixed = TRUE)))
  expect_true(any(grepl("Incremental Costs (€)", out_both, fixed = TRUE)))

  expect_error(print(toy_icers(), ref = "reference"), "no reference strategy")
})
