# calculate_cers_ref() and calculate_cers_ref_psa(): every strategy against a reference (#209)

# Toy example with one strategy in each position relative to the reference "Ref":
#   strategy  cost  effect   vs Ref: inc cost  inc effect  status
#   Ref        100   1.0             -          -          Reference
#   More       200   2.0           100        1.0          More costly, more effective (CER 100)
#   Cheaper     50   1.5           -50        0.5          Dominant
#   Worse      300   0.5           200       -0.5          Dominated
#   Less        80   0.8           -20       -0.2          Less costly, less effective (CER 100)
#   Same       100   1.0             0        0.0          Equivalent
df_toy <- data.frame(strategy = c("Ref", "More", "Cheaper", "Worse", "Less", "Same"),
                     cost = c(100, 200, 50, 300, 80, 100),
                     effect = c(1, 2, 1.5, 0.5, 0.8, 1),
                     stringsAsFactors = FALSE)
toy_cers <- function(ref_str = "Ref", ...) {
  calculate_cers_ref(df_toy$cost, df_toy$effect, df_toy$strategy, ref_str = ref_str, ...)
}
row_of <- function(x, s) x[x$Strategy == s, ]

test_that("comparisons with the reference match hand calculations", {
  x <- toy_cers()
  expect_s3_class(x, "cers_ref")
  expect_identical(names(x), c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "CER", "Status"))
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
    expect_equal(r$Inc_Cost, expected[[s]][[1]], info = s)
    expect_equal(r$Inc_Effect, expected[[s]][[2]], info = s)
    expect_equal(r$CER, expected[[s]][[3]], info = s)
    expect_equal(r$Status, expected[[s]][[4]], info = s)
  }
  expect_type(x$CER, "double")
})

test_that("the reference comes first, then the other strategies by cost", {
  x <- toy_cers(ref_str = "More")
  expect_equal(x$Strategy, c("More", "Cheaper", "Less", "Ref", "Same", "Worse"))

  data(hund_strat)
  y <- calculate_cers_ref(hund_strat$Cost, hund_strat$QALYs, hund_strat$Strategy, ref_str = "1")
  v_rest <- hund_strat[hund_strat$Strategy != 1, ]
  expect_equal(y$Strategy, c("1", as.character(v_rest$Strategy[order(v_rest$Cost)])))
  r0 <- row_of(y, "1")
  others <- y$Strategy != "1"
  expect_equal(y$Inc_Cost[others], y$Cost[others] - r0$Cost)
  expect_equal(y$Inc_Effect[others], y$Effect[others] - r0$Effect)
})

test_that("a dominated or the most expensive strategy can be the reference", {
  x <- toy_cers(ref_str = "Worse")  # Worse is dominated and the most expensive strategy
  expect_equal(row_of(x, "Worse")$Status, "Reference")
  expect_equal(row_of(x, "Cheaper")$Status, "Dominant")
  expect_equal(row_of(x, "Cheaper")$Inc_Cost, 50 - 300)
  expect_true(all(x$Inc_Cost[x$Strategy != "Worse"] < 0))
})

test_that("wtp adds NMB and its difference from the reference; nhb adds NHB", {
  x <- toy_cers(wtp = 150)
  expect_equal(x$NMB, 150 * x$Effect - x$Cost)
  expect_equal(row_of(x, "More")$Inc_NMB, (150 * 2 - 200) - (150 * 1 - 100))
  expect_true(is.na(row_of(x, "Ref")$Inc_NMB))
  expect_false(any(c("NHB", "Inc_NHB") %in% names(x)))
  expect_identical(attr(x, "wtp"), 150)

  z <- toy_cers(wtp = 150, nhb = TRUE)
  expect_equal(z$NHB, z$Effect - z$Cost / 150)
  expect_equal(row_of(z, "Cheaper")$Inc_NHB, (1.5 - 50 / 150) - (1 - 100 / 150))
})

test_that("edge cases: one strategy, two strategies, names with spaces", {
  one <- calculate_cers_ref(10, 1, "Standard of care", ref_str = "Standard of care", wtp = 100)
  expect_equal(one$Status, "Reference")
  expect_equal(one$NMB, 90)

  two <- calculate_cers_ref(c(10, 30), c(1, 2), c("Standard of care", "New drug (A)"),
                            ref_str = "Standard of care")
  expect_equal(row_of(two, "New drug (A)")$CER, 20)
  expect_equal(row_of(two, "New drug (A)")$Status, "More costly, more effective")
})

test_that("invalid inputs give informative errors", {
  expect_error(calculate_cers_ref(df_toy$cost, df_toy$effect, df_toy$strategy), "must name the reference")
  expect_error(toy_cers(ref_str = "Nope"), "must be one of the strategies")
  expect_error(toy_cers(ref_str = c("Ref", "More")), "single strategy name")
  expect_error(toy_cers(wtp = c(100, 200)), "single non-negative number")
  expect_error(toy_cers(wtp = -1), "single non-negative number")
  expect_error(toy_cers(nhb = TRUE), "requires `wtp`")
  expect_error(calculate_cers_ref(1:2, 1:3, c("a", "b"), ref_str = "a"), "same length")
  expect_error(calculate_cers_ref(1:2, 1:2, c("a", "a"), ref_str = "a"), "unique")
})

test_that("print() names the reference and formats the table", {
  x <- toy_cers(wtp = 150, currency = "€")
  out <- capture.output(tbl <- print(x))
  expect_true(any(grepl("not a fully incremental analysis", out, fixed = TRUE)))
  expect_true("CER vs Ref (€/QALY)" %in% names(tbl))
  expect_true("Incremental NMB vs Ref (€)" %in% names(tbl))
  expect_equal(tbl[["CER vs Ref (€/QALY)"]][1], "-")
})

# calculate_icers() stays fully incremental: no reference strategy
test_that("calculate_icers() has no reference strategy", {
  expect_false("ref_str" %in% names(formals(calculate_icers)))
  expect_false("ref_str" %in% names(formals(calculate_icers_psa)))
  expect_false("ref_str" %in% names(formals(summary.psa)))
  x <- calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy)
  expect_identical(names(x), c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "ICER", "Status"))
  y <- calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy, wtp = 150, nhb = TRUE)
  expect_equal(y$NMB, 150 * y$Effect - y$Cost)
  expect_equal(y$NHB, y$Effect - y$Cost / 150)
  expect_identical(attr(y, "wtp"), 150)
  expect_error(calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy, nhb = TRUE),
               "requires `wtp`")
})

# PSA ----------------------------------------------------------------------------------------

# Four PSA samples; the difference "New" - "Ref" falls in a different quadrant in each:
#   sample  inc effect  inc cost  quadrant
#   1          +0.2       +100    NE (more effective, more costly)
#   2          +0.2        -50    SE (more effective, less costly)
#   3          -0.1        -50    SW (less effective, less costly)
#   4          -0.1       +100    NW (less effective, more costly)
df_cost <- data.frame(Ref = c(1000, 1000, 1000, 1000), New = c(1100, 950, 950, 1100))
df_eff <- data.frame(Ref = c(1, 1, 1, 1), New = c(1.2, 1.2, 0.9, 0.9))
psa_q <- make_psa_obj(df_cost, df_eff, strategies = c("Ref", "New"), currency = "€")

test_that("calculate_cers_ref_psa() uses the means of the PSA and keeps its currency", {
  s <- summary(psa_q)
  expected <- calculate_cers_ref(s$meanCost, s$meanEffect, s$Strategy, ref_str = "Ref",
                                 wtp = 1000, currency = "€")
  expect_equal(calculate_cers_ref_psa(psa_q, ref_str = "Ref", wtp = 1000), expected)
  expect_identical(attr(expected, "currency"), "€")
})

test_that("uncertainty = TRUE gives quadrant shares and the share of cost-effective samples", {
  x <- calculate_cers_ref_psa(psa_q, ref_str = "Ref", wtp = 1000, uncertainty = TRUE)
  r_new <- row_of(x, "New")
  expect_equal(unlist(r_new[, c("Prop_NE", "Prop_SE", "Prop_SW", "Prop_NW")], use.names = FALSE),
               rep(0.25, 4))
  inmb <- 1000 * (df_eff$New - df_eff$Ref) - (df_cost$New - df_cost$Ref)  # 100, 250, -50, -200
  expect_equal(r_new$Prop_CE, mean(inmb > 0))
  expect_equal(r_new$Lower_95_Inc_NMB, unname(quantile(inmb, 0.025)))
  r_ref <- row_of(x, "Ref")
  expect_true(all(is.na(r_ref[, c("Lower_95_Inc_Cost", "Prop_NE", "Prop_CE")])))
  # each interval follows its point estimate
  v_names <- names(x)
  expect_equal(match("Lower_95_Inc_Cost", v_names), match("Inc_Cost", v_names) + 1)
  expect_identical(attr(x, "ref_str"), "Ref")
  expect_s3_class(x, "cers_ref")
})

test_that("uncertainty = TRUE gives intervals of the sample-by-sample differences", {
  data("cancer_psa_data")
  psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                          cancer_psa_data$parameters, cancer_psa_data$strategies)
  x <- calculate_cers_ref_psa(psa_big, ref_str = "Surgery", wtp = 50000, uncertainty = TRUE)
  i_chemo <- which(cancer_psa_data$strategies == "Chemo")
  i_surg <- which(cancer_psa_data$strategies == "Surgery")
  r <- row_of(x, "Chemo")
  d_cost <- cancer_psa_data$cost[[i_chemo]] - cancer_psa_data$cost[[i_surg]]
  d_eff <- cancer_psa_data$effectiveness[[i_chemo]] - cancer_psa_data$effectiveness[[i_surg]]
  expect_equal(r$Lower_95_Inc_Cost, unname(quantile(d_cost, 0.025)))
  expect_equal(r$Upper_95_Inc_Cost, unname(quantile(d_cost, 0.975)))
  expect_equal(r$Lower_95_Inc_Effect, unname(quantile(d_eff, 0.025)))
  expect_equal(r$Upper_95_Inc_NMB, unname(quantile(50000 * d_eff - d_cost, 0.975)))
  out <- capture.output(tbl <- print(x))
  expect_match(tbl$Prop_NE[2], "^0\\.[0-9]{3}$")
})
