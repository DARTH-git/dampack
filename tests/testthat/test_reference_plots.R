# incremental plane and WTP lines in plot.icers(), plot.psa() and plot.cers_ref() (#209, #202)

df_toy <- data.frame(strategy = c("Standard of care", "New drug (A)", "Cheaper", "Worse"),
                     cost = c(100, 200, 50, 300),
                     effect = c(1, 2, 1.5, 0.5),
                     stringsAsFactors = FALSE)
x_toy <- calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy)

point_layer <- function(p) {
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]
  ggplot2::layer_data(p, i)
}
abline_layers <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomAbline"), logical(1)))
}

test_that("plot.icers() is unchanged by default", {
  p <- plot(x_toy)
  expect_length(abline_layers(p), 0)
  expect_equal(p$labels$y, "Cost ($)")
  expect_setequal(point_layer(p)$y, df_toy$cost)
})

test_that("plot.icers(incremental = TRUE) puts the first non-dominated strategy at the origin", {
  # "Cheaper" (cost 50, effect 1.5) is the least costly strategy on the frontier
  p <- plot(x_toy, incremental = TRUE)
  d <- point_layer(p)
  expect_setequal(d$x, df_toy$effect - 1.5)
  expect_setequal(d$y, df_toy$cost - 50)
  expect_equal(p$labels$y, "Incremental Cost ($)")
  expect_equal(p$labels$x, "Incremental Effect (QALYs)")
  expect_match(p$labels$caption, "Origin: Cheaper", fixed = TRUE)
  # the frontier and statuses do not change
  expect_equal(table(d$shape), table(point_layer(plot(x_toy))$shape))
  expect_error(plot(x_toy, incremental = "yes"), "TRUE or FALSE")
  expect_false("ref_str" %in% names(formals(plot.icers)))
})

test_that("plot.icers(wtp = ) draws the WTP line through the least costly frontier strategy", {
  p_abs <- plot(x_toy, wtp = 150)
  i <- abline_layers(p_abs)
  expect_length(i, 1)
  d <- ggplot2::layer_data(p_abs, i)
  expect_equal(d$slope, 150)
  expect_equal(d$intercept, 50 - 150 * 1.5)
  expect_match(p_abs$labels$caption, "WTP = $150/QALY", fixed = TRUE)

  p_inc <- plot(x_toy, incremental = TRUE, wtp = 150)
  expect_equal(ggplot2::layer_data(p_inc, abline_layers(p_inc))$intercept, 0)
})

test_that("plot.icers() uses the currency stored by calculate_icers()", {
  x_eur <- calculate_icers(df_toy$cost, df_toy$effect, df_toy$strategy, currency = "€")
  expect_equal(plot(x_eur)$labels$y, "Cost (€)")
  expect_equal(plot(x_eur, currency = "£")$labels$y, "Cost (£)")
})

# PSA with two strategies, to cover the drop = FALSE bug in plot.psa(ref_str = )
set.seed(20261112)
n_sim <- 200
df_cost2 <- data.frame(A = rnorm(n_sim, 1000, 50), B = rnorm(n_sim, 1500, 50))
df_eff2 <- data.frame(A = rnorm(n_sim, 1, 0.05), B = rnorm(n_sim, 1.2, 0.05))
psa_2 <- make_psa_obj(df_cost2, df_eff2, strategies = c("Standard of care", "New drug (A)"))

test_that("plot.psa(ref_str = ) works with two strategies", {
  expect_warning(p <- plot(psa_2, ref_str = "Standard of care"), NA)
  d <- point_layer(p)
  expect_equal(sort(d$y), sort(df_cost2$B - df_cost2$A))
  expect_equal(sort(d$x), sort(df_eff2$B - df_eff2$A))
})

test_that("plot.psa(wtp = ) draws the WTP line on the incremental plane", {
  p <- plot(psa_2, ref_str = "Standard of care", wtp = 2000)
  i <- abline_layers(p)
  expect_length(i, 1)
  expect_equal(ggplot2::layer_data(p, i)$slope, 2000)
  expect_match(p$labels$caption, "WTP = $2,000/QALY", fixed = TRUE)
  expect_error(plot(psa_2, ref_str = "Nope"), "must be one of the strategies")
  expect_length(abline_layers(plot(psa_2)), 0)
})

test_that("plot.psa(wtp = ) without ref_str draws the line through the least costly strategy", {
  p <- plot(psa_2, wtp = 2000)
  i <- abline_layers(p)
  expect_length(i, 1)
  d <- ggplot2::layer_data(p, i)
  # A ("Standard of care") has the lower mean cost, so the line passes through its means
  expect_equal(d$slope, 2000)
  expect_equal(d$intercept, mean(df_cost2$A) - 2000 * mean(df_eff2$A))
  expect_match(p$labels$caption, "WTP = $2,000/QALY", fixed = TRUE)
})

# plot.cers_ref(): the star around the reference strategy
segment_layers <- function(p) {
  which(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1)))
}

test_that("plot.cers_ref() centers the plane at the reference and draws the star", {
  x_ref <- calculate_cers_ref(df_toy$cost, df_toy$effect, df_toy$strategy,
                              ref_str = "Standard of care", wtp = 150)
  p <- plot(x_ref)
  expect_s3_class(p, "ggplot")
  d <- point_layer(p)
  others <- df_toy$strategy != "Standard of care"
  expect_setequal(d$x, df_toy$effect[others] - 1)
  expect_setequal(d$y, df_toy$cost[others] - 100)
  expect_length(segment_layers(p), 1)
  # the stored threshold draws the WTP line through the origin; NULL removes it
  d_wtp <- ggplot2::layer_data(p, abline_layers(p))
  expect_equal(d_wtp$intercept, 0)
  expect_equal(d_wtp$slope, 150)
  expect_length(abline_layers(plot(x_ref, wtp = NULL)), 0)
  expect_match(p$labels$caption, "Reference Strategy (diamond): Standard of care", fixed = TRUE)
  expect_length(segment_layers(plot(x_ref, lines = FALSE)), 0)
  expect_no_warning(ggplot2::ggplot_build(plot(x_ref, col = "bw", label = "none")))
})

test_that("plot.cers_ref() leaves out the segments for many strategies", {
  data(hund_strat)
  x_hund <- calculate_cers_ref(hund_strat$Cost, hund_strat$QALYs, hund_strat$Strategy, ref_str = "1")
  p <- plot(x_hund, label = "none")
  expect_length(segment_layers(p), 0)
  expect_length(segment_layers(plot(x_hund, label = "none", lines = TRUE)), 1)
  expect_no_warning(ggplot2::ggplot_build(p))
})
