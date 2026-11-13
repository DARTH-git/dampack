context("reference strategy and WTP lines in plot.icers() and plot.psa() (#209, #202)")

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

test_that("plot.icers(ref_str = ) puts the reference at the origin", {
  p <- plot(x_toy, ref_str = "Standard of care")
  d <- point_layer(p)
  expect_setequal(d$x, df_toy$effect - 1)
  expect_setequal(d$y, df_toy$cost - 100)
  expect_equal(p$labels$y, "Incremental Cost ($)")
  expect_equal(p$labels$x, "Incremental Effect (QALYs)")
  expect_match(p$labels$caption, "Reference Strategy: Standard of care", fixed = TRUE)
  expect_error(plot(x_toy, ref_str = "Nope"), "must be one of the strategies")
})

test_that("plot.icers(wtp = ) draws the WTP line", {
  p_ref <- plot(x_toy, ref_str = "Standard of care", wtp = 150)
  i <- abline_layers(p_ref)
  expect_length(i, 1)
  d <- ggplot2::layer_data(p_ref, i)
  expect_equal(d$slope, 150)
  expect_equal(d$intercept, 0)
  expect_match(p_ref$labels$caption, "WTP = $150/QALY", fixed = TRUE)

  # without a reference, the line goes through the least costly strategy on the frontier
  p_abs <- plot(x_toy, wtp = 150)
  d <- ggplot2::layer_data(p_abs, abline_layers(p_abs))
  expect_equal(d$intercept, 50 - 150 * 1.5)
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
