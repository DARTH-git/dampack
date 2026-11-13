context("owsa")
library(dampack)

# test the class
test_that("owsa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "owsa"))
  expected_methods <- c("plot.owsa")
  expect_equal(current_methods, expected_methods)
})

# class creation

## setup
data("cancer_psa_data")
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)

test_that("metamod object has correct classes", {
  o <- owsa(psa_big, outcome = "eff", wtp = 100000)
  expect_is(o, "owsa")
  expect_is(o, "data.frame")
})

# methods

test_that("plot.owsa returns a ggplot object", {
  o <- owsa(psa_big, outcome = "nhb", wtp = 100000)
  g <- plot(o)
  expect_is(g, "ggplot")
})

# test basecase argument for plot.owsa
# black-and-white plots must distinguish strategies by line type (#176)
test_that("plot.owsa(col = 'bw') gives each strategy its own line type", {
  o <- owsa(psa_big, params = c("pFailChemo", "cSurg"), outcome = "nhb", wtp = 100000)
  g <- plot(o, col = "bw")
  expect_warning(ggplot2::ggplot_build(g), NA)
  d <- ggplot2::layer_data(g, 1)
  n_strat <- length(unique(o$strategy))
  expect_equal(length(unique(d$linetype)), n_strat)
  expect_equal(length(unique(d$group)), n_strat)
  # one line type per strategy within each parameter panel
  expect_true(all(tapply(d$linetype, d$PANEL, function(v) length(unique(v))) == n_strat))
  expect_setequal(as.character(ggplot2::get_guide_data(g, "linetype")$.label), unique(o$strategy))
})

# colored plots are unchanged
test_that("plot.owsa(col = 'full') still maps strategies to color", {
  o <- owsa(psa_big, params = "pFailChemo", outcome = "nhb", wtp = 100000)
  d <- ggplot2::layer_data(plot(o, col = "full"), 1)
  expect_equal(length(unique(d$colour)), length(unique(o$strategy)))
})

test_that("plot.owsa basecase argument", {
  o <- owsa(psa_big, outcome = "nhb", wtp = 100000)
  # all params except 1
  p <- plot(o,
            basecase = list(cRadio = 10000, cSurg = 30000, muDieCancer = 0.2,
                            pDieSurg = .1, pFailChemo = .45, pFailRadio = .5,
                            pFailSurg = .050))
  expect_is(p, "ggplot")

  # test error when wrong parameter is supplied
  expect_error(plot(o,
                    basecase = list(cRadio = 10000, non_existent = .050)),
               "Some parameter names in the basecase argument of plot.owsa are not present in owsa object.")
})

# test owsa tornado
test_that("owsa_tornado", {
  o <- owsa(psa_big, outcome = "nmb", wtp = 100000)

  # returning plot
  p <- owsa_tornado(o)
  expect_is(p, "ggplot")

  # some parameters
  p2 <- owsa_tornado(o, txtsize = 15,
                     min_rel_diff = 0.05,
                     col = "bw", ylim = c(7e5, 2e6))
  expect_is(p2, "ggplot")

  # returning data
  d <- owsa_tornado(o, return = "data")

  ## expect that muDieCancer has the highest relative difference
  max_param <- d$parameter[which.max(d$abs_diff)]
  expect_equal(max_param, "muDieCancer")

})

test_that("owsa_opt_strat", {
  o <- owsa(psa_big, outcome = "nhb", wtp = 100000)

  # returning plot
  p <- owsa_opt_strat(o)
  expect_is(p, "ggplot")

  # returning data
  d <- owsa_opt_strat(o, return = "data")
  expect_is(d, "data.frame")

  # pFailChemo should have 1 switch between .4 and .5
  pfc <- d[d$parameter == "pFailChemo", ]
  n_switch <- nrow(pfc) - 1
  expect_equal(n_switch, 1)

  switch_min <- pfc$pmin[2]
  expect_true(switch_min > 0.4 & switch_min < 0.5)
})

# owsa_opt_strat() plot: one bar per run of the same optimal strategy, meeting halfway
# between grid points, and separate bars when a strategy is optimal in two ranges (#67)
test_that("owsa_opt_strat() bars meet at the switch points", {
  df <- expand.grid(param_val = 1:6, strategy = c("A", "B"), stringsAsFactors = FALSE)
  df$parameter <- "p"
  # A is best at 1-2 and 5-6, B at 3-4
  df$outcome_val <- ifelse(df$strategy == "A", c(10, 10, 1, 1, 10, 10)[df$param_val], 5)
  o <- structure(df[, c("parameter", "strategy", "param_val", "outcome_val")],
                 class = c("owsa", "data.frame"))
  g <- owsa_opt_strat(o)
  d <- ggplot2::layer_data(g, 1)
  d <- d[order(d$xmin), ]
  expect_equal(nrow(d), 3)
  expect_equal(d$xmin, c(1, 2.5, 4.5))
  expect_equal(d$xmax, c(2.5, 4.5, 6))
  expect_equal(length(unique(d$fill)), 2)
  expect_equal(d$fill[1], d$fill[3])
})

test_that("owsa_opt_strat() returns the same data as before", {
  o <- owsa(psa_big, params = c("pFailChemo", "cSurg"), outcome = "nmb", wtp = 100000)
  d <- owsa_opt_strat(o, return = "data")
  expect_true(all(c("parameter", "strategy", "pmin", "pmax") %in% names(d)))
  # pmin and pmax are observed parameter values
  expect_true(all(d$pmin %in% o$param_val & d$pmax %in% o$param_val))
})

test_that("owsa_opt_strat(plot_const = FALSE) leaves out parameters with one optimal strategy", {
  df <- expand.grid(param_val = 1:4, strategy = c("A", "B"), parameter = c("switches", "constant"),
                    stringsAsFactors = FALSE)
  df$outcome_val <- ifelse(df$parameter == "constant", ifelse(df$strategy == "A", 2, 1),
                           ifelse(df$strategy == "A", c(2, 2, 0, 0)[df$param_val], 1))
  o <- structure(df[, c("parameter", "strategy", "param_val", "outcome_val")],
                 class = c("owsa", "data.frame"))
  d_all <- ggplot2::layer_data(owsa_opt_strat(o, plot_const = TRUE), 1)
  d_var <- ggplot2::layer_data(owsa_opt_strat(o, plot_const = FALSE), 1)
  expect_equal(length(unique(d_all$PANEL)), 2)
  expect_equal(length(unique(d_var$PANEL)), 1)
  expect_equal(nrow(d_var), 2)
})
