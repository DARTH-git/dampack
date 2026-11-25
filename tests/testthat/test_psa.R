library(dampack)

# test the class
test_that("psa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "psa"))
  expected_methods <- c("plot.psa", "summary.psa")
  expect_setequal(current_methods, expected_methods)
})

# class creation

## setup
sm_costs <- data.frame(rbind(c(10e3, 20e3, 100e3),
                             c(20e3, 40e3, 200e3)))
sm_effectiveness <- data.frame(rbind(c(100, 50, 900),
                                     c(56, 89, 700)))
sm_parameters <- data.frame(
  "p1" = c(1, 2),
  "p2" = c(2, 10)
)
sm_strategies <- c("test", "notest", "treat")
names(sm_costs) <- names(sm_effectiveness) <- sm_strategies
psa_small <- make_psa_obj(sm_costs, sm_effectiveness, sm_parameters, sm_strategies)
test_that("psa returns correct object", {
  expect_equal(psa_small$cost, sm_costs)
  expect_equal(psa_small$effectiveness, sm_effectiveness)
  expect_equal(psa_small$strategies, sm_strategies)
  expect_equal(psa_small$n_strategies, 3)
  expect_equal(psa_small$n_sim, 2)
  expect_equal(psa_small$parameters, sm_parameters)
  expect_equal(psa_small$parnames, colnames(sm_parameters))
})

# missing strategies
test_that("psa: missing strategies", {
  psa_small <- make_psa_obj(sm_costs, sm_effectiveness,
                            sm_parameters)
  expect_equal(psa_small$strategies, c("Strategy_1", "Strategy_2", "Strategy_3"))
})

## strategy names with spaces are kept as typed (#208); this used to convert
## "no test" to "no.test" with a warning
sm_str_space <- c("test", "no test", "treat")
test_that("strategy names with spaces are kept", {
  psa_space <- expect_warning(make_psa_obj(sm_costs, sm_effectiveness,
                                           sm_parameters, sm_str_space), NA)
  expect_identical(psa_space$strategies, sm_str_space)
  expect_identical(names(psa_space$cost), sm_str_space)
})

## methods

# plot
data("cancer_psa_data")
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
test_that("plot.psa runs", {
  expect_silent({
    plot(psa_big)
    plot(psa_big, center = FALSE)
    plot(psa_big, ellipse = FALSE)
    plot(psa_big, center = FALSE, ellipse = FALSE)
    plot(psa_big, col = "bw")
    plot(psa_big, col = "bw", xbreaks = seq(0, 15), xlim = c(0, 15),
         ylim = NULL, alpha = 1)
  })
})

test_that("Plot returns ggplot object", {
  p <- plot(psa_big)
  expect_s3_class(p, "ggplot")
})

# print
# use small example from above
test_that("print.psa returns correct output", {
  msg <- capture.output(print(psa_small))
  expected <- c("",
                "PSA object ",
                "------------------------------------------------- ",
                "number of strategies (n_strategies): 3 ",
                "strategies: test, notest, treat  ",
                "number of simulations (n_sim): 2 ",
                "cost: a data frame with 2 rows and 3 columns. ",
                "effectiveness: a data frame with 2 rows and 3 columns. ",
                "parameters: a data frame with 2 rows and 2 columns ",
                "parameter names (parnames):  p1, p2 ",
                "currency: $ ")
  expect_equal(msg, expected)
})

# summary
test_that("summary.psa returns correct output", {
  # no sds
  df_expected <- data.frame("Strategy" = sm_strategies,
                            "meanCost" = colMeans(sm_costs),
                            "meanEffect" = colMeans(sm_effectiveness),
                            stringsAsFactors = FALSE,
                            row.names = NULL)
  df_calc <- summary(psa_small)
  expect_equal(expected = df_expected, df_calc)
})

# other_outcome columns were never renamed to the strategy names (#210)
test_that("make_psa_obj() names other_outcome columns after the strategies", {
  df_other <- data.frame(x_out = c(1, 2), y_out = c(3, 4), z_out = c(5, 6))
  df_cost <- data.frame(a = c(1, 2), b = c(1, 2), c = c(1, 2))
  p <- make_psa_obj(df_cost, df_cost, strategies = c("A", "B", "C"), other_outcome = df_other)
  expect_identical(names(p$other_outcome), c("A", "B", "C"))
})

# log axes (#201): the ellipse is computed on the scale of the axes
test_that("plot.psa(xtrans, ytrans) draws the ellipses on log axes", {
  p <- plot(psa_big, xtrans = "log10", ytrans = "log10")
  expect_no_warning(b <- ggplot2::ggplot_build(p))
  expect_equal(b$layout$panel_scales_x[[1]]$trans$name, "log-10")
  expect_equal(b$layout$panel_scales_y[[1]]$trans$name, "log-10")
  i_path <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1)))
  df_ell <- ggplot2::layer_data(p, i_path)
  # layer data are on the axis scale (log10) and finite: no part of an ellipse falls below 0
  expect_true(all(is.finite(df_ell$x) & is.finite(df_ell$y)))
  # each ellipse is the usual ellipse of the log10 samples
  v_lcost <- log10(psa_big$cost[["Chemo"]])
  v_leff <- log10(psa_big$effectiveness[["Chemo"]])
  m_ell <- ellipse::ellipse(cor(v_leff, v_lcost), scale = c(sd(v_leff), sd(v_lcost)),
                            centre = c(mean(v_leff), mean(v_lcost)))
  df_chemo <- df_ell[df_ell$group == which(psa_big$strategies == "Chemo"), ]
  expect_equal(unname(as.matrix(df_chemo[, c("x", "y")])), unname(m_ell))
})

test_that("plot.psa() with the default identity axes keeps the usual ellipse", {
  p <- plot(psa_big)
  i_path <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1)))
  df_ell <- p$layers[[i_path]]$data
  v_cost <- psa_big$cost[["Chemo"]]
  v_eff <- psa_big$effectiveness[["Chemo"]]
  m_ell <- ellipse::ellipse(cor(v_eff, v_cost), scale = c(sd(v_eff), sd(v_cost)),
                            centre = c(mean(v_eff), mean(v_cost)))
  expect_equal(unname(as.matrix(df_ell[df_ell$group == "Chemo", c("x", "y")])), unname(m_ell))
})

test_that("plot.psa() explains when values cannot be shown on a log axis", {
  expect_error(plot(psa_big, ref_str = "Chemo", ytrans = "log10"),
               "`ytrans = \"log10\"` cannot show every incremental cost value")
  df_cost_zero <- psa_big$cost
  df_cost_zero[1, 1] <- 0
  psa_zero <- make_psa_obj(df_cost_zero, psa_big$effectiveness, psa_big$parameters,
                           psa_big$strategies)
  expect_error(plot(psa_zero, ytrans = "log10"), "cannot show every cost value")
  expect_s3_class(plot(psa_zero, xtrans = "log10"), "ggplot")
})
