# Harmonized arguments across the analysis and plot functions (#214). Old call forms keep
# working, with a soft deprecation warning.

data("cancer_psa_data")
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
v_wtp <- seq(1e4, 1e5, by = 3e4)

test_that("ceac() takes psa first; the old (wtp, psa) order still works, deprecated", {
  df_new <- ceac(psa_big, v_wtp)
  expect_s3_class(df_new, "ceac")
  # named arguments work in either order, without a warning
  expect_no_warning(df_named <- ceac(wtp = v_wtp, psa = psa_big))
  expect_identical(df_named, df_new)
  # the old positional order gives the same result and one deprecation warning
  lifecycle::expect_deprecated(df_old <- ceac(v_wtp, psa_big), "must come after `psa`")
  expect_identical(df_old, df_new)
  # a non-psa first argument still gets the usual error
  expect_error(ceac(v_wtp, v_wtp), "class `psa`")
})

test_that("plot.owsa() and plot.exp_loss() take linewidth; size and lsize are deprecated", {
  o <- owsa(psa_big, outcome = "eff")
  line_width <- function(p) {
    i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))[1]
    unique(ggplot2::layer_data(p, i)$linewidth)
  }
  expect_equal(line_width(plot(o, linewidth = 2)), 2)
  lifecycle::expect_deprecated(p_old <- plot(o, size = 2), "linewidth")
  expect_equal(line_width(p_old), 2)

  el <- calc_exp_loss(psa_big, v_wtp)
  expect_equal(line_width(plot(el, linewidth = 2)), 2)
  lifecycle::expect_deprecated(p_old <- plot(el, lsize = 2), "linewidth")
  expect_equal(line_width(p_old), 2)
})

test_that("plot.owsa(), plot.twsa() and owsa_opt_strat() accept axis limits", {
  scale_limits <- function(p, aes) p$scales$get_scales(aes)$limits
  o <- owsa(psa_big, outcome = "eff")
  p <- plot(o, xlim = c(0, 1), ylim = c(5, 15))
  expect_equal(scale_limits(p, "x"), c(0, 1))
  expect_equal(scale_limits(p, "y"), c(5, 15))
  expect_null(scale_limits(plot(o), "x"))

  tw <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  p <- plot(tw, xlim = c(0.3, 0.6), ylim = c(0.05, 0.2))
  expect_equal(scale_limits(p, "x"), c(0.3, 0.6))
  expect_equal(scale_limits(p, "y"), c(0.05, 0.2))

  o_nmb <- owsa(psa_big, outcome = "nmb", wtp = 1e5)
  p <- owsa_opt_strat(o_nmb, xlim = c(0, 1))
  expect_equal(scale_limits(p, "x"), c(0, 1))
})

test_that("effect_units defaults to QALYs everywhere, singular in per-unit labels", {
  # the default labels are unchanged: "$/QALY" on willingness-to-pay axes
  expect_match(plot(ceac(psa_big, v_wtp))$labels$x, "Thousand $ / QALY)", fixed = TRUE)
  expect_match(plot(calc_exp_loss(psa_big, v_wtp))$labels$x, "Thousand $/QALY)", fixed = TRUE)
  expect_match(plot(calc_evpi(psa_big, v_wtp))$labels$x, "Thousand $/QALY)", fixed = TRUE)
  # plural units are made singular there; units already in the singular are kept
  expect_match(plot(calc_evpi(psa_big, v_wtp), effect_units = "life-years")$labels$x,
               "$/life-year)", fixed = TRUE)
  expect_match(plot(calc_evpi(psa_big, v_wtp), effect_units = "QALY")$labels$x,
               "$/QALY)", fixed = TRUE)
  expect_equal(per_unit(c("QALYs", "QALY", "life-years", "DALYs averted")),
               c("QALY", "QALY", "life-year", "DALYs averted"))
})
