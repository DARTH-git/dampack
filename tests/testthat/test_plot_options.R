# Plot options that were not exercised by the other tests (#215). Each test checks what the
# option changes in the plot, not only that the plot is created.

cancer_psa_data <- dampack::cancer_psa_data
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
v_wtp <- seq(10000, 150000, by = 20000)

geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], character(1)))

test_that("plot.psa(): center and ellipse layers can be turned off", {
  expect_equal(sum(geoms(plot(psa_big)) == "GeomPath"), 1)
  p <- plot(psa_big, center = FALSE, ellipse = FALSE)
  expect_equal(geoms(p), "GeomPoint")
  expect_s3_class(ggplot2::ggplot_build(plot(psa_big, col = "bw")), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(plot(psa_big, col = "full", xlim = c(0, 20))), "ggplot_built")
})

test_that("plot.psa(ref_str = ): reference axes and caption are optional", {
  p <- plot(psa_big, ref_str = "Chemo")
  expect_true(all(c("GeomHline", "GeomVline") %in% geoms(p)))
  expect_match(p$labels$caption, "Chemo", fixed = TRUE)
  p2 <- plot(psa_big, ref_str = "Chemo", ref_axis = FALSE, ref_caption = FALSE,
             center = FALSE, ellipse = FALSE)
  expect_false(any(c("GeomHline", "GeomVline") %in% geoms(p2)))
  expect_null(p2$labels$caption)
  expect_setequal(as.character(unique(ggplot2::layer_data(p2, 1)$group)), c("1", "2"))
})

test_that("plot.psa() without center points builds without a fill-scale warning", {
  # both the plain and the incremental CE plane, in every color option
  for (col in c("access", "full", "bw")) {
    expect_no_warning(ggplot2::ggplot_build(plot(psa_big, center = FALSE, ellipse = FALSE, col = col)))
    expect_no_warning(ggplot2::ggplot_build(plot(psa_big, ref_str = "Chemo", center = FALSE,
                                                 ellipse = FALSE, col = col)))
  }
  # with center points, strategies still get matching color and fill
  p <- plot(psa_big)
  v_aes <- unlist(lapply(p$scales$scales, function(s) s$aesthetics))
  expect_true(all(c("colour", "fill") %in% v_aes))
})

test_that("plot.ceac(): frontier, points, and min_prob", {
  df_ceac <- ceac(v_wtp, psa_big)
  expect_true("GeomPoint" %in% geoms(plot(df_ceac)))
  p_plain <- plot(df_ceac, frontier = FALSE, points = FALSE)
  expect_false("GeomPoint" %in% geoms(p_plain))
  # min_prob drops strategies whose probability of being cost-effective never reaches it
  v_max <- tapply(df_ceac$Proportion, df_ceac$Strategy, max)
  cutoff <- sort(v_max)[2] - 1e-9
  expect_output(p_min <- plot(df_ceac, min_prob = cutoff), "filtered out")
  kept <- as.character(unique(ggplot2::ggplot_build(p_min)$plot$data$Strategy))
  expect_false(names(which.min(v_max)) %in% kept)
  for (col in c("full", "bw")) {
    expect_s3_class(ggplot2::ggplot_build(plot(df_ceac, col = col)), "ggplot_built")
  }
})

test_that("plot.evsi(): the layout follows the WTP values and sample sizes", {
  set.seed(215)
  evsi_n <- calc_evsi(psa_big, wtp = 50000, params = "pFailChemo", n = c(10, 100, 1000),
                      n0 = 10, progress = FALSE)
  p_n <- plot(evsi_n)
  expect_equal(rlang::as_label(p_n$mapping$x), "n")

  evsi_wtp <- calc_evsi(psa_big, wtp = v_wtp, params = "pFailChemo", n = 100, n0 = 10,
                        progress = FALSE)
  p_wtp <- plot(evsi_wtp)
  expect_equal(rlang::as_label(p_wtp$mapping$x), "WTP_thou")

  evsi_both <- calc_evsi(psa_big, wtp = c(50000, 100000), params = "pFailChemo",
                         n = c(10, 100), n0 = 10, progress = FALSE)
  p_both <- plot(evsi_both)
  expect_equal(rlang::as_label(p_both$mapping$x), "n")
  expect_equal(rlang::as_label(p_both$mapping$colour), "WTP_thou")
})

test_that("plot.evppi() plots EVPPI against willingness to pay", {
  evppi <- calc_evppi(psa_big, wtp = v_wtp, params = "pFailChemo", progress = FALSE)
  d <- ggplot2::layer_data(plot(evppi), 1)
  expect_equal(nrow(d), length(v_wtp))
  expect_equal(sort(d$x), v_wtp / 1000)
})

test_that("owsa_tornado(): data, relative-difference filter, and strategy comparison", {
  o <- owsa(psa_big, params = c("pFailChemo", "pFailRadio", "cSurg"), outcome = "nmb", wtp = 100000)
  df_t <- owsa_tornado(o, return = "data")
  expect_true(all(c("parameter", "abs_diff", "rel_diff") %in% names(df_t)))
  df_filtered <- owsa_tornado(o, return = "data", min_rel_diff = max(df_t$rel_diff) - 1e-9)
  expect_lt(nrow(df_filtered), nrow(df_t))
  # with two strategies, the tornado shows the difference between them
  p_diff <- owsa_tornado(o, select_str = c("Chemo", "Surgery"), outcome_name = "NMB")
  expect_match(p_diff$labels$y, "NMB", fixed = TRUE)
  expect_error(owsa_tornado(o, select_str = c("Chemo", "Radio", "Surgery")), "max of 2 strategies")
  expect_s3_class(ggplot2::ggplot_build(owsa_tornado(o, col = "bw")), "ggplot_built")
})
