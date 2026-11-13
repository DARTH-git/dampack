context("add_common_aes")

# The accessible palette is chosen from the number of groups in the first layer when no
# strategy names (v_str) are given. That used ggplot_build(p)@data, which exists only in
# ggplot2 >= 4.0 and failed on older versions with "no applicable method for `@`" (#198).
make_plot <- function(n_groups) {
  df <- data.frame(x = seq_len(n_groups), y = seq_len(n_groups), g = factor(letters[seq_len(n_groups)]))
  ggplot(df, aes(x = .data$x, y = .data$y, fill = .data$g, color = .data$g)) + geom_point(shape = 21)
}

test_that("accessible palette without v_str uses Dark2 for up to 8 groups", {
  p <- add_common_aes(make_plot(3), txtsize = 12, col = "access", col_aes = c("fill", "color"))
  d <- ggplot2::layer_data(p, 1)
  expect_setequal(d$fill, RColorBrewer::brewer.pal(3, "Dark2"))
  expect_setequal(d$colour, RColorBrewer::brewer.pal(3, "Dark2"))
})

test_that("accessible palette without v_str uses viridis for more than 8 groups", {
  p <- add_common_aes(make_plot(10), txtsize = 12, col = "access", col_aes = "fill")
  d <- ggplot2::layer_data(p, 1)
  expect_setequal(d$fill, viridis::viridis(10, option = "D"))
})

test_that("plots that use the accessible palette without v_str build", {
  data("cancer_psa_data")
  psa <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                      cancer_psa_data$parameters, cancer_psa_data$strategies)
  o <- owsa(psa, params = c("pFailChemo", "cSurg"), outcome = "nhb", wtp = 100000)
  expect_s3_class(ggplot2::ggplot_build(owsa_tornado(o, col = "access")), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(owsa_opt_strat(o, col = "access")), "ggplot_built")
  tw <- twsa(psa, param1 = "pFailChemo", param2 = "cSurg", outcome = "nhb", wtp = 100000)
  expect_s3_class(ggplot2::ggplot_build(plot(tw, col = "access")), "ggplot_built")
})
