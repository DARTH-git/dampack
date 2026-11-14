library(dampack)


# test the class methods
test_that("exp_loss has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "exp_loss"))
  expected_methods <- c("plot.exp_loss")
  expect_equal(current_methods, expected_methods)
})

# test return object
## setup
data("cancer_psa_data")
v_wtp <- seq(1000, 141000, by = 10000)
psa_obj <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
elc_obj <- calc_exp_loss(psa = psa_obj,
                         wtp = v_wtp)

# classes
test_that("return object has classes evpi and data.frame", {
  expect_s3_class(elc_obj, "exp_loss")
  expect_s3_class(elc_obj, "data.frame")
})


# structure
test_that("return object has correct column names", {
  expect_true(all(c("WTP", "Strategy", "Expected_Loss", "On_Frontier") %in% names(elc_obj)))
})


# calculation
test_that("expected loss is what we'd expect", {
  nmb <-  v_wtp[1] * cancer_psa_data$effectiveness - cancer_psa_data$cost
  max_str <- max.col(nmb, ties.method = "first")
  loss <- nmb[cbind(1:(psa_obj$n_sim), max_str)] - nmb
  exp_loss <- colMeans(loss)
  obs_loss <- unlist(c(elc_obj[1:3, 3]))
  expect_equal(exp_loss, obs_loss, ignore_attr = TRUE)
})

# methods

## plot
test_that("plot.exp_loss returns ggplot class", {
  p <- plot(elc_obj, title = "Title",
            txtsize = 16, effect_units = "QALY", currency = "Dollars ($%$%)")
  expect_s3_class(p, "ggplot")
})

test_that("plot.exp_loss runs with several combinations of arguments", {
  expect_silent(plot(elc_obj, frontier = TRUE, log_y = TRUE))
  expect_silent(plot(elc_obj, frontier = FALSE, log_y = TRUE))
  expect_silent(plot(elc_obj, frontier = TRUE, log_y = FALSE))
  expect_silent(plot(elc_obj, frontier = FALSE, log_y = FALSE))
  expect_silent(plot(elc_obj, col = "bw"))
  expect_silent(plot(elc_obj, ylim = c(1e3, 1e5), n_y_ticks = 10,
                     log_y = FALSE, frontier = TRUE, col = "full", points = FALSE))
})

# black-and-white expected loss curves ignored lsize and warned about an unused color scale (#210)
test_that("plot.exp_loss(col = 'bw') uses linewidth and builds without warnings", {
  el <- calc_exp_loss(psa_obj, wtp = seq(1e4, 1e5, 1e4))
  g <- plot(el, col = "bw", linewidth = 3)
  expect_warning(b <- ggplot2::ggplot_build(g), NA)
  i_line <- which(vapply(g$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  expect_equal(unique(b$data[[i_line]]$linewidth), 3)
  expect_equal(length(unique(b$data[[i_line]]$linetype)), length(unique(el$Strategy)))
})
