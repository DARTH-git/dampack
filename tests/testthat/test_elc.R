context("elc")
library(dampack)


# test the class methods
test_that("elc has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "elc"))
  expected_methods <- c("plot.elc")
  expect_equal(current_methods, expected_methods)
})

# test return object
## setup
data("example_psa")
psa_obj <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)
elc_obj <- calc_elc(wtp = example_psa$wtp,
                      psa = psa_obj)

# classes
test_that("return object has classes evpi and data.frame", {
  expect_is(elc_obj, "elc")
  expect_is(elc_obj, "data.frame")
})


# structure
test_that("return object has column names WTP and 'Frontier & EVPI'", {
  expect_true(all(c("WTP", "Frontier_EVPI") %in% names(elc_obj)))
})


# calculation
test_that("expected loss is what we'd expect", {
  nmb <-  example_psa$wtp[1] * example_psa$effectiveness - example_psa$cost
  max_str <- max.col(nmb)
  loss <- nmb[cbind(1:(psa_obj$n_sim), max_str)] - nmb
  exp_loss <- colMeans(loss)
  obs_loss <- unlist(c(elc_obj[1, 2:4]))
  expect_equal(exp_loss, obs_loss, check.names = FALSE)
})

# methods

## plot
test_that("plot.elc returns ggplot class", {
  p <- plot(elc_obj, title = "Title",
            txtsize = 16, effect_units = "QALY", currency = "Dollars ($%$%)")
  expect_is(p, "ggplot")
})

test_that("plot.elc runs with several combinations of arguments", {
  expect_silent(plot(elc_obj, frontier = TRUE, log_y = TRUE))
  expect_silent(plot(elc_obj, frontier = FALSE, log_y = TRUE))
  expect_silent(plot(elc_obj, frontier = TRUE, log_y = FALSE))
  expect_silent(plot(elc_obj, frontier = FALSE, log_y = FALSE))
  expect_silent(plot(elc_obj, col = "bw"))
  expect_silent(plot(elc_obj, ylim = c(1e3, 1e5), n_y_ticks = 10,
                     log_y = FALSE, frontier = TRUE, col = "full", points = FALSE))
})
