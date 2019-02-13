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
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

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

# test owsa tornado
test_that("owsa_tornado", {
  o <- owsa(psa_big, outcome = "nmb", wtp = 100000)

  # returning plot
  p <- owsa_tornado(o, strategy = "Chemo")
  expect_is(p, "ggplot")

  # some parameters
  p2 <- owsa_tornado(o, strategy = "Chemo", txtsize = 15,
                     min_rel_diff = 0.05,
                     col = "bw", ylim = c(7e5, 2e6))
  expect_is(p2, "ggplot")

  # returning data
  d <- owsa_tornado(o, strategy = "Chemo", return = "data")

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
