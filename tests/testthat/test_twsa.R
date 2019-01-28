context("twsa")
library(dampack)

# test the class
test_that("twsa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "twsa"))
  expected_methods <- c("plot.twsa")
  expect_equal(current_methods, expected_methods)
})

# class creation

## setup
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

test_that("twsa object has correct classes", {
  t <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  expect_is(t, "twsa")
  expect_is(t, "data.frame")
})

test_that("parameters we expect to vary are varying", {
  p1 <- "pFailChemo"
  p2 <- "muDieCancer"
  t <- twsa(psa_big, p1, p2, outcome = "eff")
  expect_true(length(unique(range(t[, p1]))) == 2)
  expect_true(length(unique(range(t[, p2]))) == 2)
})

# methods

test_that("plot.twsa returns a ggplot object", {
  t <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  g <- plot(t)
  expect_is(g, "ggplot")
})
