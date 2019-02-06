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
  o <- owsa(psa_big, outcome = "eff")
  expect_is(o, "owsa")
  expect_is(o, "data.frame")
})

# methods

test_that("plot.owsa returns a ggplot object", {
  o <- owsa(psa_big, outcome = "nhb", wtp = 50000)
  g <- plot(o)
  expect_is(g, "ggplot")
})
