context("evpi")
library(dampack)


# test the class methods
test_that("evpi has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "evpi"))
  expected_methods <- c("plot.evpi")
  expect_equal(current_methods, expected_methods)
})

# test return object
## setup
data("example_psa")
psa_obj <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

evpi_obj <- calc_evpi(wtp = example_psa$wtp,
                      psa = psa_obj)

# classes
test_that("return object has classes evpi and data.frame", {
  expect_is(evpi_obj, "evpi")
  expect_is(evpi_obj, "data.frame")
})

# structure
test_that("return object has column names WTP and EVPI", {
  expect_equal(names(evpi_obj), c("WTP", "EVPI"))
})

# calculation
test_that("evpi is what we'd expect", {
  nmb <-  example_psa$wtp[1] * example_psa$effectiveness - example_psa$cost
  d.star <- which.max(colMeans(nmb))
  loss <- nmb - nmb[, d.star]
  evpi <- mean(apply(loss, 1, max))
  expect_equal(evpi, evpi_obj[1, 2])
})

# methods

## plot
test_that("plot.evpi has ggplot class", {
  p <- plot(evpi_obj, title = "Title",
             txtsize = 16, effect_units = "QALY", currency = "Dollars ($%$%)")
  expect_is(p, "ggplot")
})
