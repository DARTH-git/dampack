context('ceac')
library(dampack)

test_that("ceac has all methods we'd expect", {
  current_methods <- as.vector(methods(class = ceac))
  expected_methods <- c("plot.ceac", "summary.ceac")
  expect_equal(current_methods, expected_methods)
})
