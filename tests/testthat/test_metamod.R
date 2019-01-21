context("metamod")
library(dampack)

# test the class
test_that("psa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "metamodel"))
  expected_methods <- c("predict.metamodel")
  expect_equal(current_methods, expected_methods)
})

# class creation

## setup
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)


test_that("metamod object has correct classes", {
  mm <- metamod(psa_big$effectiveness, psa_big, "cChemo")
  expect_is(mm, "lm")
  expect_is(mm, "metamodel")
})
