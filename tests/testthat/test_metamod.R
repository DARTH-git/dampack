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
  mm <- metamod(psa = psa_big, parm = "pFailChemo", outcome = "cost")
  expect_is(mm, "lm")
  expect_is(mm, "metamodel")
})

# methods

test_that("metamodel with one outcome", {
  # metamodel
  mm <- metamod(psa = psa_big, parm = "pFailChemo", outcome = "cost", strategies = "Chemo")
  expect_is(mm, "lm")
  expect_is(mm, "metamodel")

  # predictions
  pred <- predict(mm)
  expect_equal(colnames(pred), c("pFailChemo", "Chemo"))
})

test_that("prediction with several outcomes", {
  # metamodel
  mm <- metamod(psa = psa_big, parm = "pFailChemo", outcome = "eff")
  expect_is(mm, "lm")
  expect_is(mm, "metamodel")

  # predictions
  pred <- predict(mm)
  expect_equal(colnames(pred), c("pFailChemo", "Chemo", "Radio", "Surgery"))
})
