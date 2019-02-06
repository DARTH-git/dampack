context("metamod")
library(dampack)

# test the class
test_that("metamod has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "metamodel"))
  expected_methods <- c("predict.metamodel", "print.metamodel", "summary.metamodel")
  expect_setequal(current_methods, expected_methods)
})

# class creation

## setup
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

test_that("metamod object has correct classes", {
  mm <- metamod(psa = psa_big, parm = "pFailChemo", outcome = "cost")
  expect_is(mm, "metamodel")
})

# methods

test_that("metamodel with one outcome", {
  # metamodel
  mm <- metamod(psa = psa_big, parms = "pFailChemo", outcome = "cost", strategies = "Chemo")
  expect_is(mm, "metamodel")

  # predictions
  pred <- predict(mm)
  expect_equal(colnames(pred),
               c("parameter", "strategy", "pranges_samp", "outcome_val"))
})

test_that("prediction with several outcomes", {
  # metamodel
  mm <- metamod(psa = psa_big, outcome = "eff")
  expect_is(mm, "metamodel")

  # number of linear models
  expect_equal(length(mm$mods), 8)

  # number of strategies for each
  nstrats <- sapply(mm$mods, length)
  expect_setequal(nstrats, 3)

  # predictions - same colnames
  pred <- predict(mm)
  expect_equal(colnames(pred),
               c("parameter", "strategy", "pranges_samp", "outcome_val"))
})
