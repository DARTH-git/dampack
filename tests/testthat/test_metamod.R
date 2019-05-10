context("metamodel")
library(dampack)

# test the class
test_that("metamodel has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "metamodel"))
  expected_methods <- c("predict.metamodel", "print.metamodel", "summary.metamodel")
  expect_setequal(current_methods, expected_methods)
})

# class creation

## setup
data("example_psa")
psa_big <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
                        example_psa$parameters, example_psa$strategies)

test_that("metamodel object has correct classes", {
  mm <- metamodel(psa = psa_big, parm = "pFailChemo", outcome = "cost")
  expect_is(mm, "metamodel")
})

# methods

test_that("metamodel with one outcome", {
  # metamodel
  ## linear
  mm_lin <- metamodel(psa = psa_big, parms = "pFailChemo", outcome = "cost", strategies = "Chemo")
  expect_is(mm_lin, "metamodel")

  # predictions
  pred_lin <- predict(mm_lin)
  expect_equal(colnames(pred_lin),
               c("parameter", "strategy", "param_val", "outcome_val"))

  ## poly
  mm_poly <- metamodel(psa = psa_big, parms = "pFailChemo", outcome = "cost", strategies = "Chemo",
                  type = "poly")
  expect_is(mm_poly, "metamodel")

  # predictions
  pred_poly <- predict(mm_poly)
  expect_equal(colnames(pred_poly),
               c("parameter", "strategy", "param_val", "outcome_val"))

  ## GAM
  mm_gam <- metamodel(psa = psa_big, parms = "pFailChemo", outcome = "cost", strategies = "Chemo",
                  type = "gam")
  expect_is(mm_gam, "metamodel")

  # predictions
  pred_gam <- predict(mm_gam)
  expect_equal(colnames(pred_gam),
               c("parameter", "strategy", "param_val", "outcome_val"))
})

test_that("prediction with several outcomes", {
  # metamodel
  mm <- metamodel(psa = psa_big, outcome = "eff")
  expect_is(mm, "metamodel")

  # number of linear models
  expect_equal(length(mm$mods), 8)

  # number of strategies for each
  nstrats <- sapply(mm$mods, length)
  expect_setequal(nstrats, 3)

  # predictions - same colnames
  pred <- predict(mm)
  expect_equal(colnames(pred),
               c("parameter", "strategy", "param_val", "outcome_val"))
})

test_that("type and content checking with predict", {
  # metamodel
  mm <- metamodel(psa = psa_big, outcome = "eff")

  # good example 1:  a list with some parameters
  preds1 <- predict(mm,
                    ranges = list("pFailChemo" = c(0.3, 0.6),
                                  "pFailRadio" = c(0.4, 0.5)),
                    nsamp = 10)
  expect_equal(nrow(preds1), 60)

  # good example 2: a list with some NULL parameters
  preds2 <- predict(mm,
                    ranges = list("pFailChemo" = NULL,
                                  "pFailRadio" = NULL),
                    nsamp = 10)
  expect_equal(nrow(preds2), 60)

  # good example 3: a list with a custom range and a NULL
  preds3 <- predict(mm,
                    ranges = list("pFailChemo" = c(0.3, 0.6),
                                  "pFailRadio" = NULL),
                    nsamp = 10)
  expect_equal(nrow(preds3), 60)

  # bad example 1: someone passes vector
  expect_error(predict(mm, ranges = c("pFailChemo" = c(0.3, 0.6),
                                      "pFailRadio" = c(0.4, 0.5)),
                       nsamp = 10), regexp = "list")

  # bad example 2: pass list but with length 1 or 3
  expect_error(predict(mm, ranges = list("pFailChemo" = c(0), "pFailRadio" = c(0.3, 0.4)),
                       nsamp = 10), regexp = "length 2")

})

test_that("out of range warning", {
  # metamodel
  mm <- metamodel(psa = psa_big, outcome = "eff")

  # out of range
  expect_warning(predict(mm, ranges = list("pFailChemo" = c(-1, 1))),
                 regexp = "caution")

  # not out of range
  expect_silent(predict(mm, ranges = list("pFailChemo" = c(0.4, 0.3))))
})

test_that("two-way metamodel", {
  # metamodel
  mm <- metamodel(analysis = "twoway", parms = c("pFailChemo", "pFailRadio"),
                psa = psa_big, outcome = "eff")
  expect_is(mm, "metamodel")
})
