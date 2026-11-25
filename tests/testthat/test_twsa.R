library(dampack)

# test the class
test_that("twsa has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "twsa"))
  expected_methods <- c("plot.twsa")
  expect_equal(current_methods, expected_methods)
})

# class creation

## setup
data("cancer_psa_data")
psa_big <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)

test_that("twsa object has correct classes", {
  t <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  expect_s3_class(t, "twsa")
  expect_s3_class(t, "data.frame")
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
  expect_s3_class(g, "ggplot")
})


# test basecase argument for plot.twsa
test_that("plot.twsa basecase argument", {
  t <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  g <- plot(t,
            basecase = list(pFailChemo = 0.37, muDieCancer = 0.09))
  expect_s3_class(g, "ggplot")

  t2 <- twsa(psa_big, "pFailChemo", "muDieCancer", outcome = "eff")
  g2 <- plot(t,
             basecase = list(muDieCancer = 0.09, pFailChemo = 0.37))
  expect_s3_class(g2, "ggplot")

  # test error when wrong parameter is supplied
  expect_error(plot(t,
                    basecase = list(pFailChemo = 0.37, non_existent = .050)),
               "Some parameter names in the basecase argument do not match param1 or param2 of twsa")
})
