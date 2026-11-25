test_that("cancer_psa_data has the documented structure", {
  data("cancer_psa_data")
  expect_named(cancer_psa_data, c("strategies", "cost", "effectiveness", "parameters"))
  expect_equal(cancer_psa_data$strategies, c("Chemo", "Radio", "Surgery"))
  expect_equal(dim(cancer_psa_data$cost), c(10000, 3))
  expect_equal(dim(cancer_psa_data$effectiveness), c(10000, 3))
  expect_equal(dim(cancer_psa_data$parameters), c(10000, 8))
})

test_that("superseded example_psa still loads and matches cancer_psa_data", {
  data("cancer_psa_data")
  data("example_psa")
  expect_equal(example_psa$wtp, seq(1000, 141000, by = 10000))
  for (el in names(cancer_psa_data)) {
    expect_identical(example_psa[[el]], cancer_psa_data[[el]])
  }
})

test_that("superseded example_psa_obj still loads as a psa object", {
  data("cancer_psa_data")
  data("example_psa_obj")
  expect_s3_class(example_psa_obj, "psa")
  expect_equal(example_psa_obj$strategies, cancer_psa_data$strategies)
  expect_equal(unname(as.matrix(example_psa_obj$cost)),
               unname(as.matrix(cancer_psa_data$cost)))
  expect_equal(unname(as.matrix(example_psa_obj$effectiveness)),
               unname(as.matrix(cancer_psa_data$effectiveness)))
  expect_identical(example_psa_obj$parameters, cancer_psa_data$parameters)
})
