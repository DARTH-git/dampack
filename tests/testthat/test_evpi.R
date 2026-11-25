library(dampack)


# test the class methods
test_that("evpi has all methods we'd expect", {
  current_methods <- as.vector(methods(class = "evpi"))
  expected_methods <- c("plot.evpi")
  expect_equal(current_methods, expected_methods)
})

# test return object
## setup
data("cancer_psa_data")
v_wtp <- seq(1000, 141000, by = 10000)
psa_obj <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)

evpi_obj <- calc_evpi(psa = psa_obj,
                      wtp = v_wtp)

# classes
test_that("return object has classes evpi and data.frame", {
  expect_s3_class(evpi_obj, "evpi")
  expect_s3_class(evpi_obj, "data.frame")
})

# structure
test_that("return object has column names WTP and EVPI", {
  expect_equal(names(evpi_obj), c("WTP", "EVPI"))
})

# calculation
test_that("evpi is what we'd expect", {
  nmb <-  v_wtp[1] * cancer_psa_data$effectiveness - cancer_psa_data$cost
  n_sim <- nrow(cancer_psa_data$cost)
  max_str <- max.col(nmb)
  loss <-  nmb[cbind(1:n_sim, max_str)] - nmb
  evpi <- min(apply(loss, 2, mean))
  expect_equal(evpi, evpi_obj[1, 2])
})

# zero-length input: seq_len() loop must not misbehave when n == 0
test_that("calc_evpi returns an empty evpi object for zero-length wtp", {
  empty_evpi <- calc_evpi(psa = psa_obj, wtp = numeric(0))
  expect_s3_class(empty_evpi, "evpi")
  expect_s3_class(empty_evpi, "data.frame")
  expect_equal(nrow(empty_evpi), 0)
  expect_equal(names(empty_evpi), c("WTP", "EVPI"))
})

# methods

## plot
test_that("plot.evpi has ggplot class", {
  p <- plot(evpi_obj,
            txtsize = 16, effect_units = "QALY", currency = "Dollars ($)",
            xbreaks = seq(0, 200, by = 10), ylim = c(0, 100000))
  expect_s3_class(p, "ggplot")
})
