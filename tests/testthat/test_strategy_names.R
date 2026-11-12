context("strategy names are kept as typed (#208)")

# Strategy names that are not syntactically valid R names: spaces, parentheses,
# a leading digit, and a non-ASCII character.
v_names <- c("Standard of care", "New drug (A)", "2-dose regimen ñ")
v_wtp <- seq(10000, 150000, by = 20000)

cancer_psa_data <- dampack::cancer_psa_data
make_named_psa <- function() {
  make_psa_obj(cost = cancer_psa_data$cost,
               effectiveness = cancer_psa_data$effectiveness,
               parameters = cancer_psa_data$parameters,
               strategies = v_names)
}

guide_labels <- function(p, aesthetic = "colour") {
  as.character(ggplot2::get_guide_data(p, aesthetic)$.label)
}

test_that("make_psa_obj() keeps the names and does not warn", {
  expect_warning(l_psa <- make_named_psa(), NA)
  expect_identical(l_psa$strategies, v_names)
  expect_identical(names(l_psa$cost), v_names)
  expect_identical(names(l_psa$effectiveness), v_names)
  expect_identical(summary(l_psa)$Strategy, v_names)
})

test_that("ICERs, CEAC, expected loss, and EVPI keep the names", {
  l_psa <- make_named_psa()
  df_sum <- summary(l_psa)
  df_icers <- calculate_icers(df_sum$meanCost, df_sum$meanEffect, df_sum$Strategy)
  expect_setequal(df_icers$Strategy, v_names)

  df_ceac <- ceac(v_wtp, l_psa)
  expect_setequal(as.character(unique(df_ceac$Strategy)), v_names)
  expect_true(all(summary(df_ceac)$optimal_strategy %in% v_names))

  df_elc <- calc_exp_loss(l_psa, v_wtp)
  expect_setequal(as.character(unique(df_elc$Strategy)), v_names)

  expect_s3_class(calc_evpi(l_psa, v_wtp), "evpi")
})

test_that("metamodel-based analyses work with non-syntactic names", {
  l_psa <- make_named_psa()
  l_mm <- metamodel("oneway", l_psa, params = "pFailChemo", outcome = "nmb", wtp = 50000)
  expect_identical(l_mm$strategies, v_names)

  df_owsa <- owsa(l_psa, params = "pFailChemo", outcome = "nmb", wtp = 50000)
  expect_setequal(as.character(unique(df_owsa$strategy)), v_names)

  df_twsa <- twsa(l_psa, param1 = "pFailChemo", param2 = "pFailRadio",
                  outcome = "nmb", wtp = 50000)
  expect_setequal(as.character(unique(df_twsa$strategy)), v_names)

  df_evppi <- calc_evppi(l_psa, wtp = c(50000, 100000), params = "pFailChemo",
                         outcome = "nmb", type = "gam", progress = FALSE)
  expect_true(all(is.finite(df_evppi$EVPPI)))
})

test_that("plot legends show the names exactly as typed", {
  l_psa <- make_named_psa()
  expect_setequal(guide_labels(plot(l_psa)), v_names)
  expect_setequal(guide_labels(plot(ceac(v_wtp, l_psa))), v_names)
  expect_setequal(guide_labels(plot(calc_exp_loss(l_psa, v_wtp))), v_names)
  df_owsa <- owsa(l_psa, params = "pFailChemo", outcome = "nmb", wtp = 50000)
  expect_setequal(guide_labels(plot(df_owsa)), v_names)
})

test_that("invalid strategy names give informative errors", {
  expect_error(make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                            strategies = c("A", "A", "B")),
               "must be unique")
  expect_error(make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                            strategies = c("A", NA, "B")),
               "missing or empty")
  expect_error(make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                            strategies = c("A", "", "B")),
               "missing or empty")
  expect_error(make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                            strategies = c("A", "B")),
               "3 strategy names are needed")
})
