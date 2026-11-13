# the currency of a psa object carries through to results, plots, and tables (#203)

cancer_psa_data <- dampack::cancer_psa_data
psa_eur <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies,
                        currency = "€")
psa_usd <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
v_wtp <- seq(10000, 100000, by = 30000)
eur <- "€"

test_that("ceac() and calc_exp_loss() keep the currency and use it in their plots", {
  df_ceac <- ceac(v_wtp, psa_eur)
  expect_identical(attr(df_ceac, "currency"), eur)
  expect_match(plot(df_ceac)$labels$x, eur, fixed = TRUE)
  expect_match(plot(df_ceac, currency = "£")$labels$x, "£", fixed = TRUE)
  expect_match(plot(df_ceac, effect_units = "LY")$labels$x, "/ LY)", fixed = TRUE)

  df_elc <- calc_exp_loss(psa_eur, v_wtp)
  expect_identical(attr(df_elc, "currency"), eur)
  expect_equal(plot(df_elc)$labels$y, paste0("Expected Loss (", eur, ")"))
})

test_that("EVPI, EVPPI, and EVSI plots use the currency or the health units of the outcome", {
  df_evpi <- calc_evpi(psa_eur, v_wtp)
  expect_identical(attr(df_evpi, "currency"), eur)
  expect_equal(plot(df_evpi)$labels$y, paste0("EVPI (", eur, ")"))

  evppi_nmb <- calc_evppi(psa_eur, v_wtp, params = "pFailChemo", outcome = "nmb", progress = FALSE)
  expect_equal(plot(evppi_nmb)$labels$y, paste0("EVPPI (", eur, ")"))
  evppi_nhb <- calc_evppi(psa_eur, v_wtp, params = "pFailChemo", outcome = "nhb", progress = FALSE)
  expect_equal(plot(evppi_nhb)$labels$y, "EVPPI (QALY)")

  # calc_evsi() defaults to outcome = "nhb", whose EVSI is in health units, not currency
  evsi_nhb <- calc_evsi(psa_eur, v_wtp, params = "pFailChemo", n = 100, n0 = 10, progress = FALSE)
  expect_equal(plot(evsi_nhb)$labels$y, "EVSI (QALY)")
  evsi_nmb <- calc_evsi(psa_eur, v_wtp, params = "pFailChemo", n = 100, n0 = 10,
                        outcome = "nmb", progress = FALSE)
  expect_equal(plot(evsi_nmb)$labels$y, paste0("EVSI (", eur, ")"))
})

test_that("calculate_icers_psa() keeps the currency for print and plot", {
  x <- calculate_icers_psa(psa_eur)
  expect_identical(attr(x, "currency"), eur)
  expect_true(any(grepl(paste0("Costs (", eur, ")"), capture.output(print(x)), fixed = TRUE)))
  expect_equal(plot(x)$labels$y, paste0("Cost (", eur, ")"))
  x_unc <- calculate_icers_psa(psa_eur, uncertainty = TRUE)
  expect_identical(attr(x_unc, "currency"), eur)
})

test_that("a dollar psa object still gives dollar labels", {
  expect_match(plot(ceac(v_wtp, psa_usd))$labels$x, "$", fixed = TRUE)
  expect_equal(plot(calc_evpi(psa_usd, v_wtp))$labels$y, "EVPI ($)")
})
