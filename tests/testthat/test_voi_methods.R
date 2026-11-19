# Properties that EVPI, EVPPI, and EVSI must satisfy, and agreement with a
# direct implementation of the regression and Gaussian approximation methods
# (Strong et al. 2014; Jalal and Alarid-Escudero 2018).

psa_voi <- make_psa_obj(cancer_psa_data$cost, cancer_psa_data$effectiveness,
                        cancer_psa_data$parameters, cancer_psa_data$strategies)
v_wtp_voi <- c(5e4, 1e5, 2e5)
param_voi <- "pFailChemo"

evpi_voi <- calc_evpi(psa_voi, wtp = v_wtp_voi)
evppi_voi <- calc_evppi(psa_voi, wtp = v_wtp_voi, params = param_voi, progress = FALSE)

# opportunity loss relative to the strategy that is optimal in expectation
loss_at <- function(wtp) {
  m_nmb <- as.matrix(psa_voi$effectiveness * wtp - psa_voi$cost)
  m_nmb - m_nmb[, which.max(colMeans(m_nmb))]
}

test_that("EVPPI is between 0 and EVPI", {
  expect_true(all(evppi_voi$df_evppi$EVPPI >= -1e-8))
  expect_true(all(evppi_voi$df_evppi$EVPPI <= evpi_voi$EVPI + 1e-8))
})

test_that("calc_evppi() equals a direct GAM regression estimate", {
  v_direct <- sapply(v_wtp_voi, function(wtp) {
    m_loss <- loss_at(wtp)
    m_fitted <- apply(m_loss, 2, function(y) {
      fitted(mgcv::gam(y ~ s(pFailChemo), data = data.frame(y, psa_voi$parameters)))
    })
    mean(apply(m_fitted, 1, max))
  })
  expect_equal(evppi_voi$df_evppi$EVPPI, v_direct)
})

test_that("calc_evsi() equals a direct Gaussian approximation", {
  n <- 50
  n0 <- 20
  evsi <- calc_evsi(psa_voi, wtp = 1e5, params = param_voi, outcome = "nmb",
                    n = n, n0 = n0, progress = FALSE)
  # shrink each basis function of the metamodel toward its mean by sqrt(n / (n + n0))
  vrf <- sqrt(n / (n + n0))
  m_tilde <- apply(loss_at(1e5), 2, function(y) {
    fit <- mgcv::gam(y ~ s(pFailChemo), data = data.frame(y, psa_voi$parameters))
    m_x <- predict(fit, type = "lpmatrix")
    m_x_ga <- vrf * m_x + (1 - vrf) * matrix(colMeans(m_x), nrow(m_x), ncol(m_x), byrow = TRUE)
    m_x_ga %*% coef(fit)
  })
  expect_equal(evsi$df_evsi$EVSI, mean(apply(m_tilde, 1, max)))
})

test_that("EVSI is 0 without a study, increases with n, and tends to EVPPI", {
  evsi <- calc_evsi(psa_voi, wtp = v_wtp_voi, params = param_voi, outcome = "nmb",
                    n = c(0, 10, 100, 1000, 1e8), n0 = 10, progress = FALSE)$df_evsi
  for (wtp in v_wtp_voi) {
    v_evsi <- evsi$EVSI[evsi$WTP == wtp]
    expect_equal(v_evsi[1], 0, tolerance = 1e-8)
    expect_true(all(diff(v_evsi) >= -1e-8))
    expect_equal(v_evsi[5], evppi_voi$df_evppi$EVPPI[evppi_voi$df_evppi$WTP == wtp],
                 tolerance = 1e-5)
  }
})

test_that("EVPPI and EVSI scale with the population", {
  evppi_pop <- calc_evppi(psa_voi, wtp = 1e5, params = param_voi, pop = 1000,
                          progress = FALSE)
  expect_equal(evppi_pop$df_evppi$EVPPI,
               1000 * evppi_voi$df_evppi$EVPPI[evppi_voi$df_evppi$WTP == 1e5])
  evsi_1 <- calc_evsi(psa_voi, wtp = 1e5, params = param_voi, outcome = "nmb",
                      progress = FALSE)
  evsi_pop <- calc_evsi(psa_voi, wtp = 1e5, params = param_voi, outcome = "nmb",
                        pop = 1000, progress = FALSE)
  expect_equal(evsi_pop$df_evsi$EVSI, 1000 * evsi_1$df_evsi$EVSI)
})

test_that("EVPPI in NHB is EVPPI in NMB divided by the threshold", {
  # the NHB and NMB losses differ by the factor 1 / wtp, and GAM fits are
  # invariant to rescaling the response
  evppi_nhb <- calc_evppi(psa_voi, wtp = 1e5, params = param_voi, outcome = "nhb",
                          progress = FALSE)
  expect_equal(evppi_nhb$df_evppi$EVPPI,
               evppi_voi$df_evppi$EVPPI[evppi_voi$df_evppi$WTP == 1e5] / 1e5,
               tolerance = 1e-6)
})
