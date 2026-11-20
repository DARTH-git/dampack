context("converting between BCEA and dampack objects (#204)")

# a minimal object with the structure of BCEA::bcea() output, so bcea_to_psa() can be tested
# without BCEA installed
fake_bcea <- function() {
  set.seed(204)
  m_e <- cbind(rnorm(50, 1, 0.1), rnorm(50, 1.2, 0.1))
  m_c <- cbind(rnorm(50, 100, 10), rnorm(50, 150, 10))
  structure(list(e = m_e, c = m_c, interventions = c("Status Quo", "Vaccination")),
            class = c("bcea", "list"))
}

test_that("bcea_to_psa() builds a psa object with the intervention names and outcomes", {
  m <- fake_bcea()
  p <- bcea_to_psa(m, currency = "£")
  expect_is(p, "psa")
  expect_identical(p$strategies, c("Status Quo", "Vaccination"))
  expect_equal(unname(as.matrix(p$cost)), m$c)
  expect_equal(unname(as.matrix(p$effectiveness)), m$e)
  expect_identical(p$currency, "£")
  expect_null(p$parameters)

  df_par <- data.frame(p_a = runif(50), c_b = runif(50))
  expect_identical(bcea_to_psa(m, parameters = df_par)$parnames, c("p_a", "c_b"))
})

test_that("bcea_to_psa() rejects objects that are not bcea output", {
  expect_error(bcea_to_psa(list(e = 1, c = 1)), "must be an object of class \"bcea\"")
  broken <- structure(list(e = matrix(1, 2, 2)), class = "bcea")
  expect_error(bcea_to_psa(broken), "must contain the costs")
})

test_that("BCEA and dampack round-trip, and their EVPI agree", {
  skip_if_not_installed("BCEA")
  data(Vaccine, package = "BCEA", envir = environment())
  m <- BCEA::bcea(eff, cost, ref = 2, interventions = treats, Kmax = 50000)
  p <- bcea_to_psa(m)
  expect_identical(p$strategies, m$interventions)

  m2 <- psa_to_bcea(p, ref_str = "Vaccination", Kmax = 50000)
  expect_is(m2, "bcea")
  expect_equal(unname(m2$e), unname(m$e))
  expect_equal(unname(m2$c), unname(m$c))
  expect_equal(m2$ref, m$ref)

  # an independent check of calc_evpi(): BCEA computes the EVPI from the same samples
  v_idx <- c(1, 101, 251, 501)
  expect_equal(calc_evpi(p, m$k[v_idx])$EVPI, m$evi[v_idx], tolerance = 1e-8)

  expect_error(psa_to_bcea(p, ref_str = "Nope"), "must be one of the strategies")
})
