context("plot.ceac")
library(dampack)

source('load_test_data.R')
psa_obj <- psa_results(costs, effectiveness, strategies)

test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, psa_obj)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_true(inherits(gf, "ggplot"))
  gnof <- plot(ceac_obj, rontier = FALSE)
  expect_true(inherits(gnof, "ggplot"))
})
