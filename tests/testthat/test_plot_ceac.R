context("plot.ceac")
library(dampack)

source('load_test_data.R')

test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, costs, effectiveness, strategies)
  gf <- plot(ceac_obj, frontier = TRUE)
  expect_true(inherits(gf, "ggplot"))
  gnof <- plot(ceac_obj, rontier = FALSE)
  expect_true(inherits(gnof, "ggplot"))
})
