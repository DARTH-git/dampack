# context("plot.ceac")
library(dampack)
library(testthat)

## test data
wtp <- c(50000, 100000, 200000)
strategies <- c("good", "bad", "worse")
costs <- matrix(c(3e2, 2e4, 2e4,
                  1e2, 1e4, 1e5),
                byrow = TRUE,
                nrow = 2)
effectiveness <- matrix(c(200, 1000, 1000,
                          190, 1100, 1200),
                        byrow = TRUE,
                        nrow = 2)

test_that("plot.ceac produces ggplot object", {
  ceac_obj <- ceac(wtp, costs, effectiveness, strategies)
  g <- plot(ceac_obj)
  expect_true(inherits(g, "ggplot"))
})
