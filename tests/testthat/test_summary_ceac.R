context('ceac methods')
library(dampack)

source('load_test_data.R')
psa_obj <- psa_results(costs, effectiveness, strategies)

test_that("message is correct in summary.ceac", {
  c <- ceac(wtp, psa_obj)
  msg <- capture.output(summary(c))
  expected <- c("There was one switch in the optimal strategy.",
                "At WTP = 51000 the optimal strategy changed from Radio to Chemo")
  expect_equal(expected, msg)
})
