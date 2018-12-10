context('ceac methods')
library(dampack)

source('load_test_data.R')

test_that("message is correct in summary.ceac", {
  c <- ceac(wtp, costs, effectiveness, strategies)
  msg <- capture.output(summary(c))
  expected <- c("There was one switch in the optimal strategy.",
                "At WTP = 51000 the optimal strategy changed from Radio to Chemo")
  expect_equal(expected, msg)
})
