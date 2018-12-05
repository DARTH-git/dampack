context("ceac")
library(dampack)

## test data
wtp <- seq(0, 2e5, by = 5e4)
strategies <- c("good", "bad", "worse")
costs <- matrix(c(30, 20, 100,
                     10, 10, 5),
                   byrow = TRUE,
                   nrow = 2)
effectiveness <- matrix(c(20, 10, 50,
                          2, 11, 3),
                        byrow = TRUE,
                        nrow = 2)

## tests

test_that("result has classes 'data.frame' and 'ceac'", {
          c <- ceac(wtp, costs, effectiveness, strategies)
          expect_true(inherits(c, "data.frame"))
          expect_true(inherits(c, "ceac"))
})

