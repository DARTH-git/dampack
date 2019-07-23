context("dsa oneway")
library(dampack)

# creation of example data
nsamps <- 20
p1 <- data.frame("parameter" = "p1",
                 "paramval" = seq(0, 1, length.out = nsamps),
                 stringsAsFactors = FALSE)
e1 <- data.frame("s1" = 4 * p1$paramval - 2,
                 "s2" = 5 * p1$paramval + 3)
c1 <- data.frame("s1" = 1e4 + 1000 * p1$paramval,
                 "s2" = 1e2 + 2000 * p1$paramval)

p2 <- data.frame("parameter" = "p2",
                 "paramval" = seq(4, 5, length.out = nsamps),
                 stringsAsFactors = FALSE)
e2 <- data.frame("s1" = 2 * p2$paramval ^ 2 - 10,
                 "s2" = 3 * p2$paramval ^ 2 + 4 * p2$paramval - 20)
c2 <- data.frame("s1" = 1e5 - 2000 * p2$paramval,
                 "s2" = 1e6 - 1300 * p2$paramval)

strategies <- c("s1", "s2")

ps <- rbind(p1, p2)
es <- rbind(e1, e2)
cs <- rbind(c1, c2)

# class creation
test_that("dsa class creation", {
  # with cost
  expect_silent(create_dsa_oneway(ps, es, strategies, cs))
  # without cost
  expect_silent(create_dsa_oneway(ps, es, strategies, cs))
})

test_that("correct class members", {
  dsa <- create_dsa_oneway(ps, es, strategies)
  expect_equal(dsa$n_strategies, 2)
  expect_equal(length(dsa$parnames), 2)
  expect_equal(dsa$parameters, ps)
  expect_null(dsa$cost)
})

# plots
test_that("dsa plots", {
  dsa <- create_dsa_oneway(ps, es, strategies, cs)
  o <- owsa(dsa, outcome = "nmb", wtp = 1e5)
  expect_silent(owsa_tornado(o, min_rel_diff = 0))
  expect_silent(owsa_opt_strat(o))
})
