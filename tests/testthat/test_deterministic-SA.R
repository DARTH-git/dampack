context("deterministic-SA")
library(dampack)

source("util_models_detfun.R")

state_name <- c("S", "IS", "IA", "T")
cycle <- 100
popsize <- 1000

parms_df <- data.frame(pars = c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec"),
                       basecase = c(0.05, 0.8, 0.8, 0.01, 0.01),
                       min = c(0, 0.5, 0.5, 0, 0),
                       max = c(0.5, 0.9, 1, 0.2, 0.5))

parms <- c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec")
outcome <- c("tot_incidence")

# testing owsa_det
test_that("correct input in owsa_det", {
  expect_silent(owsa_det(parms, parms_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcome2 <- "prev"
  expect_silent(owsa_det(parms, parms_df, nsamps = 10,
                         strategy_func, outcome2, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_parms <- sort(parms)
  expect_silent(owsa_det(temp_parms, parms_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_parms <- parms[-1]
  expect_silent(owsa_det(temp_parms, parms_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(owsa_det(parms, parms_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})


test_that("check output of owsa_det", {
  o <- owsa_det(parms, parms_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_is(o, "owsa")
  expect_is(o, "data.frame")

  parms2 <- parms[-2]
  o <- owsa_det(parms2, parms_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_equal(unique(as.character(o$parameter)), parms2)


  o <- owsa_det(parms2, parms_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)

  min_val <- o %>%
    group_by(parameter) %>%
    summarise(min_val = min(param_val)) %>%
    select(min_val)
  expect_equal(min_val$min_val,
               parms_df[match(parms2, parms_df$pars), "min"])

  max_val <- o %>%
    group_by(parameter) %>%
    summarise(max_val = max(param_val)) %>%
    select(max_val)
  expect_equal(max_val$max_val,
               parms_df[match(parms2, parms_df$pars), "max"])
})


test_that("check output from owsa_det with other dampack functions", {
  o <- owsa_det(parms, parms_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  g <- plot(o)
  expect_is(g, "ggplot")
  expect_silent(g)
})

test_that("accurately producing warnings and errors in owsa_det", {
  temp_parms <- parms
  temp_parms[1] <- c("xx")
  expect_error(owsa_det(temp_parms, parms_df, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "parms should be in the parameters provided in pars_df")

  parms_df2 <- as.matrix(parms_df)
  expect_error(owsa_det(parms, parms_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "pars_df must be a data.frame")

  parms_df2 <- parms_df
  parms_df2[, 3] <- parms_df[, 4]
  parms_df2[, 4] <- parms_df[, 3]
  expect_error(owsa_det(parms, parms_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase has to be in between min and max")

  parms_df2 <- parms_df
  parms_df2[2, 3] <- as.character(parms_df2[2, 3])
  expect_error(owsa_det(parms, parms_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase, min and maximum in pars_df must be numeric")

  expect_error(owsa_det(parms, parms_df, nsamps = 10),
               "FUN is missing")

  expect_error(owsa_det(parms, parms_df, nsamps = 10,
                        strategy_func, "treated", outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "outcome is not in FUN outcomes")

  tmp_strategy <- c("redInf", "incScr", "incCov")
  expect_error(owsa_det(parms, parms_df, nsamps = 10,
                        strategy_func, "treated", outcome_type = "eff",
                        strategies = tmp_strategy,
                        state_name = state_name, cycle = cycle),
               "number of strategies is not the same as the number of strategies in user defined FUN")
})


# testing twsa_det
parm1 <- "p_inf"
parm2 <- "p_test_asymp"

test_that("check input in twsa_det", {
  expect_silent(twsa_det(parm1, parm2, parms_df, nsamps = 30,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcome2 <- "prev"
  expect_silent(twsa_det(parm1, parm2, parms_df, nsamps = 30,
                         strategy_func, outcome2, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(twsa_det(parm1, parm2, parms_df, nsamps = 30,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})

test_that("check output from twsa_det", {
  strategy <- c("s1", "s2")
  tw <- twsa_det(parm1, parm2, parms_df, nsamps = 30,
                 strategy_func, outcome, outcome_type = "eff",
                 strategies = strategy,
                 state_name = state_name, cycle = cycle)
  expect_is(tw, "twsa")
  expect_is(tw, "data.frame")

  expect_equal(min(tw[, parm1]), parms_df$min[parms_df$pars == parm1])
  expect_equal(max(tw[, parm1]), parms_df$max[parms_df$pars == parm1])
  expect_equal(min(tw[, parm2]), parms_df$min[parms_df$pars == parm2])
  expect_equal(max(tw[, parm2]), parms_df$max[parms_df$pars == parm2])
  expect_equal(unique(as.character(tw[, "strategy"])), strategy)
})

test_that("checking warning error message from twsa_det", {
  expect_error(twsa_det(parm1, parm2, parms_df, nsamps = 30,
                        outcome = outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "FUN is missing")

  expect_error(twsa_det(c("p_inf", "p_rec"), c("p_asymp"), parms_df, nsamps = 30,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")

  expect_error(twsa_det("xx", parm2, parms_df, nsamps = 30,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "parm1 and parm2 should be in the parameters provided in pars_df")
})
