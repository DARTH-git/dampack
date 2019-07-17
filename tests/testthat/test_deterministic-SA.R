context("deterministic-SA")
library(dampack)

source("util_models_detfun.R")

state_name <- c("S", "IS", "IA", "T")
cycle <- 100
popsize <- 1000

params_df <- data.frame(pars = c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec"),
                       basecase = c(0.05, 0.8, 0.8, 0.01, 0.01),
                       min = c(0, 0.5, 0.5, 0, 0),
                       max = c(0.5, 0.9, 1, 0.2, 0.5))

params <- c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec")
outcome <- c("tot_incidence")

# testing owsa_det
test_that("correct input in owsa_det", {
  expect_silent(owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcome2 <- "prev"
  expect_silent(owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcome2, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_params <- sort(params)
  expect_silent(owsa_det(temp_params, params_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_params <- params[-1]
  expect_silent(owsa_det(temp_params, params_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})


test_that("check output of owsa_det", {
  o <- owsa_det(params, params_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_is(o, "owsa")
  expect_is(o, "data.frame")

  params2 <- params[-2]
  o <- owsa_det(params2, params_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_equal(unique(as.character(o$parameter)), params2)


  o <- owsa_det(params2, params_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)

  min_val <- o %>%
    group_by(parameter) %>%
    summarise(min_val = min(param_val)) %>%
    select(min_val)
  expect_equal(min_val$min_val,
               params_df[match(params2, params_df$pars), "min"])

  max_val <- o %>%
    group_by(parameter) %>%
    summarise(max_val = max(param_val)) %>%
    select(max_val)
  expect_equal(max_val$max_val,
               params_df[match(params2, params_df$pars), "max"])
})


test_that("check output from owsa_det with other dampack functions", {
  o <- owsa_det(params, params_df, nsamps = 10,
                strategy_func, outcome, outcome_type = "eff",
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  g <- plot(o)
  expect_is(g, "ggplot")
  expect_silent(g)
})

test_that("accurately producing warnings and errors in owsa_det", {
  temp_params <- params
  temp_params[1] <- c("xx")
  expect_error(owsa_det(temp_params, params_df, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "params should be in the parameters provided in pars_df")

  params_df2 <- as.matrix(params_df)
  expect_error(owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "pars_df must be a data.frame")

  params_df2 <- params_df
  params_df2[, 3] <- params_df[, 4]
  params_df2[, 4] <- params_df[, 3]
  expect_error(owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase has to be in between min and max")

  params_df2 <- params_df
  params_df2[2, 3] <- as.character(params_df2[2, 3])
  expect_error(owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase, min and max in pars_df must be numeric")

  expect_error(owsa_det(params, params_df, nsamps = 10,
                        strategy_func, "treated", outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "outcome is not in FUN outcomes")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(owsa_det(params, params_df, nsamps = 10,
                        strategy_func2, outcome, outcome_type = "eff",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")

  tmp_strategy <- c("redInf", "incScr", "incCov")
  expect_error(owsa_det(params, params_df, nsamps = 10,
                        strategy_func, "treated", outcome_type = "eff",
                        strategies = tmp_strategy,
                        state_name = state_name, cycle = cycle),
               "number of strategies is not the same as the number of strategies in user defined FUN")
})


# testing twsa_det
param1 <- "p_inf"
param2 <- "p_test_asymp"

test_that("check input in twsa_det", {
  expect_silent(twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcome2 <- "prev"
  expect_silent(twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcome2, outcome_type = "eff",
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcome, outcome_type = "eff",
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})

test_that("check output from twsa_det", {
  strategy <- c("s1", "s2")
  tw <- twsa_det(param1, param2, params_df, nsamps = 30,
                 strategy_func, outcome, outcome_type = "eff",
                 strategies = strategy,
                 state_name = state_name, cycle = cycle)
  expect_is(tw, "twsa")
  expect_is(tw, "data.frame")

  expect_equal(min(tw[, param1]), params_df$min[params_df$pars == param1])
  expect_equal(max(tw[, param1]), params_df$max[params_df$pars == param1])
  expect_equal(min(tw[, param2]), params_df$min[params_df$pars == param2])
  expect_equal(max(tw[, param2]), params_df$max[params_df$pars == param2])
  expect_equal(unique(as.character(tw[, "strategy"])), strategy)
})

test_that("checking warning error message from twsa_det", {
  expect_error(twsa_det(c("p_inf", "p_rec"), c("p_asymp"), params_df, nsamps = 30,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")

  expect_error(twsa_det("xx", param2, params_df, nsamps = 30,
                        strategy_func, outcome, outcome_type = "eff",
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "param1 and param2 should be in the parameters provided in pars_df")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(twsa_det(param1, param2, params_df, nsamps = 30,
                        strategy_func2, outcome, outcome_type = "eff",
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
})
