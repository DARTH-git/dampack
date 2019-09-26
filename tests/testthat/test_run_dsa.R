context("run_dsa")
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
outcomes <- c("tot_incidence")

# testing run_owsa_det
test_that("correct input in run_owsa_det", {
  expect_silent(run_owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcomes_2 <- "prev"
  expect_silent(run_owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcomes_2,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_params <- sort(params)
  expect_silent(run_owsa_det(temp_params, params_df, nsamps = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  temp_params <- params[-1]
  expect_silent(run_owsa_det(temp_params, params_df, nsamps = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(run_owsa_det(params, params_df, nsamps = 10,
                         strategy_func, outcomes,
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})


test_that("check output of run_owsa_det", {
  o <- run_owsa_det(params, params_df, nsamps = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_is(o, "owsa")
  expect_is(o, "data.frame")

  params2 <- params[-2]
  o <- run_owsa_det(params2, params_df, nsamps = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  expect_equal(unique(as.character(o$parameter)), params2)


  o <- run_owsa_det(params2, params_df, nsamps = 10,
                strategy_func, outcomes,
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


test_that("check output from run_owsa_det with other dampack functions", {
  o <- run_owsa_det(params, params_df, nsamps = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle)
  g <- plot(o)
  expect_is(g, "ggplot")
  expect_silent(g)
})

test_that("accurately producing warnings and errors in run_owsa_det", {
  temp_params <- params
  temp_params[1] <- c("xx")
  expect_error(run_owsa_det(temp_params, params_df, nsamps = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "params should be in the parameters provided in params_all")

  params_df2 <- as.matrix(params_df)
  expect_error(run_owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "params_all must be a data.frame")

  params_df2 <- params_df
  params_df2[, 3] <- params_df[, 4]
  params_df2[, 4] <- params_df[, 3]
  expect_error(run_owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase has to be in between min and max")

  params_df2 <- params_df
  params_df2[2, 3] <- as.character(params_df2[2, 3])
  expect_error(run_owsa_det(params, params_df2, nsamps = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "basecase, min and max in params_all must be numeric")

  expect_error(run_owsa_det(params, params_df, nsamps = 10,
                        strategy_func, outcomes = "treated",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "at least one outcome is not in FUN outcomes")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(run_owsa_det(params, params_df, nsamps = 10,
                        strategy_func2, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")

  tmp_strategy <- c("redInf", "incScr", "incCov")
  expect_error(run_owsa_det(params, params_df, nsamps = 10,
                        strategy_func, outcomes = "treated",
                        strategies = tmp_strategy,
                        state_name = state_name, cycle = cycle),
               "number of strategies is not the same as the number of strategies in user defined FUN")
})


# testing run_twsa_det
param1 <- "p_inf"
param2 <- "p_test_asymp"

test_that("check input in run_twsa_det", {
  expect_silent(run_twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  outcomes_2 <- "prev"
  expect_silent(run_twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcomes_2,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle))

  strategies <- c("redInf", "incScr")
  expect_silent(run_twsa_det(param1, param2, params_df, nsamps = 30,
                         strategy_func, outcomes,
                         strategies = strategies,
                         state_name = state_name, cycle = cycle))
})

test_that("check output from run_twsa_det", {
  strategy <- c("s1", "s2")
  tw <- run_twsa_det(param1, param2, params_df, nsamps = 30,
                 strategy_func, outcomes,
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

test_that("checking warning error message from run_twsa_det", {
  expect_error(run_twsa_det(c("p_inf", "p_rec"), c("p_asymp"), params_df, nsamps = 30,
                        strategy_func, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")

  expect_error(run_twsa_det("xx", param2, params_df, nsamps = 30,
                        strategy_func, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "param1 and param2 should be in the parameters provided in params_all")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(run_twsa_det(param1, param2, params_df, nsamps = 30,
                        strategy_func2, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
})
