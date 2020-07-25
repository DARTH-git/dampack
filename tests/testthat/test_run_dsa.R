context("run_dsa")
library(dampack)

source("util_models_detfun.R")

state_name <- c("S", "IS", "IA", "T")
cycle <- 100
popsize <- 1000

params_basecase <- list(p_inf = 0.05, p_asymp = 0.8, p_test_symp = 0.8, p_test_asymp = 0.01, p_rec = 0.01)

params_range <- data.frame(pars = c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec"),
                          min = c(0, 0.5, 0.5, 0, 0),
                          max = c(0.5, 0.9, 1, 0.2, 0.5))
outcomes <- c("tot_incidence")

# testing run_owsa_det
test_that("correct input in run_owsa_det", {
  expect_silent(run_owsa_det(params_range, params_basecase, nsamp = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))

  outcomes_2 <- "prev"
  expect_silent(run_owsa_det(params_range, params_basecase, nsamp = 10,
                         strategy_func, outcomes_2,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))


  temp_range <- params_range %>% arrange(pars)
  expect_silent(run_owsa_det(temp_range, params_basecase, nsamp = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))

  temp_range <- params_range[-1, ]
  expect_silent(run_owsa_det(temp_range, params_basecase, nsamp = 10,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))

  strategies <- c("redInf", "incScr")
  expect_silent(run_owsa_det(params_range, params_basecase, nsamp = 10,
                         strategy_func, outcomes,
                         strategies = strategies,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))
})


test_that("check output of run_owsa_det", {
  o <- run_owsa_det(params_range, params_basecase, nsamp = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle,
                progress = FALSE)
  expect_is(o, "owsa")
  expect_is(o, "data.frame")

  params_range2 <- params_range[-2, ]
  o <- run_owsa_det(params_range2, params_basecase, nsamp = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle,
                progress = FALSE)
  expect_equal(unique(as.character(o$parameter)), as.character(params_range2[, 1]))


  o <- run_owsa_det(params_range2, params_basecase, nsamp = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle,
                progress = FALSE)

  min_val <- o %>%
    group_by(parameter) %>%
    summarise(min_val = min(param_val))
  expect_equal(min_val$min_val,
               params_range2[order(match(params_range2$pars, min_val$parameter)), "min"])

  max_val <- o %>%
    group_by(parameter) %>%
    summarise(max_val = max(param_val))
  expect_equal(max_val$max_val,
               params_range2[order(match(params_range2$pars, max_val$parameter)), "max"])
})


test_that("check output from run_owsa_det with other dampack functions", {
  o <- run_owsa_det(params_range, params_basecase, nsamp = 10,
                strategy_func, outcomes,
                strategies = NULL,
                state_name = state_name, cycle = cycle,
                progress = FALSE)
  g <- plot(o)
  expect_is(g, "ggplot")
  expect_silent(g)
})

test_that("accurately producing warnings and errors in run_owsa_det", {
  temp_params_range <- params_range
  temp_params_range[, 1] <- c("xx", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec")
  expect_error(run_owsa_det(temp_params_range, params_basecase, nsamp = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "the first column of params_range should consist only of parameter names from params_basecase")

  params_range2 <- as.matrix(params_range)
  expect_error(run_owsa_det(params_range2, params_basecase, nsamp = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "params_range must be a data.frame")

  params_range2 <- params_range
  params_range2[, 2] <- params_range[, 3]
  params_range2[, 3] <- params_range[, 2]
  expect_error(run_owsa_det(params_range2, params_basecase, nsamp = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "basecase has to be in between min and max")

  params_range2 <- params_range
  params_range2[, c(2, 3)] <- as.character(params_range2[, c(2, 3)])
  expect_error(run_owsa_det(params_range2, params_basecase, nsamp = 10,
                        strategy_func, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "min and max in params_range and elements of params_basecase must be numeric")

  params_basecase2 <- list(0.05, 0.8, 0.8, 0.01, 0.01)
  expect_error(run_owsa_det(params_range, params_basecase2, nsamp = 10,
                            strategy_func, outcomes,
                            strategies = NULL,
                            state_name = state_name, cycle = cycle,
                            progress = FALSE),
               "params_basecase must be a named list")

  expect_error(run_owsa_det(params_range, params_basecase, nsamp = 10,
                        strategy_func, outcomes = "treated",
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "at least one outcome is not in FUN outcomes")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(run_owsa_det(params_range, params_basecase, nsamp = 10,
                        strategy_func2, outcomes,
                        strategies = NULL,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")

  tmp_strategy <- c("redInf", "incScr", "incCov")
  expect_error(run_owsa_det(params_range, params_basecase, nsamp = 10,
                        strategy_func, outcomes = "treated",
                        strategies = tmp_strategy,
                        state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "number of strategies is not the same as the number of strategies in user defined FUN")
})

# testing run_twsa_det
params_range <- data.frame(pars = c("p_inf", "p_test_asymp"),
                          min = c(0, 0),
                          max = c(0.5, 0.2))

test_that("check input in run_twsa_det", {
  expect_silent(run_twsa_det(params_range, params_basecase, nsamp = 30,
                         strategy_func, outcomes,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))

  outcomes_2 <- "prev"
  expect_silent(run_twsa_det(params_range, params_basecase, nsamp = 30,
                         strategy_func, outcomes_2,
                         strategies = NULL,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))

  strategies <- c("redInf", "incScr")
  expect_silent(run_twsa_det(params_range, params_basecase, nsamp = 30,
                         strategy_func, outcomes,
                         strategies = strategies,
                         state_name = state_name, cycle = cycle,
                         progress = FALSE))
})

test_that("check output from run_twsa_det", {
  strategy <- c("s1", "s2")
  tw <- run_twsa_det(params_range, params_basecase, nsamp = 30,
                 strategy_func, outcomes,
                 strategies = strategy,
                 state_name = state_name, cycle = cycle,
                 progress = FALSE)
  expect_is(tw, "twsa")
  expect_is(tw, "data.frame")

  expect_equal(min(tw[, params_range[1, 1]]), params_range[1, ]$min)
  expect_equal(max(tw[, params_range[1, 1]]), params_range[1, ]$max)
  expect_equal(min(tw[, params_range[2, 1]]), params_range[2, ]$min)
  expect_equal(max(tw[, params_range[2, 1]]), params_range[2, ]$max)
  expect_equal(unique(as.character(tw[, "strategy"])), strategy)
})

test_that("checking warning error message from run_twsa_det", {

  params_range2 <- data.frame(pars = c("p_inf", "p_asymp", "p_test_symp", "p_test_asymp", "p_rec"),
                            min = c(0, 0.5, 0.5, 0, 0),
                            max = c(0.5, 0.9, 1, 0.2, 0.5))
  expect_error(run_twsa_det(params_range2, params_basecase, nsamp = 30,
                        strategy_func, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")

  params_range2 <- data.frame(pars = c("xx", "p_asymp"),
                            min = c(0, 0.5),
                            max = c(0.5, 0.9))
  expect_error(run_twsa_det(params_range2, params_basecase, nsamp = 30,
                        strategy_func, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "the first column of params_range should consist only of parameter names from params_basecase")

  strategy_func2 <- function(param, state_name, cycle,
                             mystrategy = NULL, popsize = 1000,
                             init_state = c(1, 0, 0, 0)) {
    input_ls <- as.list(environment())
    output <- do.call(strategy_func, input_ls)
    return(as.matrix(output))
  }
  expect_error(run_twsa_det(params_range, params_basecase, nsamp = 30,
                        strategy_func2, outcomes,
                        strategies = NULL, state_name = state_name, cycle = cycle,
                        progress = FALSE),
               "FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
})
