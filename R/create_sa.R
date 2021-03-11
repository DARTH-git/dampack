#' A generic sensitivity analysis object
#'
#' @description This function is called by \code{\link{make_psa_obj}},
#' \code{\link{create_dsa_oneway}},
#' and \code{\link{create_dsa_oneway}}, and checks the structure of
#' each of the arguments before creating an SA object.
#'
#' @param parameters a data frame with parameter values for each model run. Each
#' column should represent a different parameter, and each row should represent a
#' simulation (in the same order as \code{cost} and \code{effectiveness})
#' @param parnames names for the parameters.
#' @param cost,effectiveness,other_outcome data frames containing data for costs,
#' effectiveness or another outcome (user-defined), respectively.
#' Each simulation should be a row of the data frame, and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param strategies vector with the names of the strategies. Due to requirements in
#' certain uses of this vector, this function uses \code{\link{make.names}} to modify
#' strategy names as necessary. It is strongly suggested that you follow the rules
#' in the \code{\link{make.names}} help page, to avoid unexpected errors.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
create_sa <- function(parameters, parnames, effectiveness, strategies,
                      cost, currency, other_outcome) {
  # checks that each is a dataframe
  if (!is.null(cost)) {
    cost <- check_df_and_coerce(cost)
  }

  if (!is.null(other_outcome)) {
    other_outcome <- check_df_and_coerce(other_outcome)
  }

  if (!is.null(effectiveness)) {
    effectiveness <- check_df_and_coerce(effectiveness)
  }

  if (!is.null(parameters)) {
    parameters <- check_df_and_coerce(parameters)
  }

  ### argument checks and definitions of other variables ###

  # costs, effectiveness, and parameters have same number of rows
  n_sim_ls <- list(effectiveness, cost, parameters, other_outcome)
  if (length(unique(unlist(lapply(n_sim_ls[!unlist(lapply(n_sim_ls, is.null))], nrow)))) != 1) {
    stop("Among those provided, the cost, effectiveness, parameter,
         and other_outcome dataframes must all have the same number of rows.")
  }

  # define n_sim
  n_sim <- unique(unlist(lapply(n_sim_ls[!unlist(lapply(n_sim_ls, is.null))], nrow)))

  # costs and effectiveness have same number of columns (strategies)
  n_strategies_ls <- list(effectiveness, cost, other_outcome)
  if (length(unique(unlist(lapply(n_strategies_ls[!unlist(lapply(n_strategies_ls, is.null))], ncol)))) != 1) {
    stop("Among those provided, the cost, effectiveness,
         and other_outcome dataframes must all have the same number of columns.")
  }

  # define n_strategies
  n_strategies <- unique(unlist(lapply(n_strategies_ls[!unlist(lapply(n_strategies_ls, is.null))], ncol)))

  # If the strategy names are not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n_strategies), seq(1, n_strategies), sep = "")
  } else {
    # correct strategy names. they are used as data.frame column names and in lm()
    # so they need to be syntactically valid
    new_strategies <- make.names(strategies, unique = TRUE)

    # write warning to console, so user knows that strategy name was changed
    for (i in 1:n_strategies) {
      old_strat <- strategies[i]
      new_strat <- new_strategies[i]
      if (new_strat != old_strat) {
        warning(paste0("strategy name '", old_strat, "' was converted to '", new_strat,
                       "' for compatibility. See ?make.names"), call. = FALSE)
      }
    }
    # update strategies
    strategies <- new_strategies

    # make sure strategies is the same length as the number of columns
    if (n_strategies != length(strategies)) {
      stop(
        paste0("The number of columns in the cost and effectiveness",
               "matrices is different from the number of strategies provided"))
    }
  }

  # define cost and effectiveness column names using strategies
  if (!is.null(cost)) {
    names(cost) <- strategies
  }
  if (!is.null(effectiveness)) {
    names(effectiveness) <- strategies
  }

  # define sa as a named list
  sa <- list("n_strategies" = n_strategies,
              "strategies" = strategies,
              "n_sim" = n_sim,
              "cost" = cost,
              "effectiveness" = effectiveness,
              "other_outcome" = other_outcome,
              "parameters" = parameters,
              "parnames" = parnames,
              "currency" = currency)
  class(sa) <- "sa"
  return(sa)
}

#' print a psa object
#'
#' @param x the psa object
#' @param all_strat whether or not to print the full list of strategies. defaults to FALSE, which truncates
#' the strategy list to 5
#' @param ... further arguments to print (not used)
#'
#' @export
print.sa <- function(x, all_strat = FALSE, ...) {
  xclass <- class(x)
  is_ow_dsa <- "dsa_oneway" %in% xclass
  is_tw_dsa <- "dsa_twoway" %in% xclass
  is_psa <- "psa" %in% xclass
  cat("\n")
  if (is_ow_dsa) {
    cat("One-way Deterministic SA Object", "\n")
  }
  if (is_tw_dsa) {
    cat("Two-way Deterministic SA Object", "\n")
  }
  if (is_psa) {
    cat("PSA object", "\n")
  }
  cat("-------------------------------------------------", "\n")

  # cost
  cat("number of strategies (n_strategies):", x$n_strategies, "\n")
  n_trunc <- 5
  if (all_strat | (x$n_strategies <= n_trunc)) {
    s2print <- x$strategies
    msg <- ""
  } else {
    s2print <- c(x$strategies[1:n_trunc], "...")
    msg <- paste("(truncated at", n_trunc, ")")
  }
  s_collapsed <- paste(s2print, collapse = ", ")
  cat("strategies:", s_collapsed, msg, "\n")
  if (is_psa) {
    cat("number of simulations (n_sim):", x$n_sim, "\n")
  }
  cat("cost: a data frame with", nrow(x$cost), "rows and", ncol(x$cost), "columns.", "\n")
  cat("effectiveness: a data frame with",
      nrow(x$effectiveness), "rows and",
      ncol(x$effectiveness), "columns.", "\n")
  cat("parameters: a data frame with",
      nrow(x$parameters), "rows and",
      ncol(x$parameters), "columns", "\n")
  cat("parameter names (parnames): ", paste(x$parnames, collapse = ", "), "\n")
  cat("currency:", x$currency, "\n")
}
