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
#' @param strategies vector with the names of the strategies, in the same order as
#' the columns of the outcome data frames. Names are kept exactly as provided, so
#' they can contain spaces and other characters (for example "Standard of care"),
#' and are used as labels in tables and plots. They must be unique and not missing
#' or empty. If \code{NULL}, strategies are named \code{Strategy_1},
#' \code{Strategy_2}, and so on.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#' @return returns "sa" sensitivity analysis object.
#' @keywords internal
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
  l_n_sim <- list(effectiveness, cost, parameters, other_outcome)
  if (length(unique(unlist(lapply(l_n_sim[!unlist(lapply(l_n_sim, is.null))], nrow)))) != 1) {
    stop("Among those provided, the cost, effectiveness, parameter,
         and other_outcome dataframes must all have the same number of rows.")
  }

  # define n_sim
  n_sim <- unique(unlist(lapply(l_n_sim[!unlist(lapply(l_n_sim, is.null))], nrow)))

  # costs and effectiveness have same number of columns (strategies)
  l_n_strategies <- list(effectiveness, cost, other_outcome)
  if (length(unique(unlist(lapply(l_n_strategies[!unlist(lapply(l_n_strategies, is.null))], ncol)))) != 1) {
    stop("Among those provided, the cost, effectiveness,
         and other_outcome dataframes must all have the same number of columns.")
  }

  # define n_strategies
  n_strategies <- unique(unlist(lapply(l_n_strategies[!unlist(lapply(l_n_strategies, is.null))], ncol)))

  # If the strategy names are not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n_strategies), seq(1, n_strategies), sep = "")
  } else {
    # strategy names are kept exactly as provided (they may contain spaces and
    # other characters); they only need to be complete and unique
    strategies <- check_strategy_names(strategies, n_strategies)
  }

  # define cost and effectiveness column names using strategies
  if (!is.null(cost)) {
    names(cost) <- strategies
  }
  if (!is.null(effectiveness)) {
    names(effectiveness) <- strategies
  }
  if (!is.null(other_outcome)) {
    names(other_outcome) <- strategies
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
  sa
}

#' print a psa object
#'
#' @param x the psa object
#' @param all_strat whether or not to print the full list of strategies. defaults to FALSE, which truncates
#' the strategy list to 5
#' @param ... further arguments to print (not used)
#'
#' @return None (invisible NULL).
#'
#' @examples
#' data("psa_cdiff")
#'
#' # the list of strategies is truncated at 5
#' print(psa_cdiff)
#'
#' # print all of them
#' print(psa_cdiff, all_strat = TRUE)
#' @export
print.sa <- function(x, all_strat = FALSE, ...) {
  v_xclass <- class(x)
  is_ow_dsa <- "dsa_oneway" %in% v_xclass
  is_tw_dsa <- "dsa_twoway" %in% v_xclass
  is_psa <- "psa" %in% v_xclass
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
  if (all_strat || (x$n_strategies <= n_trunc)) {
    v_s2print <- x$strategies
    msg <- ""
  } else {
    v_s2print <- c(x$strategies[1:n_trunc], "...")
    msg <- paste("(truncated at", n_trunc, ")")
  }
  s_collapsed <- paste(v_s2print, collapse = ", ")
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

#' Check strategy names
#'
#' Strategy names are used as labels in tables and plots, so they are kept
#' exactly as provided. They must be complete and unique, with one name per
#' strategy.
#'
#' @param strategies vector of strategy names.
#' @param n_strategies number of strategies (columns) in the outcome data.
#' @return \code{strategies} as a character vector.
#' @keywords internal
check_strategy_names <- function(strategies, n_strategies) {
  strategies <- as.character(strategies)
  if (length(strategies) != n_strategies) {
    stop(n_strategies, " strategy names are needed (one per column of the outcome data), but ",
         length(strategies), " were provided.", call. = FALSE)
  }
  if (anyNA(strategies) || any(!nzchar(trimws(strategies)))) {
    stop("Strategy names cannot be missing or empty.", call. = FALSE)
  }
  if (anyDuplicated(strategies)) {
    stop("Strategy names must be unique. Duplicated: ",
         paste(unique(strategies[duplicated(strategies)]), collapse = ", "), call. = FALSE)
  }
  strategies
}
