#' Create deterministic sensitivity analysis object
#'
#' @param parameters parameter values associated with effectiveness and outcomes.
#' The table must have two columns, with each parameter name in the first column
#' and the associated parameter value in the second column:
#'
#' \tabular{ll}{
#' parameter      \tab value \cr
#' parm1 name     \tab parm1 val1 \cr
#' ...            \tab ... \cr
#' parm2 name     \tab  parm2 val1 \cr
#' ...            \tab ... \cr
#' }
#'
#' @inheritParams make_psa_obj
#'
#' @export
create_dsa_oneway <- function(parameters, effectiveness, strategies,
                              cost = NULL, currency = "$") {
  # argument checking
  cost <- check_df_and_coerce(cost)
  effectiveness <- check_df_and_coerce(effectiveness)
  parameters <- check_df_and_coerce(parameters)

  # parameter names
  colnames(parameters) <- c("parameter", "parmval")
  parnames <- unique(parameters$parameter)

  # argument checks and defining other variables
  # costs, effectiveness, and parameters have same number of rows
  n_sim_costs <- nrow(cost)
  n_sim_effectiveness <- nrow(effectiveness)
  n_sim_parameters <- nrow(parameters)
  if ( (n_sim_costs != n_sim_effectiveness) | (n_sim_parameters != n_sim_costs) ) {
    stop("The cost, effectiveness, and parameter dataframes must all have the same number of rows.")
  }

  # define n_sim (could be any of the three, since they're all equal)
  n_sim <- n_sim_costs

  # costs and effectiveness have same number of columns
  n_strategies_costs <- ncol(cost)
  n_strategies_effectiveness <- ncol(effectiveness)
  if (n_strategies_costs != n_strategies_effectiveness) {
    stop("The number of columns of the cost and benefit matrices is different and must be the same.")
  }
  # define n.strat (could be either n_sim_costs or n_sim_effectiveness)
  n_strategies <- n_strategies_costs

  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n_strategies), seq(1, n_strategies), sep = "")
  } else {
    # make sure strategies is the same length as the number of columns
    if (n_strategies != length(strategies)) {
      stop(
        paste0("The number of columns in the cost and effectiveness",
               "matrices is different from the number of strategies provided"))
    }
  }

  # define cost and effectiveness column names using strategies
  names(cost) <- names(effectiveness) <- strategies
  # define psa as a named list
  dsa <- list("n_strategies" = n_strategies,
              "strategies" = strategies,
              "n_sim" = n_sim,
              "cost" = cost,
              "effectiveness" = effectiveness,
              "parameters" = parameters,
              "parnames" = parnames,
              "n_dsa" = length(parnames),
              "currency" = currency)
  class(dsa) <- "dsa_oneway"
  dsa
}
