#' An object to hold PSA results.
#'
#' @param cost Matrix with the cost for each simulation (rows) and strategy (columns).
#' @param effectiveness Matrix with the effectiveness for each simulation (rows) and strategy (columns)
#' @param strategies String vector with the name of the strategies
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
#' @export
make_psa_obj <- function(cost, effectiveness, strategies=NULL, currency = "$"){
  # argument checking
  orig_cost_name <- deparse(substitute(cost))
  orig_eff_name <- deparse(substitute(effectiveness))
  cost <- check_df_and_coerce(cost, orig_cost_name)
  effectiveness <- check_df_and_coerce(effectiveness, orig_eff_name)

  # argument checks and defining other variables
  # costs and effectiveness have same number of rows
  n.sim.costs <- nrow(cost)
  n.sim.effectiveness <- nrow(effectiveness)
  if (n.sim.costs != n.sim.effectiveness) {
    stop('The number of rows of the cost and benefit matrices is different and must be the same.')
  }
  # define n.sim (could be either n.sim.costs or n.sim.effectiveness)
  n.sim <- n.sim.costs

  # costs and effectiveness have same number of columns
  n.strategies.costs <- ncol(cost)
  n.strategies.effectiveness <- ncol(effectiveness)
  if (n.strategies.costs != n.strategies.effectiveness) {
    stop('The number of columns of the cost and benefit matrices is different and must be the same.')
  }
  # define n.strat (could be either n.sim.costs or n.sim.effectiveness)
  n.strategies <- n.strategies.costs

  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n.strategies), seq(1, n.strategies), sep = "")
  } else {
    # make sure strategies is the same length as the number of columns
    if (n.strategies != length(strategies)) {
      stop('The number of columns in the cost and effectiveness matrices is different from the number of strategies provided')
    }
  }
  # define cost and effectiveness column names using strategies
  names(cost) <- names(effectiveness) <- strategies
  # define psa as a named list
  psa_obj <- list("n.strategies" = n.strategies,
                  "strategies" = strategies,
                  "n.sim" = n.sim,
                  "cost" = cost,
                  "effectiveness" = effectiveness,
                  "currency" = currency)
  class(psa_obj) <- "psa"
  return(psa_obj)
}

check_psa_object <- function(psa) {
  if (!inherits(psa, "psa")) {
    stop(paste0("The psa results parameter must be an object of class `psa`.\n",
                "Please run the make_psa() function to create this object."))
  }
}

check_df_and_coerce <- function(obj, obj_name) {
  if (!inherits(obj, "data.frame")) {
    warning(paste0("\'", obj_name, "\'", " is not a data frame. coercing to data frame"))
    df <- as.data.frame(obj)
  } else {
    df <- obj
  }
  return(df)
}
