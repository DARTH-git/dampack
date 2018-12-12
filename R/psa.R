#' An object to hold PSA results.
#'
#' @param cost Matrix with the cost for each simulation (rows) and strategy (columns).
#' @param effectiveness Matrix with the effectiveness for each simulation (rows) and strategy (columns)
#' @param strategies String vector with the name of the strategies
#' @export
psa <- function(cost, effectiveness, strategies=NULL){
  # argument checking
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
  n.strategies.costs <- ncol(costs)
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

  # define psa as a named list
  psa_obj <- list("cost" = cost,
                  "effectiveness" = effectiveness,
                  "strategies" = strategies,
                  "n.strategies" = n.strategies,
                  "n.sim" = n.sim)
  class(psa_obj) <- "psa"
  return(psa_obj)
}

