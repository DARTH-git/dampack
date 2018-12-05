#' Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param costs Matrix with the costs for each simulation (rows) and strategy (columns).
#' @param effectiveness Matrix with the effectiveness for each simulation (rows) and strategy (columns)
#' @param strategies String vector with the name of the strategies
#' @keywords cost-effectiveness acceptability curves
#' @section Details:
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} threshold.
#' @return ceac A melted data frame with each strategy's probability of being
#' cost-effective at each WTP threshold.
#' @import reshape2
#'
#' @export
ceac <- function(wtp, costs, effectiveness, strategies = NULL){
  # argument checks and defining other variables
  # costs and effectiveness have same number of rows
  n.sim.costs <- nrow(costs)
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
  }
  # Matrix to store NHB for each strategy
  NHB <- array(0, dim = c(n.sim, n.strategies))
  colnames(NHB) <- strategies
  cea <- array(0, dim = c(length(wtp), n.strategies))

  for (l in 1:length(wtp)) {
    NHB <-  effectiveness - costs/wtp[l] # Effectiveness minus Costs, with vector indexing
    # find best strategy for each simulation
    Max.NHB <- max.col(NHB)
    opt <- table(Max.NHB)
    cea[l, as.numeric(names(opt))] <- opt/n.sim
  }
  cea <- data.frame(cbind(wtp, cea))
  colnames(cea) <- c("WTP", strategies)

  ceac <- melt(cea, id.vars = "WTP", variable.name = "Strategy")
  # Return a data frame of class ceac
  class(ceac) <- c("data.frame", "ceac")
  return(ceac)
}
