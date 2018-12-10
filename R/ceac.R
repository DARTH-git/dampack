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
#' cost-effective (optimal?) at each \code{wtp} threshold.
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
  } else {
    # make sure strategies is the same length as the number of columns
    if (n.strategies != length(strategies)) {
      stop('The number of columns in the cost and effectiveness matrices is different from the number of strategies provided')
    }
  }

  # number of willingness to pay thresholds
  n.wtps <- length(wtp)

  # matrices to store probability optimal for each strategy (cea)
  cea <- matrix(0, nrow = n.wtps, ncol = n.strategies)
  colnames(cea) <- strategies

  # vector to store strategy at the cost-effectiveness acceptability frontier
  frontv <- rep(0, n.wtps)

  for (l in 1:length(wtp)) {
    nhb <-  effectiveness - costs/wtp[l] # Effectiveness minus Costs, with vector indexing
    # find best strategy for each simulation
    max.nhb <- max.col(nhb)
    opt <- table(max.nhb)
    cea[l, as.numeric(names(opt))] <- opt/n.sim

    # calculate point on CEAF
    # the strategy with the highest expected NHB
    frontv[l] <- which.max(colMeans(nhb))
  }

  # make cea df
  cea.df <- data.frame(wtp, cea, strategies[frontv], stringsAsFactors = FALSE)
  colnames(cea.df) <- c("WTP", strategies, "fstrat")

  # make ceaf df

  ceac <- reshape2::melt(cea.df, id.vars = c("WTP", "fstrat"),
               variable.name = "Strategy", value.name = "Proportion")

  # boolean for on frontier or not
  ceac$On_Frontier <- (ceac$fstrat == ceac$Strategy)

  # drop fstrat column
  ceac$fstrat <- NULL

  # replace factors with strings
  ceac$Strategy <- as.character(ceac$Strategy)

  # order by WTP
  ceac <- ceac[order(ceac$WTP), ]

  # Return a data frame of class ceac
  class(ceac) <- c("ceac", "data.frame")
  return(ceac)
}
