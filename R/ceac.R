#' Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param outcomes Matrix with the model outputs. The outcomes must be ordered
#' in such a way that for each strategy the cost must appear first then the
#' effectiveness.
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
ceac <- function(wtp, outcomes, strategies = NULL){
  # todo:
  # check that outcomes has even number of columns
  # check that strategies has length ncol(outcomes)
  # better to provide effectiveness and cost separately? then we can index by sim
  # Create scalar with number of simulations
  n.sim <- nrow(outcomes)
  # Create scalar with number of strategies (i.e. number of columns of
  # `outcomes` divided by two)
  n.strategies <- ncol(outcomes)/2
  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n.strategies), seq(1, n.strategies), sep = "")
  }
  # Matrix to store NHB for each strategy
  NHB <- array(0, dim = c(n.sim, n.strategies))
  colnames(NHB) <- strategies
  cea <- array(0, dim = c(length(wtp), n.strategies))
  # Vector to index costs
  costInd <- seq(1, 2*n.strategies, by = 2)
  # Vector to index effectiveness
  effInd  <- seq(2, 2*n.strategies, by = 2)

  for (l in 1:length(wtp)) {
    NHB <-  outcomes[, effInd] - outcomes[, costInd]/wtp[l] # Effectiveness minus Costs, with vector indexing
    Max.NHB <- max.col(NHB)
    opt <- table(Max.NHB)
    cea[l, as.numeric(names(opt))] <- opt/n.sim
  }
  cea <- data.frame(cbind(wtp, cea))
  colnames(cea) <- c("WTP", strategies)

  ceac <- melt(cea, id.vars = "WTP")
  colnames(ceac)[2] <- "Strategy"
  # Return a data frame of class ceac
  class(ceac) <- "ceac"
  return(ceac)
}
