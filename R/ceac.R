#' Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @keywords cost-effectiveness acceptability curves
#' @section Details:
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective (optimal?) at each \code{wtp} threshold.
#' @return ceac A melted data frame with each strategy's probability of being
#' cost-effective at each WTP threshold.
#' @import reshape2
#'
#' @export
ceac <- function(wtp, psa_results){
  # check that psa_results has class 'psa'
  if (!inherits(psa_results, "psa_results")) {
    stop(paste0("The psa results parameter must be an object of class `psa_results`.\n",
                "Please run the psa_results() function to create this object."))
  }
  strategies <- psa_results$strategies
  n.strategies <- psa_results$n.strategies
  effectiveness <- psa_results$effectiveness
  cost <- psa_results$cost
  n.sim <- psa_results$n.sim

  # number of willingness to pay thresholds
  n.wtps <- length(wtp)

  # matrices to store probability optimal for each strategy (cea)
  cea <- matrix(0, nrow = n.wtps, ncol = n.strategies)
  colnames(cea) <- strategies

  # vector to store strategy at the cost-effectiveness acceptability frontier
  frontv <- rep(0, n.wtps)

  for (l in 1:length(wtp)) {
    nhb <-  effectiveness - cost/wtp[l] # Effectiveness minus cost, with vector indexing
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
