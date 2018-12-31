#' Find the CEA frontier, up to a given WTP level, by
#' identifying strategies with the highest NMB
#'
#' @details
#' Originally written by Sze Suen on Feb 25, 2015.
#' Modified by Fernando Alarid-Escudero and Caleb Easterly.
#'
#' If code is used, please cite the below paper:
#'
#' Suen S-C, Goldhaber-Fiebert JD. An Efficient,
#           Noniterative Method of Identifying the Cost-Effectiveness Frontier.
#           Med Decis Making. 2016
#           https://www.ncbi.nlm.nih.gov/pubmed/25926282
#'
#' @export
get_ce_frontier <- function(ce_df, max_wtp = Inf){
  # Notes:
  #    ~Frontier strategies are displaced on the R output screen and
  #      plotted in red on the scatter plot.
  #
  #    ~User needs to provide a csv file of costs and QALYs
  #	    (CostQalyInputFile_online_supp.csv) inside the folder specified
  #	    below (inputFolder). The CSV should have three columns (labeled
  #     in first row) in this order:
  #      Strategy number, costs, and QALYs.
  #
  #    ~User can specify the maximum willingness-to-pay level to
  #      consider (max_wtp).  Can be Inf for infinity.
  #
  #    ~QALY-reducing strategies will be on the frontier if they save
  #      enough money (script assumes maximum willingness to save money
  #      per QALY lost is equivalent to maximum willingness to pay per QALY
  #      gained). If the user does not wish to consider such policies as
  #      being on the frontier, do not include strategies with negative
  #      QALYs in the input csv file.
  #
  #    ~Script does not use the one-line code cited in the text
  #      as the max function is slow. This implementation is
  #      faster and methodologically does the same thing.
  #
  #    ~May take a few minutes if thousands of strategies and
  #       processing resources are low.  Please be patient.
  #
  #    Please cite article if this code is used.
  #
  # USER INPUTS:
  #inputFolder <- "CostEffectivenessFrontier_MDM/"
  #max_wtp <- Inf        # any positive value or Inf

  ## Clean everythng from workspace
  #rm(list=ls())
  ####################################################################
  ####################################################################

  # check for duplicated strategies
  dups <- ce_df[c(duplicated(ce_df[, 2:3]) | duplicated(ce_df[, 2:3], fromLast = TRUE)), 1]

  # initialize some variables
  costsCol <- 2; qalyCol <- 3
  numStrat <- nrow(ce_df)

  # find WTP levels to test so that all strategies on frontier will be captured
  # this means testing on either side of all NMB intersections, which are just all the pairwise ICERs
  ICERmat <- matrix(1, numStrat, numStrat)
  suppressWarnings(
    for (i in 1:numStrat) {
      indexStrat <- matrix(1, numStrat, 3)
      indexStrat[, costsCol] <- indexStrat[, costsCol] * ce_df[i, costsCol]
      indexStrat[, qalyCol] <- indexStrat[, qalyCol] * ce_df[i, qalyCol]
      delCostQalys <- ce_df - indexStrat
      ICERmat[, i] <- delCostQalys[, costsCol] / delCostQalys[, qalyCol]
    }
  )
  intersections <- sort(unique(c(ICERmat)))
  intersections <- intersections[is.finite(intersections)]
  WTPtestPoints <- c(0, intersections [intersections >= 0 & intersections <= max_wtp], max_wtp)

  # Find the strategy with the max NMB at each of the WTP test points
  indiciesOfMax <- vector()
  NMBmat <- matrix(0, numStrat, length(WTPtestPoints))
  for (i in 1:length(WTPtestPoints) ) {
    NMBmat[, i] <- (WTPtestPoints[i] * ce_df[, qalyCol]) - ce_df[, costsCol]
  }
  if (is.infinite(max_wtp)) {
    #WTP of infinity means costs are not considered
    NMBmat[, length(WTPtestPoints)] = ce_df[, qalyCol] - (0 * ce_df[, costsCol]);
  }
  maxVals <- apply(NMBmat, 2, max)  #find strategy that maximizes NMB at each WTP
  for (i in 1:length(WTPtestPoints) ) {  #find all strategies that match max at each WTP
    indiciesOfMax <- c(indiciesOfMax, which( NMBmat[, i] == maxVals[i]))
  }
  v.frontier <- unique(indiciesOfMax)  #find strategy that maximizes NMB at each WTP

  df.frontier <- cbind(ce_df, `On Frontier` = 0)
  df.frontier$`On Frontier`[v.frontier] <- 1

  sprintf("Frontier is formed by strategies: %s", paste(sort(ce_df[v.frontier, 1]), collapse = ", "))

  class(df.frontier) <- c("frontier", "data.frame")
  return(df.frontier)
}

#' @importFrom scales comma
#' @export
plot_frontier <- function(df.frontier,
                          ncol = 1,
                          coord.flip = F,
                          txtsize = 16)
{
  # A function to plot CE frontier
  # USER INPUTS:
  #   ce_df: A CE matrix arranged as: Col1: Strategy; Col2: Cost; Col3: Effectiveness
  # Create a dataframe from matrix
  colnames(df.frontier) <- c("Strategy", "Cost", "Effectiveness", "Frontier")
  n.strategies <- nrow(df.frontier)
  # Make Strategies as factor
  df.frontier$Strategy <- as.factor(df.frontier$Strategy)
  #
  if (coord.flip == T) {
    ggplot(df.frontier, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) +
      coord_flip() +
      scale_y_continuous("Cost ($)", labels = comma) +
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = subset(df.frontier, Frontier == 1),
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = subset(df.frontier, Frontier == 1), aes(Effectiveness, Cost)) +
      scale_shape_manual(values = 0:(n.strategies - 1)) +
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw(base_size = txtsize) +
      theme(legend.position = "bottom")
  } else {
    ggplot(df.frontier, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) +
      scale_y_continuous("Cost ($)", labels = comma) +
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = subset(df.frontier, Frontier == 1),
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = subset(df.frontier, Frontier == 1), aes(Effectiveness, Cost)) +
      scale_shape_manual(values = 0:(n.strategies - 1)) +
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw(base_size = txtsize) +
      theme(legend.position = "bottom")
  }
}

# ### Test functions
# library(ggplot2)
# library(scales)
# data("hund_strat")
#
# df.ce.frontier <- getFrontier(hund_strat)
# plot(df.ce.frontier, ncol = 10) +
#   theme(legend.position = "bottom")
