#' R functions to compute and plot CE Frontier ####
#' @export
getFrontier <- function(CEmat, maxWTP = Inf, plot = TRUE){
  # Name: getFrontier.R
  # Goal: Find the CEA frontier, up to a given WTP level, by
  #       identifying strategies with the highest NMB
  # Originally written by: Sze Suen on Feb 25, 2015
  # Citation: Suen S-C, Goldhaber-Fiebert JD. An Efficient,
  #           Noniterative Method of Identifying the Cost-Effectiveness Frontier.
  #           Med Decis Making. 2016
  #           https://www.ncbi.nlm.nih.gov/pubmed/25926282
  # Modified by: Fernando Alarid-Escudero on July 20, 2015
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
  #      consider (maxWTP).  Can be Inf for infinity.
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
  #maxWTP <- Inf        # any positive value or Inf

  ## Clean everythng from workspace
  #rm(list=ls())
  ####################################################################
  ####################################################################

  # check for duplicated strategies
  dups <- CEmat[c(duplicated(CEmat[, 2:3]) | duplicated(CEmat[, 2:3], fromLast = TRUE)), 1]

  # initialize some variables
  costsCol <- 2; qalyCol <- 3
  numStrat <- nrow(CEmat)

  # find WTP levels to test so that all strategies on frontier will be captured
  # this means testing on either side of all NMB intersections, which are just all the pairwise ICERs
  ICERmat <- matrix(1, numStrat, numStrat)
  suppressWarnings(
    for (i in 1:numStrat) {
      indexStrat <- matrix(1, numStrat, 3)
      indexStrat[, costsCol] <- indexStrat[, costsCol] * CEmat[i, costsCol]
      indexStrat[, qalyCol] <- indexStrat[, qalyCol] * CEmat[i, qalyCol]
      delCostQalys <- CEmat - indexStrat
      ICERmat[, i] <- delCostQalys[, costsCol] / delCostQalys[, qalyCol]
    }
  )
  intersections <- sort(unique(c(ICERmat)))
  intersections <- intersections[is.finite(intersections)]
  WTPtestPoints <- c(0, intersections [intersections >= 0 & intersections <= maxWTP], maxWTP)

  # Find the strategy with the max NMB at each of the WTP test points
  indiciesOfMax <- vector()
  NMBmat <- matrix(0, numStrat, length(WTPtestPoints))
  for (i in 1:length(WTPtestPoints) ) {
    NMBmat[, i] <- (WTPtestPoints[i] * CEmat[, qalyCol]) - CEmat[, costsCol]
  }
  if (is.infinite(maxWTP)) {
    #WTP of infinity means costs are not considered
    NMBmat[, length(WTPtestPoints)] = CEmat[, qalyCol] - (0 * CEmat[, costsCol]);
  }
  maxVals <- apply(NMBmat, 2, max)  #find strategy that maximizes NMB at each WTP
  for (i in 1:length(WTPtestPoints) ) {  #find all strategies that match max at each WTP
    indiciesOfMax <- c(indiciesOfMax, which( NMBmat[, i] == maxVals[i]))
  }
  v.frontier <- unique(indiciesOfMax)  #find strategy that maximizes NMB at each WTP

  df.frontier <- cbind(CEmat, `On Frontier` = 0)
  df.frontier$`On Frontier`[v.frontier] <- 1

  sprintf("Frontier is formed by strategies: %s", paste(sort(CEmat[v.frontier, 1]), collapse = ", "))

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
  #   CEmat: A CE matrix arranged as: Col1: Strategy; Col2: Cost; Col3: Effectiveness
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
