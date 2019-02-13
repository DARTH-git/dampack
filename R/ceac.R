#### object ####

#' Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa A psa object from \code{psa}
#' @keywords cost-effectiveness acceptability curves
#' @section Details:
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective (optimal?) at each \code{wtp} threshold.
#' @return ceac A melted data frame with each strategy's probability of being
#' cost-effective at each WTP threshold.
#' @import reshape2
#'
#' @export
ceac <- function(wtp, psa){
  # check that psa has class 'psa'
  check_psa_object(psa)

  # define needed variables
  strategies <- psa$strategies
  n_strategies <- psa$n_strategies
  effectiveness <- psa$effectiveness
  cost <- psa$cost
  n_sim <- psa$n_sim

  # number of willingness to pay thresholds
  n_wtps <- length(wtp)

  # matrices to store probability optimal for each strategy (cea)
  cea <- matrix(0, nrow = n_wtps, ncol = n_strategies)
  colnames(cea) <- strategies

  # vector to store strategy at the cost-effectiveness acceptability frontier
  frontv <- rep(0, n_wtps)

  for (l in 1:length(wtp)) {
    nmb <-  wtp[l] * effectiveness - cost # net monetary benefit at wtp[l]
    # find best strategy for each simulation
    max.nmb <- max.col(nmb)
    opt <- table(max.nmb)
    cea[l, as.numeric(names(opt))] <- opt / n_sim

    # calculate point on CEAF
    # the strategy with the highest expected nmb
    frontv[l] <- which.max(colMeans(nmb))
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

  # remove rownames
  rownames(ceac) <- NULL

  # Return a data frame of class ceac
  class(ceac) <- c("ceac", "data.frame")
  return(ceac)
}

#### methods ####

#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param x Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @param frontier Whether to plot acceptability frontier
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#'Defaults to \code{$}, but can be any currency symbol or word (e.g., £, €, peso)
#' @param min_prob minimum probability to show strategy in plot.
#' For example, if the min_prob is 0.05, only strategies that ever
#' exceed Pr(Cost Effective) = 0.05 will be plotted. Most useful in situations
#' with many strategies.
#' @inheritParams add_common_aes
#'
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return ceac.gg A \code{ggplot2} object with the CEAC
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot.ceac <- function(x,
                      frontier = TRUE,
                      currency = "$",
                      min_prob = 0,
                      txtsize = 12,
                      n_x_ticks = 10,
                      n_y_ticks = 8,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      ylim = NULL,
                      xlim = NULL,
                      col = c("full", "bw"),
                      ...){
  wtp_name <- "WTP"
  prop_name <- "Proportion"
  strat_name <- "Strategy"
  x$WTP_thou <- x[, wtp_name] / 1000

  # removing strategies with probabilities always below `min_prob`
  # get group-wise max probability
  if (min_prob > 0) {
    max_prob <- x %>%
      group_by(.data$Strategy) %>%
      summarize(maxpr = max(.data$Proportion)) %>%
      filter(.data$maxpr >= min_prob)
    strat_to_keep <- max_prob$Strategy
    if (length(strat_to_keep) == 0) {
      stop(
        paste("no strategies remaining. you may want to lower your min_prob value (currently ",
              min_prob, ")", sep = "")
      )
    }
    # report filtered out strategies
    old_strat <- unique(x$Strategy)
    diff_strat <- setdiff(old_strat, strat_to_keep)
    n_diff_strat <- length(diff_strat)
    if (n_diff_strat > 0) {
      # report strategies filtered out
      cat("filtered out ", n_diff_strat, " strategies with max prob below ", min_prob, ":\n",
          paste(diff_strat, collapse = ","), "\n", sep = "")

      # report if any filtered strategies are on the frontier
      df_filt <- filter(x, .data$Strategy %in% diff_strat & .data$On_Frontier)
      if (nrow(df_filt) > 0) {
        cat(paste0("WARNING - some strategies that were filtered out are on the frontier:\n",
                   paste(unique(df_filt$Strategy), collapse = ","), "\n"))
      }
    }

    # filter dataframe
    x <- filter(x, .data$Strategy %in% strat_to_keep)
  }
  p <- ggplot(data = x, aes_(x = as.name("WTP_thou"),
                             y = as.name(prop_name),
                             color = as.name(strat_name),
                             shape = as.name(strat_name))) +
    geom_point() +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, " / QALY)", sep = "")) +
    ylab("Pr Cost-Effective")
  if (frontier) {
    front <- x[x$On_Frontier, ]
    p <- p + geom_point(data = front, aes_(x = as.name("WTP_thou"),
                                           y = as.name(prop_name)),
                        shape = 0, size = 3, stroke = 1.2, color = "black")
  }
  col <- match.arg(col)
  add_common_aes(p, txtsize, col = col, col_aes = "color",
                 continuous = c("x", "y"), n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 ylim = ylim, xlim = xlim)
}


#' summary function for the ceac object
#'
#' @param object object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @export
summary.ceac <- function(object, ...){
  front <- object[object$On_Frontier == TRUE, ]
  wtp <- front$WTP
  wtp_range <- range(wtp)
  n.wtps <- length(wtp)

  # get the indices where the CE strategy isn't the same as the following CE strategy
  strat_on_front <- front$Strategy
  lagged_strat <- c(strat_on_front[-1], strat_on_front[n.wtps])
  switches <- which(strat_on_front != lagged_strat) + 1
  n_switches <- length(switches)
  # strat_on_front[switches] are the optimal strategies at wtp[switches]
  if (n_switches == 0) {
    wtp_min <- wtp_range[1]
    wtp_max <- wtp_range[2]
    one_strat <- unique(front$Strategy)
    sum_df <- data.frame(wtp_min,
                         wtp_max,
                         one_strat)
  } else {
    # build up summary data frame
    sum_df <- NULL
    for (i in 1:n_switches) {
      if (i == 1) {
        sum_df_row_first <- data.frame(wtp_range[1],
                                       wtp[switches],
                                       strat_on_front[switches - 1],
                                       fix.empty.names = FALSE,
                                       stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_first)
      }
      if (i == n_switches) {
        sum_df_row_last <- data.frame(wtp[switches],
                                      wtp_range[2],
                                      strat_on_front[switches],
                                      fix.empty.names = FALSE,
                                      stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_last)
      }
      if (i > 1) {
        sum_df_row_middle <- data.frame(wtp[switches[i]],
                                        wtp[switches[i + 1]],
                                        strat_on_front[switches[i]],
                                        fix.empty.names = FALSE,
                                        stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_middle)
      }
    }
  }
  names(sum_df) <- c("range_min", "range_max", "cost_eff_strat")
  sum_df
}
