#### object ####

#' Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#'
#' @param wtp numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @keywords cost-effectiveness acceptability curves
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} threshold. The returned object has classes
#' \code{ceac} and \code{data.frame}, and has its own plot method (\code{\link{plot.ceac}}).
#'
#' @return An object of class \code{ceac} that can be visualized with \code{plot}. The \code{ceac}
#' object is a data.frame that shows the proportion of PSA samples for which each strategy at each
#' WTP threshold is cost-effective. The final column indicates whether or not the strategy at a
#' particular WTP is on the cost-efficient frontier.
#'
#' @examples
#' # psa input provided with package
#' data("example_psa")
#' example_psa_obj <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
#'                     example_psa$parameters, example_psa$strategies)
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' ceac_obj <- ceac(wtp, example_psa_obj)
#' plot(ceac_obj) # see ?plot.ceac for options
#'
#' # this is most useful when there are many strategies
#' # warnings are printed to describe strategies that
#' # have been filtered out
#' plot(ceac_obj, min_prob = 0.5)
#'
#' # standard ggplot layers can be used
#' plot(ceac_obj) +
#'     labs(title = "CEAC", y = "Pr(Cost-effective) at WTP")
#'
#' # the ceac object is also a data frame
#' head(ceac_obj)
#'
#' # summary() tells us the regions of cost-effectiveness for each strategy.
#' # Note that the range_max column is an open parenthesis, meaning that the
#' # interval over which that strategy is cost-effective goes up to but does not include
#' # the value in the range_max column.
#' summary(ceac_obj)
#'
#' @seealso
#' \code{\link{plot.ceac}}, \code{\link{summary.ceac}}
#'
#'
#' @importFrom tidyr pivot_longer
#' @export
ceac <- function(wtp, psa) {
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

  # matrix to store probability optimal for each strategy
  cea <- matrix(0, nrow = n_wtps, ncol = n_strategies)
  colnames(cea) <- strategies

  # vector to store strategy at the cost-effectiveness acceptability frontier
  frontv <- rep(0, n_wtps)

  for (l in 1:n_wtps) {
    # calculate net monetary benefit at wtp[l]
    lth_wtp <- wtp[l]
    nmb <-  calculate_outcome("nmb", cost, effectiveness, lth_wtp)

    # find the distribution of optimal strategies
    max.nmb <- max.col(nmb)
    opt <- table(max.nmb)
    cea[l, as.numeric(names(opt))] <- opt / n_sim

    # calculate point on CEAF
    # the strategy with the highest expected nmb
    frontv[l] <- which.max(colMeans(nmb))
  }

  # make cea df
  cea_df <- data.frame(wtp, cea, strategies[frontv],
                       stringsAsFactors = FALSE)
  colnames(cea_df) <- c("WTP", strategies, "fstrat")

  # Reformat df to long format
  ceac <- tidyr::pivot_longer(
    data = cea_df,
    cols = !c("WTP", "fstrat"),
    names_to = "Strategy",
    values_to = "Proportion"
  )

  # boolean for on frontier or not
  ceac$On_Frontier <- (ceac$fstrat == ceac$Strategy)

  # drop fstrat column
  ceac$fstrat <- NULL

  # order by WTP
  ceac <- ceac[order(ceac$WTP), ]

  # remove rownames
  rownames(ceac) <- NULL

  # make strategies in ceac object into ordered factors
  ceac$Strategy <- factor(ceac$Strategy, levels = strategies, ordered = TRUE)

  # define classes
  # defining data.frame as well allows the object to use print.data.frame, for example
  class(ceac) <- c("ceac", "data.frame")

  return(ceac)
}

#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC, using the object created by \code{\link{ceac}}.
#'
#' @param x object of class \code{ceac}.
#' @param frontier whether to plot acceptability frontier (TRUE) or not (FALSE)
#' @param points whether to plot points (TRUE) or not (FALSE)
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'Defaults to \code{$}, but can be any currency symbol or word (e.g., £, €, peso)
#' @param min_prob minimum probability to show strategy in plot.
#' For example, if the min_prob is 0.05, only strategies that ever
#' exceed Pr(Cost Effective) = 0.05 will be plotted. Most useful in situations
#' with many strategies.
#' @inheritParams add_common_aes
#'
#' @keywords internal
#'
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return A \code{ggplot2} plot of the CEAC.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang !!
#' @importFrom rlang sym
#'
#' @export
plot.ceac <- function(x,
                      frontier = TRUE,
                      points = TRUE,
                      currency = "$",
                      min_prob = 0,
                      txtsize = 12,
                      n_x_ticks = 10,
                      n_y_ticks = 8,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      ylim = NULL,
                      xlim = c(0, NA),
                      col = c("full", "bw"),
                      ...) {
  wtp_name <- "WTP"
  prop_name <- "Proportion"
  strat_name <- "Strategy"
  x$WTP_thou <- x[, wtp_name] / 1000

  Strategy <- Proportion <- maxpr <- On_Frontier <- WTP_thou <- NULL
  # removing strategies with probabilities always below `min_prob`
  # get group-wise max probability
  if (min_prob > 0) {
    max_prob <- x %>%
      group_by(Strategy) %>%
      summarize(maxpr = max(Proportion)) %>%
      filter(maxpr >= min_prob)
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
      df_filt <- filter(x, Strategy %in% diff_strat & On_Frontier)
      if (nrow(df_filt) > 0) {
        cat(paste0("WARNING - some strategies that were filtered out are on the frontier:\n",
                   paste(unique(df_filt$Strategy), collapse = ","), "\n"))
      }
    }

    # filter dataframe
    x <- filter(x, Strategy %in% strat_to_keep)
  }

  # Drop unused strategy names
  x$Strategy <- droplevels(x$Strategy)

  p <- ggplot(data = x, aes(x = WTP_thou,
                            y = !!sym(prop_name),
                            color = !!sym(strat_name))) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, " / QALY)", sep = "")) +
    ylab("Pr Cost-Effective")

  if (points) {
    p <- p + geom_point(aes(color = !!sym(strat_name)))
  }

  if (frontier) {
    front <- x[x$On_Frontier, ]
    p <- p + geom_point(data = front, aes(x = WTP_thou,
                                          y = !!sym(prop_name),
                                          shape = On_Frontier),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier") +
      guides(color = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  col <- match.arg(col)
  add_common_aes(p, txtsize, col = col, col_aes = "color",
                 continuous = c("x", "y"), n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 ylim = ylim, xlim = xlim)
}


#' Summarize a ceac
#'
#' Describes cost-effective strategies and their
#' associated intervals of cost-effectiveness
#'
#' @param object object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @return data frame showing the interval of cost effectiveness for each
#' interval. The intervals are open on the right endpoint -
#' i.e., [\code{range_min}, \code{range_max})
#'
#' @keywords internal
#'
#' @export
summary.ceac <- function(object, ...) {
  front <- object[object$On_Frontier == TRUE, ]
  front$Strategy <- as.character(front$Strategy)
  wtp <- front$WTP
  wtp_range <- range(wtp)
  n_wtps <- length(wtp)

  # get the indices where the CE strategy isn't the same as the following CE strategy
  strat_on_front <- front$Strategy
  lagged_strat <- c(strat_on_front[-1], strat_on_front[n_wtps])
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
