#### object ####

#' Cost-Effectiveness Acceptability Curve (CEAC)
#'
#' \code{ceac} is used to compute and plot the cost-effectiveness acceptability
#' curves (CEAC) from a probabilistic sensitivity analysis (PSA) dataset.
#'
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @param wtp numeric vector with willingness-to-pay (WTP) thresholds
#' @concept cost-effectiveness acceptability curves
#' @details
#' Like \code{\link{calc_exp_loss}} and \code{\link{calc_evpi}}, \code{ceac} takes the
#' \code{psa} object first. The order used before dampack 1.1.0, \code{ceac(wtp, psa)}, still
#' works but is deprecated; named arguments work in any order.
#'
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
#' data("cancer_psa_data")
#' psa_cancer <- make_psa_obj(cost = cancer_psa_data$cost,
#'                            effectiveness = cancer_psa_data$effectiveness,
#'                            parameters = cancer_psa_data$parameters,
#'                            strategies = cancer_psa_data$strategies)
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' ceac_obj <- ceac(psa_cancer, wtp)
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
#' @importFrom lifecycle deprecate_soft deprecated is_present
#' @export
ceac <- function(psa, wtp) {
  # the order before 1.1.0 was ceac(wtp, psa): accept it, with a soft deprecation warning
  if (inherits(wtp, "psa") && !inherits(psa, "psa")) {
    deprecate_soft("1.1.0", "ceac(wtp = 'must come after `psa`')",
                   details = "Use `ceac(psa, wtp)`, the order of `calc_exp_loss()` and `calc_evpi()`.")
    tmp <- psa
    psa <- wtp
    wtp <- tmp
  }
  # check that psa has class 'psa'
  check_psa_object(psa)

  # define needed variables
  v_strategies <- psa$strategies
  n_strategies <- psa$n_strategies
  df_effectiveness <- psa$effectiveness
  df_cost <- psa$cost
  n_sim <- psa$n_sim

  # number of willingness to pay thresholds
  n_wtps <- length(wtp)

  # matrix to store probability optimal for each strategy
  m_cea <- matrix(0, nrow = n_wtps, ncol = n_strategies)
  colnames(m_cea) <- v_strategies

  # vector to store strategy at the cost-effectiveness acceptability frontier
  v_front <- rep(0, n_wtps)

  for (l in seq_len(n_wtps)) {
    # calculate net monetary benefit at wtp[l]
    lth_wtp <- wtp[l]
    df_nmb <-  calculate_outcome("nmb", df_cost, df_effectiveness, lth_wtp)

    # find the distribution of optimal strategies
    # ties.method = "first": exact comparison; the default ("random") treats values within a
    # relative tolerance of 1e-5 as ties and picks among them at random (#167)
    v_max_nmb <- max.col(df_nmb, ties.method = "first")
    a_opt <- table(v_max_nmb)
    m_cea[l, as.numeric(names(a_opt))] <- a_opt / n_sim

    # calculate point on CEAF
    # the strategy with the highest expected nmb
    v_front[l] <- which.max(colMeans(df_nmb))
  }

  # make cea df
  df_cea <- data.frame(wtp, m_cea, v_strategies[v_front],
                       stringsAsFactors = FALSE)
  colnames(df_cea) <- c("WTP", v_strategies, "fstrat")

  # Reformat df to long format
  ceac <- tidyr::pivot_longer(
    data = df_cea,
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
  ceac$Strategy <- factor(ceac$Strategy, levels = v_strategies, ordered = TRUE)

  # define classes
  # defining data.frame as well allows the object to use print.data.frame, for example
  class(ceac) <- c("ceac", "data.frame")
  attr(ceac, "currency") <- psa$currency

  ceac
}

#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC, using the object created by \code{\link{ceac}}.
#'
#' @param x object of class \code{ceac}.
#' @param frontier whether to plot acceptability frontier (TRUE) or not (FALSE)
#' @param points whether to plot points (TRUE) or not (FALSE)
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#' Defaults to the currency of the PSA the CEAC was computed from (or \code{$}), but can be any
#' currency symbol or word (e.g., £, €, peso).
#' @param effect_units units of effectiveness, in the plural (for example \code{"QALYs"} or
#' \code{"life-years"}). Default: \code{"QALYs"}. The willingness-to-pay axis uses the singular
#' ("$/QALY"), and a value already in the singular is used as given.
#' @param min_prob minimum probability to show strategy in plot.
#' For example, if the min_prob is 0.05, only strategies that ever
#' exceed Pr(Cost Effective) = 0.05 will be plotted. Most useful in situations
#' with many strategies.
#' @inheritParams add_common_aes
#'
#' @details
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return A \code{ggplot2} plot of the CEAC.
#'
#' @seealso \code{\link{ceac}}, \code{\link{summary.ceac}}
#'
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' df_ceac <- ceac(l_psa, wtp = seq(1e4, 1e5, by = 1e4))
#' plot(df_ceac)
#'
#' # without the frontier, and with fewer x-axis ticks
#' plot(df_ceac, frontier = FALSE, n_x_ticks = 5)
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
                      currency = NULL,
                      min_prob = 0,
                      txtsize = 12,
                      n_x_ticks = 10,
                      n_y_ticks = 8,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      ylim = NULL,
                      xlim = c(0, NA),
                      col = c("access", "full", "bw"),
                      effect_units = "QALYs",
                      ...) {
  currency <- resolve_currency(currency, x)
  wtp_name <- "WTP"
  prop_name <- "Proportion"
  strat_name <- "Strategy"
  x$WTP_thou <- x[, wtp_name] / 1000
  v_strategies <- unique(x$Strategy)

  Strategy <- Proportion <- maxpr <- On_Frontier <- WTP_thou <- NULL
  # removing strategies with probabilities always below `min_prob`
  # get group-wise max probability
  if (min_prob > 0) {
    df_max_prob <- x %>%
      group_by(Strategy) %>%
      summarize(maxpr = max(Proportion)) %>%
      filter(maxpr >= min_prob)
    v_strat_to_keep <- df_max_prob$Strategy
    if (length(v_strat_to_keep) == 0) {
      stop(
        paste("no strategies remaining. you may want to lower your min_prob value (currently ",
              min_prob, ")", sep = "")
      )
    }
    # report filtered out strategies
    v_old_strat <- unique(x$Strategy)
    v_diff_strat <- setdiff(v_old_strat, v_strat_to_keep)
    n_diff_strat <- length(v_diff_strat)
    if (n_diff_strat > 0) {
      # report strategies filtered out
      cat("filtered out ", n_diff_strat, " strategies with max prob below ", min_prob, ":\n",
          paste(v_diff_strat, collapse = ","), "\n", sep = "")

      # report if any filtered strategies are on the frontier
      df_filt <- filter(x, Strategy %in% v_diff_strat & On_Frontier)
      if (nrow(df_filt) > 0) {
        cat(paste0("WARNING - some strategies that were filtered out are on the frontier:\n",
                   paste(unique(df_filt$Strategy), collapse = ","), "\n"))
      }
    }

    # filter dataframe
    x <- filter(x, Strategy %in% v_strat_to_keep)
  }

  # Drop unused strategy names
  x$Strategy <- droplevels(x$Strategy)

  p <- ggplot(data = x, aes(x = WTP_thou,
                            y = !!sym(prop_name),
                            color = !!sym(strat_name))) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, " / ", per_unit(effect_units), ")", sep = "")) +
    ylab("Pr Most Cost-Effective")

  if (points) {
    p <- p + geom_point(aes(color = !!sym(strat_name)))
  }

  if (frontier) {
    df_front <- x[x$On_Frontier, ]
    p <- p + geom_point(data = df_front, aes(x = WTP_thou,
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
                 ylim = ylim, xlim = xlim,
                 v_str = v_strategies)
}


#' Summarize a ceac
#'
#' Describes cost-effective strategies and their
#' associated intervals of cost-effectiveness
#'
#' @param object object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @return data frame with one row per interval of willingness to pay over which
#' a single strategy is optimal, and three columns: \code{range_min} and
#' \code{range_max} bounding the interval, and \code{optimal_strategy} naming the
#' strategy on the cost-effectiveness frontier there. The intervals are open on
#' the right endpoint - i.e., [\code{range_min}, \code{range_max})
#'
#' @seealso \code{\link{ceac}}, \code{\link{plot.ceac}}
#'
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' df_ceac <- ceac(l_psa, wtp = seq(1e4, 1e5, by = 1e4))
#'
#' # Radio is optimal up to a WTP of 40,000 and Chemo from there on
#' summary(df_ceac)
#'
#' @export
summary.ceac <- function(object, ...) {
  df_front <- object[object$On_Frontier == TRUE, ]
  df_front$Strategy <- as.character(df_front$Strategy)
  v_wtp <- df_front$WTP
  v_wtp_range <- range(v_wtp)
  n_wtps <- length(v_wtp)

  # get the indices where the CE strategy isn't the same as the following CE strategy
  v_strat_on_front <- df_front$Strategy
  v_lagged_strat <- c(v_strat_on_front[-1], v_strat_on_front[n_wtps])
  v_switches <- which(v_strat_on_front != v_lagged_strat) + 1
  n_switches <- length(v_switches)
  # strat_on_front[switches] are the optimal strategies at wtp[switches]
  if (n_switches == 0) {
    wtp_min <- v_wtp_range[1]
    wtp_max <- v_wtp_range[2]
    one_strat <- unique(df_front$Strategy)
    df_sum <- data.frame(wtp_min,
                         wtp_max,
                         one_strat)
  } else {
    # build up summary data frame
    df_sum <- NULL
    # each switch contributes the interval that ends at it, and the final
    # switch additionally contributes the interval running to the highest wtp,
    # giving n_switches + 1 rows. every index below is a single element -
    # indexing with the whole `switches` vector recycles and duplicates rows.
    for (i in seq_len(n_switches)) {
      if (i == 1) {
        df_sum_row <- data.frame(v_wtp_range[1],
                                 v_wtp[v_switches[1]],
                                 v_strat_on_front[v_switches[1] - 1],
                                 fix.empty.names = FALSE,
                                 stringsAsFactors = FALSE)
      } else {
        df_sum_row <- data.frame(v_wtp[v_switches[i - 1]],
                                 v_wtp[v_switches[i]],
                                 v_strat_on_front[v_switches[i] - 1],
                                 fix.empty.names = FALSE,
                                 stringsAsFactors = FALSE)
      }
      df_sum <- rbind(df_sum, df_sum_row)
      if (i == n_switches) {
        df_sum_row_last <- data.frame(v_wtp[v_switches[n_switches]],
                                      v_wtp_range[2],
                                      v_strat_on_front[v_switches[n_switches]],
                                      fix.empty.names = FALSE,
                                      stringsAsFactors = FALSE)
        df_sum <- rbind(df_sum, df_sum_row_last)
      }
    }
  }
  names(df_sum) <- c("range_min", "range_max", "optimal_strategy")
  df_sum
}
