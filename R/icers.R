#' Calculate incremental cost-effectiveness ratios (ICERs)
#'
#' @description
#' This function takes in strategies and their associated cost and effect, assigns them
#' one of three statuses (non-dominated, extended dominated, or dominated), and
#' calculates the incremental cost-effectiveness ratios for the non-dominated strategies
#'
#' The cost-effectiveness frontier can be visualized with \code{plot}, which calls \code{\link{plot.icers}}.
#'
#' An efficent way to get from a probabilistic sensitivity analysis to an ICER table
#' is by using \code{summary} on the PSA object and then using its columns as
#' inputs to \code{calculate_icers}.
#'
#' @param cost vector of cost for each strategy
#' @param effect vector of effect for each strategy
#' @param strategies string vector of strategy names
#' With the default (NULL), there is no reference strategy, and the strategies
#' are ranked in ascending order of cost.
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, incremental cost, incremental effect, and ICER.
#'
#' @seealso \code{\link{plot.icers}}
#'
#' @examples
#' ## Base Case
#' # if you have a base case analysis, can use calculate_icers on that
#' data(hund_strat)
#' hund_icers <- calculate_icers(hund_strat$Cost,
#'                               hund_strat$QALYs,
#'                               hund_strat$Strategy)
#'
#' plot(hund_icers)
#' # we have so many strategies that we may just want to plot the frontier
#' plot(hund_icers, plot_frontier_only = TRUE)
#' # see ?plot.icers for more options
#'
#' ## Using a PSA object
#' data(psa_cdiff)
#'
#' # summary() gives mean cost and effect for each strategy
#' sum_cdiff <- summary(psa_cdiff)
#'
#' # calculate icers
#' icers <- calculate_icers(sum_cdiff$meanCost,
#'                          sum_cdiff$meanEffect,
#'                          sum_cdiff$Strategy)
#' icers
#'
#' # visualize
#' plot(icers)
#'
#' # by default, only the frontier is labeled
#' # if using a small number of strategies, you can label all the points
#' # note that longer strategy names will get truncated
#' plot(icers, label = "all")
#' @export
calculate_icers <- function(cost, effect, strategies) {
  # checks on input
  n_cost <- length(cost)
  n_eff <- length(effect)
  n_strat <- length(strategies)
  if (n_cost != n_eff | n_eff != n_strat) {
    stop("cost, effect, and strategies must all be vectors of the same length", call. = FALSE)
  }

  # coerce to character, in case they are provided as numeric
  char_strat <- as.character(strategies)

  # create data frame to hold data
  df <- data.frame("Strategy" = char_strat,
                   "Cost" = cost,
                   "Effect" = effect,
                   stringsAsFactors = FALSE)
  nstrat <- nrow(df)

  # if only one strategy was provided, return df with NAs for incremental
  if (nstrat == 1) {
    df[, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
    return(df)
  }

  # three statuses: dominated, extended dominated, and non-dominated
  d <- NULL

  # detect dominated strategies
  # dominated strategies have a higher cost and lower effect
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))

  # iterate over strategies and detect (strongly) dominated strategies
  # those with higher cost and equal or lower effect
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df[j, "Effect"]
      if (jth_effect <= ith_effect) {
        # append dominated strategies to vector
        d <- c(d, df[j, "Strategy"])
      }
    }
  }

  # detect weakly dominated strategies (extended dominance)
  # this needs to be repeated until there are no more ED strategies
  ed <- vector()
  continue <- TRUE  # ensure that the loop is run at least once
  while (continue) {
    # vector of all dominated strategies (strong or weak)
    dom <- union(d, ed)

    # strategies declared to be non-dominated at this point
    nd <- setdiff(strategies, dom)

    # compute icers for nd strategies
    nd_df <- df[df$Strategy %in% nd, ] %>%
      compute_icers()

    # number non-d
    n_non_d <- nrow(nd_df)

    # if only two strategies left, we're done
    if (n_non_d <= 2) {
      break
    }

    # strategy identifiers for non-d
    nd_strat <- nd_df$Strategy

    # now, go through non-d strategies and detect any
    # with higher ICER than following strategy
    ## keep track of whether any ED strategies are picked up
    # if not, we're done - exit the loop
    new_ed <- 0
    for (i in 2:(n_non_d - 1)) {
      if (nd_df[i, "ICER"] > nd_df[i + 1, "ICER"]) {
        ed <- c(ed, nd_strat[i])
        new_ed <- new_ed + 1
      }
    }
    if (new_ed == 0) {
      continue <- FALSE
    }
  }

  # recompute icers without weakly dominated strategies
  nd_df_icers <- nd_df[!(nd_df$Strategy %in% dom), ] %>%
    mutate(Status = "ND") %>%
    compute_icers()

  # dominated and weakly dominated
  d_df <- df[df$Strategy %in% d, ] %>%
    mutate(ICER = NA, Status = "D")

  ed_df <- df[df$Strategy %in% ed, ] %>%
    mutate(ICER = NA, Status = "ED")

  # when combining, sort so we have ref,ND,ED,D
  results <- bind_rows(d_df, ed_df, nd_df_icers) %>%
    arrange(desc(.data$Status), .data$Cost, desc(.data$Effect))

  # re-arrange columns
  results <- results %>%
    select(.data$Strategy, .data$Cost, .data$Effect,
           .data$Inc_Cost, .data$Inc_Effect, .data$ICER, .data$Status)

  # declare class of results
  class(results) <- c("icers", "data.frame")
  return(results)
}

#' Calculate incremental cost-effectiveness ratios from a \code{psa} object.
#'
#' @description The mean costs and QALYs for each strategy in a PSA are used
#' to conduct an incremental cost-effectiveness analysis. \code{\link{calculate_icers}} should be used
#' if costs and QALYs for each strategy need to be specified manually, whereas \code{calculate_icers_psa}
#' can be used if mean costs and mean QALYs from the PSA are assumed to represent a base case scenario for
#' calculation of ICERS.
#'
#' Optionally, the \code{uncertainty} argument can be used to provide the 2.5th and 97.5th
#' quantiles for each strategy's cost and QALY outcomes based on the variation present in the PSA.
#' Because the dominated vs. non-dominated status and the ordering of strategies in the ICER table are
#' liable to change across different samples of the PSA, confidence intervals are not provided for the
#' incremental costs and QALYs along the cost-effectiveness acceptability frontier.
#' \code{link{plot.psa}} does not show the confidence intervals in the resulting plot
#' even if present in the ICER table.
#'
#' @param psa \code{psa} object from \code{link{make_psa_object}}
#' @param uncertainty whether or not 95% quantiles for the cost and QALY outcomes should be included
#' in the resulting ICER table. Defaults to \code{FALSE}.
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, cost, effect, incremental cost, incremental effect, and ICER. If \code{uncertainty} is
#' set to \code{TRUE}, four additional columns are provided for the 2.5th and 97.5th quantiles for
#' each strategy's cost and effect.
#' @seealso \code{\link{plot.icers}}
#' @seealso \code{\link{calculate_icers}}
#' @importFrom tidyr pivot_longer
calculate_icers_psa <- function(psa, uncertainty = FALSE) {

  # check that psa has class 'psa'
  check_psa_object(psa)

  # Calculate mean outcome values
  psa_sum <- summary(psa)

  # Supply mean outcome values to calculate_icers
  icers <- calculate_icers(cost = psa_sum$meanCost,
                           effect = psa_sum$meanEffect,
                           strategies = psa_sum$Strategy)

  if (uncertainty == TRUE) {

    # extract cost and effect data.frames from psa object
    cost <- psa$cost
    effect <- psa$effectiveness

    # Calculate quantiles across costs and effects
    cost_bounds <- cost %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(.data$Strategy) %>%
      summarize(LB_95_Cost = quantile(.data$value, probs = 0.025),
                UB_95_Cost = quantile(.data$value, probs = 0.975))

    effect_bounds <- effect %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(.data$Strategy) %>%
      summarize(LB_95_Effect = quantile(.data$value, probs = 0.025),
                UB_95_Effect = quantile(.data$value, probs = 0.975))

    # merge bound data.frames into icers data.frame
    icers <- icers %>%
      left_join(cost_bounds, by = "Strategy") %>%
      left_join(effect_bounds, by = "Strategy") %>%
      select(.data$Strategy, .data$Cost, .data$LB_95_Cost, .data$UB_95_Cost,
             .data$Effect, .data$LB_95_Effect, .data$UB_95_Effect,
             .data$Inc_Cost, .data$Inc_Effect, .data$ICER, .data$Status)
    }

  return(icers)
}

#' compute icers for non-dominated strategies
#'
#' @param non_d a data frame of non-dominated strategies, with columns
#' "Strategy", "Cost", and "Effect"
#'
#' @return the input dataframe with columns "Inc_Cost",
#' "Inc_Effect", and "ICER" appended
#'
#' @keywords internal
compute_icers <- function(non_d) {
  if (nrow(non_d) > 1) {
    non_d[1, "ICER"] <- NA
    for (i in 2:nrow(non_d)) {
      inc_cost <- (non_d[i, "Cost"] - non_d[i - 1, "Cost"])
      inc_effect <- (non_d[i, "Effect"] - non_d[i - 1, "Effect"])
      non_d[i, "Inc_Cost"] <- inc_cost
      non_d[i, "Inc_Effect"] <- inc_effect
      non_d[i, "ICER"] <- inc_cost / inc_effect
    }
  } else {
    # don't calculate ICER if only one strategy
    non_d[1, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
  }
  return(non_d)
}

#' Plot of ICERs
#'
#' Plots the cost-effectiveness plane for a ICER object, calculated with \code{\link{calculate_icers}}
#' @param x Object of class \code{icers}.
#' @inheritParams add_common_aes
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - if not NULL (the default)
#' longer strategies are truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#' @param alpha opacity of points
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return a ggplot2 object which can be modified by adding additional geoms
#'
#' @importFrom stringr str_sub
#' @importFrom ggrepel geom_label_repel
#' @export
plot.icers <- function(x,
                       txtsize = 12,
                       currency = "$",
                       effect_units = "QALYs",
                       label = c("frontier", "all", "none"),
                       label_max_char = NULL,
                       plot_frontier_only = FALSE,
                       alpha = 1,
                       n_x_ticks = 6,
                       n_y_ticks = 6,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xexpand = expansion(0.1),
                       yexpand = expansion(0.1),
                       max.iter = 20000,
                       ...) {
  if (ncol(x) > 7) {
    # reformat icers class object if uncertainty bounds are present
    x <- x %>%
      select(.data$Strategy, .data$Cost, .data$Effect,
             .data$Inc_Cost, .data$Inc_Effect,
             .data$ICER, .data$Status)
  }

  # type checking
  label <- match.arg(label)

  # this is so non-dominated strategies are plotted last (on top)
  x <- arrange(x, .data$Status)

  # change status text in data frame for plotting
  d_name <- "Dominated"
  ed_name <- "Weakly Dominated"
  nd_name <- "Efficient Frontier"

  status_expand <- c("D" = d_name, "ED" = ed_name,
                     "ND" = nd_name, "ref" = nd_name)
  x$Status <- factor(status_expand[x$Status], ordered = FALSE,
                     levels = c(d_name, ed_name, nd_name))

  # linetype
  plot_lines <- c("Dominated" = "blank",
                  "Weakly Dominated" = "blank",
                  "Efficient Frontier" = "solid")

  # names to refer to in aes_
  stat_name <- "Status"
  strat_name <- "Strategy"
  eff_name <- "Effect"
  cost_name <- "Cost"

  # frontier only
  if (plot_frontier_only) {
    plt_data <- x[x$Status == nd_name, ]
  } else {
    plt_data <- x
  }

  # make plot
  icer_plot <- ggplot(plt_data, aes_(x = as.name(eff_name), y = as.name(cost_name),
                                     shape = as.name(stat_name))) +
    geom_point(alpha = alpha, size = 2) +
    geom_line(aes_(linetype = as.name(stat_name), group = as.name(stat_name))) +
    scale_linetype_manual(name = NULL, values = plot_lines) +
    scale_shape_discrete(name = NULL) +
    labs(x = paste0("Effect (", effect_units, ")"),
         y = paste0("Cost (", currency, ")"))

  icer_plot <- add_common_aes(icer_plot, txtsize, col = "none",
                              continuous = c("x", "y"),
                              n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                              xbreaks = xbreaks, ybreaks = ybreaks,
                              xlim = xlim, ylim = ylim,
                              xexpand = xexpand, yexpand = yexpand)

  # labeling
  if (label != "none") {
    if (!is.null(label_max_char)) {
      plt_data[, strat_name] <- str_sub(plt_data[, strat_name],
                                        start = 1L, end = label_max_char)
    }
    if (label == "all") {
      lab_data <- plt_data
    }
    if (label == "frontier") {
      lab_data <- plt_data[plt_data$Status == nd_name, ]
    }

    icer_plot <- icer_plot +
      geom_label_repel(data = lab_data,
                       aes_(label = as.name(strat_name)),
                       size = 3,
                       show.legend = FALSE,
                       max.iter = max.iter,
                       direction = "both")
  }
  return(icer_plot)
}
