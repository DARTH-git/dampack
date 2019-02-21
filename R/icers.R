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
#' @param strategies character vector of strategy names
#' @param ref_strat reference strategy for the incremental comparison.
#' the default (NULL) is the cheapest strategy.
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
#' data(cdiff_psa)
#'
#' # summary() gives mean cost and effect for each strategy
#' sum_cdiff <- summary(cdiff_psa)
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
calculate_icers <- function(cost, effect, strategies, ref_strat = NULL) {
  # todo: check data is in correct format
  char_strat <- as.character(strategies)

  df <- data.frame("Strategy" = char_strat,
                   "Cost" = cost,
                   "Effect" = effect,
                   stringsAsFactors = FALSE)
  nstrat <- nrow(df)

  # three statuses: dominated, extended dominated, and non-dominated
  d <- NULL

  # detect dominated strategies
  # dominated strategies have a higher cost and lower effect
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))
  # if we want a different reference strategy aside from lowest cost
  if (!is.null(ref_strat)) {
    if (!(ref_strat %in% df$Strategy)) {
      stop(paste0("arg ref_strat (value: ", ref_strat, ") not present in data"))
    }
    df_ref <- filter(df, .data$Strategy == ref_strat)
    df_other <- filter(df, .data$Strategy != ref_strat)
    df <- rbind(df_ref, df_other)
  }

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
  continue <- TRUE
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

  # put reference at the top
  # if we want a different reference strategy aside from lowest cost
  if (!is.null(ref_strat)) {
    results_ref <- filter(results, .data$Strategy == ref_strat)
    results_other <- filter(results, .data$Strategy != ref_strat)
    results <- rbind(results_ref, results_other)
  }

  # declare status for first entry to be 'ref'
  results[1, "Status"] <- "ref"

  # re-arrange columns
  results <- results %>%
    select(.data$Strategy, .data$Cost, .data$Effect,
           .data$Inc_Cost, .data$Inc_Effect, .data$ICER, .data$Status)

  # declare class of results
  class(results) <- c("icers", "data.frame")
  return(results)
}


# Source: https://miqdad.freeasinspeech.org.uk/icer_calculator/
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
    non_d[1, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
  }
  return(non_d)
}

#' Plot of ICERs
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param x Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @inheritParams add_common_aes
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - longer strategies will be
#' truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#' @param alpha opacity of points
#'
#' @importFrom stringr str_sub
#' @export
plot.icers <- function(x,
                       txtsize = 12,
                       currency = "$",
                       effect_units = "QALYs",
                       label = c("frontier", "all", "none"),
                       label_max_char = 8,
                       plot_frontier_only = FALSE,
                       alpha = 1,
                       n_x_ticks = 6,
                       n_y_ticks = 6,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       ...){
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
                              xlim = xlim, ylim = ylim)

  # labeling
  if (label != "none") {
    plt_data[, strat_name] <- str_sub(plt_data[, strat_name], start = 1L, end = label_max_char)
    if (label == "all") {
      lab_data <- plt_data
    }
    if (label == "frontier") {
      lab_data <- plt_data[plt_data$Status == nd_name, ]
    }
    # create nudge columns
    range_x <- range(lab_data[, eff_name])
    width_x <- range_x[2] - range_x[1]

    range_y <- range(lab_data[, cost_name])
    width_y <- range_y[2] - range_y[1]

    nudge_x <- width_x / 50
    nudge_y <- - width_y / 50

    icer_plot <- icer_plot +
      geom_label(data = lab_data,
                aes_(label = as.name(strat_name)),
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                size = 3, show.legend = FALSE)
  }
  return(icer_plot)
}
