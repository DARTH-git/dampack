#' function to calculate ICERS
#'
#' Adapted from https://miqdad.freeasinspeech.org.uk/icer_calculator/
#'
#' @param cost vector of cost for each strategy
#' @param effect vector of effect for each strategy
#' @param strategies character vector of strategy names
#'
#' @export
calculate_icers <- function(cost, effect, strategies) {
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
  }
  return(non_d)
}

#' Plot of ICERs
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param x Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @param title String with graph's title
#' @param txtsize integer. base font size
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - longer strategies will be
#' truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#' @param ... additional arguments to plot (not used)
#'
#' @importFrom stringr str_sub
#' @export
plot.icers <- function(x,
                       title = "",
                       txtsize = 12,
                       currency = "$",
                       effect_units = "QALYs",
                       label = c("frontier", "all", "none"),
                       label_max_char = 8,
                       plot_frontier_only = FALSE,
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
  x$Status <- factor(status_expand[x$Status], ordered = TRUE,
                     levels = c(d_name, ed_name, nd_name))

  # plot colors and lines, by status
  plot_cols <- c("Dominated" = "grey50",
                 "Weakly Dominated" = "darkorange",
                 "Efficient Frontier" = "dodgerblue")
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
                                     colour = as.name(stat_name))) +
    geom_point(alpha = 0.5) +
    geom_line(aes_(linetype = as.name(stat_name), group = as.name(stat_name))) +
    scale_color_manual(name = strat_name, values = plot_cols) +
    scale_linetype_manual(name = strat_name, values = plot_lines) +
    labs(x = paste0("Effect (", effect_units, ")"),
         y = paste0("Cost (", currency, ")"),
         title = title) +
    common_theme(txtsize)

  # labeling
  if (label != "none") {
    plt_data[, strat_name] <- str_sub(plt_data[, strat_name], start = 1L, end = label_max_char)
    if (label == "all") {
      lab_data <- plt_data
    }
    if (label == "frontier") {
      lab_data <- plt_data[plt_data$Status == nd_name, ]
    }
    icer_plot <- icer_plot +
      geom_label(data = lab_data, aes_(label = as.name(strat_name)),
                 hjust = "top", vjust = "left", size = 3, show.legend = FALSE)
  }
  return(icer_plot)
}
