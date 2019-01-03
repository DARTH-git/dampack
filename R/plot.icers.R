#' Plot of ICERs
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param x Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @param ... additional arguments to plot (not used)
#' @param title String with graph's title
#' @param txtsize integer. base font size
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - longer strategies will be
#' truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#'
#' @importFrom stringr str_sub
#' @export
plot.icers <- function(x, ...,
                      title = "",
                      txtsize = 12,
                      currency = "$",
                      effect_units = "QALYs",
                      label = c("frontier", "all", "none"),
                      label_max_char = 8,
                      plot_frontier_only = FALSE){
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
