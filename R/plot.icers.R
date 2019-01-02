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
#' @param label boolean. whether or not to label the strategies.
#' may not be that useful when strategies have long names. Defaults to FALSE.
#'
#' todo: type checking.
#' @export
plot.icers <- function(x, ...,
                      title = "",
                      txtsize = 12,
                      currency = "$",
                      effect_units = "QALYs",
                      label = FALSE){
  # this is so non-dominated strategies are plotted last (on top)
  x <- arrange(x, .data$Status)

  # change status text in data frame for plotting
  d_name <- "Dominated"
  ed_name <- "Weakly Dominated"
  nd_name <- "Non-Dominated"

  status_expand <- c("D" = d_name, "ED" = ed_name,
                     "ND" = nd_name, "ref" = nd_name)
  x$Status <- factor(status_expand[x$Status], ordered = TRUE,
                     levels = c(d_name, ed_name, nd_name))

  # plot colors and lines, by status
  plot_cols <- c("Dominated" = "grey50",
                 "Weakly Dominated" = "darkorange",
                 "Non-Dominated" = "dodgerblue")
  plot_lines <- c("Dominated" = "blank",
                  "Weakly Dominated" = "blank",
                  "Non-Dominated" = "solid")

  # names to refer to in aes_
  stat_name <- "Status"
  strat_name <- "Strategy"
  eff_name <- "Effect"
  cost_name <- "Cost"

  # make plot
  icer_plot <- ggplot(x, aes_(x = as.name(eff_name), y = as.name(cost_name),
                              colour = as.name(stat_name))) +
    geom_point(alpha = 0.5) +
    geom_line(aes_(linetype = as.name(stat_name), group = as.name(stat_name))) +
    scale_color_manual(name = strat_name, values = plot_cols) +
    scale_linetype_manual(name = strat_name, values = plot_lines) +
    labs(x = paste0("Effect (", effect_units, ")"),
         y = paste0("Cost (", currency, ")"),
         title = title) +
    common_theme(txtsize)
  if (label) {
    icer_plot <- icer_plot +
      geom_label(aes_(label = as.name(strat_name)),
                 hjust = "top", vjust = "left", size = 3, show.legend = FALSE)
  }
  return(icer_plot)
}
