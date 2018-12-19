#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param x Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @param ... additional arguments to plot (not used)
#' @param frontier Whether to plot acceptability frontier
#' @param title String with graph's title
#' @param txtsize number with text size
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#' @param threshold minimum probability to show strategy in plot.
#' For example, if the threshold is 0.05, only strategies that ever
#' exceed Pr(Cost Effective) = 0.05 will be plotted. Most useful in situations
#' with many strategies.
#'
#' Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @keywords cost-effectiveness acceptability curves
#' @section Details:
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return ceac.gg A \code{ggplot2} object with the CEAC
#' @import ggplot2
#' @import dplyr
#'
#' @export
plot.ceac <- function(x, ...,
                      frontier = TRUE,
                      title = "Cost-Effectiveness Acceptability Curves",
                      txtsize = 12,
                      currency = "$",
                      threshold = 0){
  wtp_name <- "WTP"
  prop_name <- "Proportion"
  strat_name <- "Strategy"
  x$WTP_thou <- x[, wtp_name]/1000

  # removing strategies with probabilities always below `threshold`
  # get group-wise max probability
  if (threshold > 0) {
    max_prob <- x %>%
      group_by(.data$Strategy) %>%
      summarize(maxpr = max(.data$Proportion)) %>%
      filter(.data$maxpr >= threshold)
    strat_to_keep <- max_prob$Strategy
    if (length(strat_to_keep) == 0) {
      stop(
        paste('no strategies remaining. you may want to lower your threshold value (currently ',
              threshold, ")", sep="")
      )
    }
    # report filtered out strategies
    old_strat <- unique(x$Strategy)
    diff_strat <- setdiff(old_strat, strat_to_keep)
    n_diff_strat <- length(diff_strat)
    if (n_diff_strat > 0) {
      cat('filtered out ', n_diff_strat, ' strategies with max prob below ', threshold, ':\n',
          paste(diff_strat, collapse=","), sep="")
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
    ggtitle(title) +
    scale_colour_hue(l = 50) +
    scale_x_continuous(breaks = number_ticks(20)) +
    scale_y_continuous(limits = c(0, 1)) +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/QALY)", sep = "")) +
    ylab("Pr Cost-Effective") +
    theme_bw() +
    theme(legend.title = element_text(size = txtsize),
          legend.text = element_text(size = txtsize - 3),
          title = element_text(face = "bold", size = 14),
          axis.title.x = element_text(face = "bold", size = txtsize),
          axis.title.y = element_text(face = "bold", size = txtsize),
          axis.text.y = element_text(size = txtsize),
          axis.text.x = element_text(size = txtsize))
  if (frontier) {
    front <- x[x$On_Frontier, ]
    p <- p + geom_point(data = front, aes_(x = as.name("WTP_thou"),
                                           y = as.name(prop_name)),
                        shape = 0, size = 3, stroke = 1.2, color = "black")
  }
  p
}
