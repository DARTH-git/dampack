#' Calculate the expected loss
#'
#' @param wtp vector of willingness to pay thresholds
#' @param psa object of class \code{psa}, produced by function
#' \code{\link{make_psa_obj}}
#'
#' @return object with classes \code{elc} and \code{data.frame}
#' @export
calc_elc <- function(wtp, psa) {
  check_psa_object(psa)
  cost <- psa$cost
  effectiveness <- psa$effectiveness
  strategies <- psa$strategies
  n_sim <- psa$n_sim
  n_str  <- psa$n_strategies
  exp_loss <- matrix(0, nrow = length(wtp), ncol = n_str)
  for (l in 1:length(wtp)) {
    nmb <- effectiveness * wtp[l] - cost # Effectiveness minus Costs, with vector indexing
    max_str <- max.col(nmb)
    loss <- nmb[cbind(1:n_sim, max_str)] - nmb
    exp_loss[l, ] <- colMeans(loss)
  }
  # Optimal strategy based on lowest expected loss (max of negative expected loss)
  optimal.str <- max.col(-exp_loss)
  # Expected loss of optimal strategy
  optimal.el <- exp_loss[cbind(1:length(wtp), optimal.str)]
  # Format expected loss for plotting
  exp_loss_df <- data.frame(wtp, exp_loss, optimal.el)
  colnames(exp_loss_df) <- c("WTP", strategies, "Frontier_EVPI")
  class(exp_loss_df) <- c("elc", "data.frame")
  return(exp_loss_df)
}


#' Plot of Expected Loss Curves (ELC)
#'
#' @param x object of class \code{elc}, produced by function
#'  \code{\link{calc_elc}}
#' @param ... further arguments to plot() (not used)
#' @param title String with graph's title
#' @param txtsize number with base text size
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units Units of effectiveness. Default: QALY
#' @param log_y take the base 10 log of the y axis
#' @param frontier indicate the frontier (also the expected value of perfect information)
#' @param n_y_ticks number of axis ticks on the y axis
#' @param n_x_ticks number of axis ticks on the x axis
#' @param col either full-color ("full") or black-and-white ("bw")
#' @param lsize line size. defaults to 1.
#'
#' @keywords expected loss
#' @return A \code{ggplot2} object with the expected loss

#' @importFrom reshape2 melt
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.elc <- function(x, ...,
                     title = "",
                     txtsize = 12,
                     currency = "$",
                     effect_units = "QALY",
                     log_y = TRUE,
                     frontier = TRUE,
                     n_y_ticks = 8,
                     n_x_ticks = 20,
                     col = c("full", "bw"),
                     lsize = 1) {
  # melt for plotting in ggplot
  wtp_name <- "WTP_thou"
  loss_name <- "value"
  strat_name <- "Strategy"
  x_melt <- melt(x,
                 id.vars = "WTP",
                 variable.name = strat_name)
  x_melt[, wtp_name] <- x_melt$WTP / 1000

  # split into on frontier and not on frontier
  fname <- "on_frontier"
  frontier_char <- "Frontier_EVPI"
  on_frontier <- (x_melt[, strat_name] == frontier_char)
  x_melt[, fname] <- on_frontier
  nofront <- x_melt[!on_frontier, ]
  front <- x_melt[on_frontier, ]

  # formatting if logging the y axis
  if (log_y) {
    tr <- "log10"
  } else {
    tr <- "identity"
  }

  p <- ggplot(data = nofront, aes_(x = as.name(wtp_name),
                                   y = as.name(loss_name),
                                   color = as.name(strat_name))) +
    geom_point() +
    geom_line(aes_(linetype = as.name(strat_name)), size = lsize) +
    scale_x_continuous(breaks = number_ticks(n_x_ticks)) +
    scale_y_continuous(trans = tr,
                       labels = comma,
                       breaks = number_ticks(n_y_ticks))  +
    ggtitle(title) +
    xlab(paste0("Willingness to Pay (Thousand ", currency, "/", effect_units, ")")) +
    ylab(paste0("Expected Loss (", currency, ")")) +
    common_theme(txtsize)

  # color
  col <- match.arg(col)
  if (col == "full") {
    p <- p + scale_color_hue(l = 50)
  }
  if (col == "bw") {
    p <- p + scale_color_grey(start = 0.35)
  }

  if (frontier) {
    p <- p + geom_point(data = front, aes_(x = as.name(wtp_name),
                                           y = as.name(loss_name),
                                           shape = as.name(fname)),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier & EVPI") +
      guides(color = guide_legend(order = 1),
             linetype = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  return(p)
}
