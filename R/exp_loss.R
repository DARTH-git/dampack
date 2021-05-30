#' Calculate the expected loss at a range of willingness-to-pay thresholds
#'
#' @description
#' The expected loss is the quantification of the foregone benefits
#' when choosing a suboptimal strategy given current evidence.
#'
#' @param wtp vector of willingness to pay thresholds
#' @param psa object of class \code{psa}, produced by function
#' \code{\link{make_psa_obj}}
#'
#' @details
#' Visualize the expected loss at a variety of WTP thresholds using \code{\link{plot.exp_loss}}.
#'
#' @return object with classes \code{exp_loss} and \code{data.frame}
#'
#' @seealso \code{\link{plot.exp_loss}}, \code{\link{make_psa_obj}}
#'
#' @references
#' \enumerate{
#' \item Alarid-Escudero F, Enns EA, Kuntz KM, Michaud TL, Jalal H.
#' "Time Traveling Is Just Too Dangerous" But Some Methods Are Worth Revisiting:
#' The Advantages of Expected Loss Curves Over Cost-Effectiveness Acceptability
#' Curves and Frontier. Value Health. 2019;22(5):611-618.
#' \item Eckermann S, Briggs A, Willan AR. Health technology assessment in the
#' cost- disutility plane. Med Decis Making. 2008;28(2):172–181.
#' }
#' @examples
#' data("example_psa_obj")
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' exp_loss <- calc_exp_loss(example_psa_obj, wtp)
#'
#' # can use head(), summary(), print(), etc.
#' head(exp_loss)
#'
#' # plot an expected loss curve (ELC)
#' plot(exp_loss)
#'
#' # the y axis is on a log scale by default
#' plot(exp_loss, log_y = FALSE)
#' @export
calc_exp_loss <- function(psa, wtp) {
  check_psa_object(psa)
  cost <- psa$cost
  effectiveness <- psa$effectiveness
  strategies <- psa$strategies
  n_str  <- psa$n_strategies
  exp_loss <- matrix(0, nrow = length(wtp), ncol = n_str)
  for (i in seq_len(length(wtp))) {
    ith_wtp <- wtp[i]
    loss <- calculate_outcome("nmb_loss", cost, effectiveness, ith_wtp)
    exp_loss[i, ] <- colMeans(loss)
  }
  # optimal strategy based on lowest expected loss (max of negative expected loss)
  # this was done because min.col isn't a function
  optimal_str <- max.col(-exp_loss)

  # Format expected loss for plotting
  exp_loss_df <- data.frame(wtp, exp_loss, strategies[optimal_str])
  colnames(exp_loss_df) <- c("WTP", strategies, "fstrat")

  # Reformat df to long format
  exp_loss_df_melt <- tidyr::pivot_longer(
    data = exp_loss_df,
    cols = !c("WTP", "fstrat"),
    names_to = "Strategy",
    values_to = "Expected_Loss"
  )

  # boolean for on frontier or not
  exp_loss_df_melt$On_Frontier <- (exp_loss_df_melt$fstrat == exp_loss_df_melt$Strategy)

  # drop fstrat column
  exp_loss_df_melt$fstrat <- NULL

  # order by WTP
  exp_loss_df_melt <- exp_loss_df_melt[order(exp_loss_df_melt$WTP), ]

  # remove rownames
  rownames(exp_loss_df_melt) <- NULL

  # make strategies in exp_loss object into ordered factors
  exp_loss_df_melt$Strategy <- factor(exp_loss_df_melt$Strategy, levels = strategies, ordered = TRUE)

  class(exp_loss_df_melt) <- c("exp_loss", "data.frame")
  return(exp_loss_df_melt)
}


#' Plot of Expected Loss Curves (ELC)
#'
#' @param x object of class \code{exp_loss}, produced by function
#'  \code{\link{calc_exp_loss}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @param log_y take the base 10 log of the y axis
#' @param frontier indicate the frontier (also the expected value of perfect information).
#' To only plot the EVPI see \code{\link{calc_evpi}}.
#' @param points whether to plot points on the curve (TRUE) or not (FALSE)
#' @param lsize line size. defaults to 1.
#' @inheritParams add_common_aes
#'
#' @return A \code{ggplot2} object with the expected loss
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.exp_loss <- function(x,
                          log_y = TRUE,
                          frontier = TRUE,
                          points = TRUE,
                          lsize = 1,
                          txtsize = 12,
                          currency = "$",
                          effect_units = "QALY",
                          n_y_ticks = 8,
                          n_x_ticks = 20,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlim = c(0, NA),
                          ylim = NULL,
                          col = c("full", "bw"),
                          ...) {
  wtp_name <- "WTP_thou"
  loss_name <- "Expected_Loss"
  strat_name <- "Strategy"
  x[, wtp_name] <- x$WTP / 1000

  # split into on frontier and not on frontier
  nofront <- x
  front <- x[x$On_Frontier, ]

  # Drop unused levels from strategy names
  nofront$Strategy <- droplevels(nofront$Strategy)
  front$Strategy <- droplevels(front$Strategy)
  # formatting if logging the y axis
  if (log_y) {
    tr <- "log10"
  } else {
    tr <- "identity"
  }

  p <- ggplot(data = nofront, aes_(x = as.name(wtp_name),
                                   y = as.name(loss_name))) +
    xlab(paste0("Willingness to Pay (Thousand ", currency, "/", effect_units, ")")) +
    ylab(paste0("Expected Loss (", currency, ")"))

  # color
  col <- match.arg(col)
  ## change linetype too if color is black and white
  if (col == "full") {
    if (points) {
      p <- p + geom_point(aes_(color = as.name(strat_name)))
    }
    p <- p +
      geom_line(size = lsize, aes_(color = as.name(strat_name)))

  }
  if (col == "bw") {
    if (points) {
      p <- p + geom_point()
    }
    p <- p +
      geom_line(aes_(linetype = as.name(strat_name)))
  }

  p <- add_common_aes(p, txtsize, col = col, col_aes = c("color", "line"),
                      continuous = c("x", "y"),
                      n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                      xbreaks = xbreaks, ybreaks = ybreaks,
                      xlim = xlim, ylim = ylim,
                      ytrans = tr)
  if (frontier) {
    p <- p + geom_point(data = front, aes_(x = as.name(wtp_name),
                                           y = as.name(loss_name),
                                           shape = as.name("On_Frontier")),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier & EVPI") +
      guides(color = guide_legend(order = 1),
             linetype = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  return(p)
}
