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
#' data("cancer_psa_data")
#' psa_cancer <- make_psa_obj(cost = cancer_psa_data$cost,
#'                            effectiveness = cancer_psa_data$effectiveness,
#'                            parameters = cancer_psa_data$parameters,
#'                            strategies = cancer_psa_data$strategies)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' exp_loss <- calc_exp_loss(psa_cancer, wtp)
#'
#' # can use head(), summary(), print(), etc.
#' head(exp_loss)
#'
#' # plot an expected loss curve (ELC)
#' plot(exp_loss)
#'
#' # the y axis is on a log scale by default
#' plot(exp_loss, log_y = FALSE)
#' @importFrom tidyr pivot_longer
#' @export
calc_exp_loss <- function(psa, wtp) {
  check_psa_object(psa)
  df_cost <- psa$cost
  df_effectiveness <- psa$effectiveness
  v_strategies <- psa$strategies
  n_str  <- psa$n_strategies
  m_exp_loss <- matrix(0, nrow = length(wtp), ncol = n_str)
  for (i in seq_along(wtp)) {
    ith_wtp <- wtp[i]
    df_loss <- calculate_outcome("nmb_loss", df_cost, df_effectiveness, ith_wtp)
    m_exp_loss[i, ] <- colMeans(df_loss)
  }
  # optimal strategy based on lowest expected loss (max of negative expected loss)
  # this was done because min.col isn't a function
  v_optimal_str <- max.col(-m_exp_loss)

  # Format expected loss for plotting
  df_exp_loss <- data.frame(wtp, m_exp_loss, v_strategies[v_optimal_str])
  colnames(df_exp_loss) <- c("WTP", v_strategies, "fstrat")

  # Reformat df to long format
  df_exp_loss_melt <- tidyr::pivot_longer(
    data = df_exp_loss,
    cols = !c("WTP", "fstrat"),
    names_to = "Strategy",
    values_to = "Expected_Loss"
  )

  # boolean for on frontier or not
  df_exp_loss_melt$On_Frontier <- (df_exp_loss_melt$fstrat == df_exp_loss_melt$Strategy)

  # drop fstrat column
  df_exp_loss_melt$fstrat <- NULL

  # order by WTP
  df_exp_loss_melt <- df_exp_loss_melt[order(df_exp_loss_melt$WTP), ]

  # remove rownames
  rownames(df_exp_loss_melt) <- NULL

  # make strategies in exp_loss object into ordered factors
  df_exp_loss_melt$Strategy <- factor(df_exp_loss_melt$Strategy, levels = v_strategies, ordered = TRUE)

  class(df_exp_loss_melt) <- c("exp_loss", "data.frame")
  attr(df_exp_loss_melt, "currency") <- psa$currency
  df_exp_loss_melt
}


#' Plot of Expected Loss Curves (ELC)
#'
#' @param x object of class \code{exp_loss}, produced by function
#'  \code{\link{calc_exp_loss}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: the currency of the PSA the results were computed from (or $), but it could be any
#'  currency symbol or word (e.g., £, €, peso)
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
#' @importFrom rlang !!
#' @importFrom rlang sym
#' @importFrom scales comma
#' @export
plot.exp_loss <- function(x,
                          log_y = TRUE,
                          frontier = TRUE,
                          points = TRUE,
                          lsize = 1,
                          txtsize = 12,
                          currency = NULL,
                          effect_units = "QALY",
                          n_y_ticks = 8,
                          n_x_ticks = 20,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlim = c(0, NA),
                          ylim = NULL,
                          col = c("access", "full", "bw"),
                          ...) {
  On_Frontier <- NULL
  currency <- resolve_currency(currency, x)
  wtp_name <- "WTP_thou"
  loss_name <- "Expected_Loss"
  strat_name <- "Strategy"
  x[, wtp_name] <- x$WTP / 1000
  v_strategies <- unique(x$Strategy)

  # split into on frontier and not on frontier
  df_nofront <- x
  df_front <- x[x$On_Frontier, ]

  # Drop unused levels from strategy names
  df_nofront$Strategy <- droplevels(df_nofront$Strategy)
  df_front$Strategy <- droplevels(df_front$Strategy)
  # formatting if logging the y axis
  if (log_y) {
    tr <- "log10"
  } else {
    tr <- "identity"
  }

  p <- ggplot(data = df_nofront, aes(x = !!sym(wtp_name),
                                     y = !!sym(loss_name))) +
    xlab(paste0("Willingness to Pay (Thousand ", currency, "/", effect_units, ")")) +
    ylab(paste0("Expected Loss (", currency, ")"))

  # color
  col <- match.arg(col)
  ## change linetype too if color is black and white
  if (col == "full" || col == "access") {
    if (points) {
      p <- p + geom_point(aes(color = !!sym(strat_name)))
    }
    p <- p +
      geom_line(linewidth = lsize, aes(color = !!sym(strat_name)))

  }
  if (col == "bw") {
    if (points) {
      p <- p + geom_point()
    }
    p <- p +
      geom_line(aes(linetype = !!sym(strat_name)))
  }

  p <- add_common_aes(p, txtsize, col = col, col_aes = c("color", "line"),
                      continuous = c("x", "y"),
                      n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                      xbreaks = xbreaks, ybreaks = ybreaks,
                      xlim = xlim, ylim = ylim, v_str = v_strategies,
                      ytrans = tr)
  if (frontier) {
    p <- p + geom_point(data = df_front, aes(x = !!sym(wtp_name),
                                             y = !!sym(loss_name),
                                             shape = On_Frontier),
                        size = 3, stroke = 1, color = "black") +
      scale_shape_manual(name = NULL, values = 0, labels = "Frontier & EVPI") +
      guides(color = guide_legend(order = 1),
             linetype = guide_legend(order = 1),
             shape = guide_legend(order = 2))
  }
  p
}
