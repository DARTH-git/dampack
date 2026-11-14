#' Expected Value of Perfect Information (EVPI)
#'
#' \code{calc_evpi} is used to compute the expected value of perfect information
#' (EVPI) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @param pop scalar that corresponds to the total population
#' @concept expected value of perfect information
#' @concept net monetary benefit
#' @section Details:
#' \code{evpi} calculates the value of eliminating all the uncertainty of a
#' cost-effectiveness analysis at each WTP threshold.
#' @return A data frame and \code{evpi} object with the EVPI at each WTP threshold.
#' @seealso \code{\link{plot.evpi}}, \code{\link{make_psa_obj}}
#' @examples
#' # build a psa object from the data provided with the package
#' data("cancer_psa_data")
#' psa_cancer <- make_psa_obj(cost = cancer_psa_data$cost,
#'                            effectiveness = cancer_psa_data$effectiveness,
#'                            parameters = cancer_psa_data$parameters,
#'                            strategies = cancer_psa_data$strategies)
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' evpi <- calc_evpi(psa_cancer, wtp)
#' plot(evpi) # see ?plot.evpi for options
#'
#' # can use plot options (# see ?plot.evpi for details)
#' plot(evpi, effect_units = "QALE")
#'
#' # or can use ggplot layers
#' plot(evpi) + ggtitle("Expected Value of Perfect Information")
#' @export
calc_evpi <- function(psa, wtp, pop = 1) {
  check_psa_object(psa)
  df_cost <- psa$cost
  df_effectiveness <- psa$effectiveness
  if (ncol(df_effectiveness) < 2) {
    stop("You need at least two different strategies to compute EVPI.")
  }
  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evpi
  v_evpi <- rep(0, n_wtps)
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for (l in seq_len(n_wtps)) {
    ## Calculate the opportunity loss from choosing d.star for each strategy
    df_loss <- calculate_outcome("nmb_loss", df_cost, df_effectiveness, wtp[l])

    ## Compute EVPI
    v_evpi[l] <- min(apply(df_loss, 2, mean)) * pop
  }

  # Data frame to store EVPI for each WTP threshold
  df_evpi <- data.frame("WTP" = wtp, "EVPI" = v_evpi)

  # declare class as both evpi (plotting) and data.frame (printing)
  class(df_evpi) <- c("evpi", "data.frame")
  attr(df_evpi, "currency") <- psa$currency
  df_evpi
}

#' Plot of Expected Value of Perfect Information (EVPI)
#'
#' @description
#' Plots the \code{evpi} object created by \code{\link{calc_evpi}}.
#'
#' @param x object of class \code{evpi}, produced by function
#'  \code{\link{calc_evpi}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: the currency of the PSA the results were computed from (or $), but it could be any
#'  currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness, in the plural (for example \code{"QALYs"} or
#' \code{"life-years"}). Default: \code{"QALYs"}. The willingness-to-pay axis uses the singular
#' ("$/QALY"), and a value already in the singular is used as given.
#' @inheritParams add_common_aes
#' @concept expected value of perfect information
#' @return A \code{ggplot2} plot with the EVPI
#' @seealso \code{\link{calc_evpi}}
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' df_evpi <- calc_evpi(l_psa, wtp = seq(1e4, 1e5, by = 1e4))
#' plot(df_evpi)
#'
#' # change the labels and the number of axis ticks
#' plot(df_evpi, currency = "USD", effect_units = "life-years", n_x_ticks = 5)
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evpi <- function(x,
                      txtsize = 12,
                      currency = NULL,
                      effect_units = "QALYs",
                      n_y_ticks = 8,
                      n_x_ticks = 20,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlim = c(0, NA),
                      ylim = NULL,
                      ...) {
  WTP_thou <- EVPI <- NULL
  currency <- resolve_currency(currency, x)
  x$WTP_thou <- x$WTP / 1000
  g <- ggplot(data = x,
              aes(x = WTP_thou, y = EVPI)) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", per_unit(effect_units), ")", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}
