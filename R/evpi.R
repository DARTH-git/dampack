#' Expected Value of Perfect Information (EVPI)
#'
#' \code{calc_evpi} is used to compute the expected value of perfect information
#' (EVPI) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param psa psa object from \code{\link{make_psa_obj}}
#' @param pop A scalar that corresponds to the total population
#' @keywords expected value of perfect information; net monetary benefit
#' @section Details:
#' \code{evpi} calculates the value of eliminating all the uncertainty of a
#' cost-effectiveness analysis at each WTP threshold.
#' @return A data frame and \code{evpi} object with the EVPI at each WTP threshold.
#' @seealso \code{\link{plot.evpi}}, \code{\link{make_psa_obj}}
#' @examples
#' # load psa object provided with package
#' data("example_psa_obj")
#'
#' # define wtp threshold vector (can also use a single wtp)
#' wtp <- seq(1e4, 1e5, by = 1e4)
#' evpi <- calc_evpi(wtp, example_psa_obj)
#' plot(evpi) # see ?plot.evpi for options
#'
#' # can use plot options (# see ?plot.evpi for details)
#' plot(evpi, effect_units = "QALE")
#'
#' # or can use ggplot layers
#' plot(evpi) + ggtitle("Expected Value of Perfect Information")
#' @export
calc_evpi <- function(wtp, psa, pop = 1) {
  check_psa_object(psa)
  cost <- psa$cost
  effectiveness <- psa$effectiveness
  if (ncol(effectiveness) < 2) {
    stop("You need at least two different strategies to compute EVPI.")
  }
  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evpi
  evpi <- rep(0, n_wtps)
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for (l in 1:n_wtps){
    ## Calculate the opportunity loss from choosing d.star for each strategy
    loss <- calculate_outcome("nmb_loss", cost, effectiveness, wtp[l])

    ## Compute EVPI
    evpi[l] <- mean(apply(loss, 1, max)) * pop
  }

  # Data frame to store EVPI for each WTP threshold
  df.evpi <- data.frame("WTP" = wtp, "EVPI" = evpi)

  # declare class as both evpi (plotting) and data.frame (printing)
  class(df.evpi) <- c("evpi", "data.frame")
  return(df.evpi)
}

#' Plot of Expected Value of Perfect Information (EVPI)
#'
#' @description
#' Plots the \code{evpi} object created by \code{\link{calc_evpi}}.
#'
#' @param x object of class \code{evpi}, produced by function
#'  \code{\link{calc_evpi}}
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units Units of effectiveness. Default: QALY
#' @inheritParams add_common_aes
#' @keywords expected value of perfect information
#' @return A \code{ggplot2} plot with the EVPI
#' @seealso \code{\link{calc_evpi}}
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evpi <- function(x,
                      txtsize = 12,
                      currency = "$",
                      effect_units = "QALY",
                      n_y_ticks = 8,
                      n_x_ticks = 20,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlim = c(0, NA),
                      ylim = NULL,
                      ...){
  x$WTP_thou <- x$WTP / 1000
  g <- ggplot(data = x,
         aes_(x = as.name("WTP_thou"), y = as.name("EVPI"))) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}
