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
#' @return evpi A data frame with the EVPI at each WTP threshold.
#'
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
    # Compute NMB with vector indexing
    nmb <-  wtp[l] * effectiveness - cost
    ## Find the optimal strategy with current info
    d.star <- which.max(colMeans(nmb))
    ## Calculate the opportunity loss from choosing d.star for each strategy
    loss <- nmb - nmb[, d.star]

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
#' Plots the EVPI as a \code{ggplot2} object calculated with \code{\link{calc_evpi}}.
#' @param x object of class \code{evpi}, produced by function
#'  \code{\link{calc_evpi}}
#' @param ... further arguments to plot() (not used)
#' @param title String with graph's title
#' @param txtsize number with text size
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units Units of effectiveness. Default: QALY
#'
#' @keywords expected value of perfect information
#' @return A \code{ggplot2} object with the EVPI
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evpi <- function(x, ...,
                      title = "Expected Value of Perfect Information",
                      txtsize = 12,
                      currency = "$",
                      effect_units = "QALY"){
  x$WTP_thou <- x$WTP / 1000
  ggplot(data = x,
         aes_(x = as.name("WTP_thou"), y = as.name("EVPI"))) +
    geom_line() +
    ggtitle(title) +
    scale_x_continuous(labels = comma, breaks = number_ticks(20)) +
    scale_y_continuous(labels = comma, breaks = number_ticks(6)) +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = "")) +
    common_theme(txtsize)
}