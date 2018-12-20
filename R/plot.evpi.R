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
  x$WTP_thou <- x$WTP/1000
  ggplot(data = x,
         aes_(x = as.name("WTP_thou"), y = as.name("EVPI"))) +
    geom_point() +
    geom_line() +
    ggtitle(title) +
    scale_x_continuous(labels = comma, breaks = number_ticks(20))+
    scale_y_continuous(labels = comma, breaks = number_ticks(6))+
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = "")) +
    common_theme(txtsize)
}
