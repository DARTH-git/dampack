
#' Number of ticks for \code{ggplot2} plots
#'
#' Function for number of ticks in axis of \code{ggplot2} plots.
#' @param n integer giving the desired number of number of ticks in axis of
#' \code{ggplot2} plots. Non-integer values are rounded down.
#' @keywords ggplot2
#' @section Details:
#' Based on function \code{pretty}.
number_ticks <- function(n) {
  function(limits) {
    pretty(limits, n)
  }
}
