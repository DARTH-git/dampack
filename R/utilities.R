
#' Number of ticks for \code{ggplot2} plots
#'
#' Function for number of ticks in axis of \code{ggplot2} plots.
#' @param n integer giving the desired number of number of ticks in axis of
#' \code{ggplot2} plots. Non-integer values are rounded down.
#' @section Details:
#' Based on function \code{pretty}.
number_ticks <- function(n) {
  function(limits) {
    pretty(limits, n)
  }
}

#' @import ggplot2
common_theme <- function(txtsize) {
  theme_bw() +
    theme(legend.title = element_text(size = txtsize),
          legend.text = element_text(size = txtsize - 3),
          title = element_text(face = "bold", size = (txtsize + 2)),
          axis.title.x = element_text(face = "bold", size = txtsize - 1),
          axis.title.y = element_text(face = "bold", size = txtsize - 1),
          axis.text.y = element_text(size = txtsize - 2),
          axis.text.x = element_text(size = txtsize - 2))
}
