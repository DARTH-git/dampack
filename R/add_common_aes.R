
#' Adds aesthetics to all plots to reduce code duplication
#'
#' @param gplot a ggplot object
#' @param txtsize base text size
#' @param scale_name how to name scale. Default inherits from variable name.
#' @param col either full color or black and white
#' @param col_aes which aesthetics to modify with \code{col}
#' @param lval color lightness - 0 to 100
#' @param greystart between 0 and 1. used in greyscale only. smaller numbers are lighter
#' @param greyend between 0 and 1, greater than greystart.
#' @param continuous which axes are continuous and should be modified by this function
#' @param n_x_ticks,n_y_ticks number of axis ticks
#' @param xlim,ylim vector of axis limits, or NULL, which sets limits automatically
#' @param xtrans,ytrans transformations for the axes. see \code{\link[ggplot2]{scale_continuous}} for details.
#' @param ... further arguments to plot (not used)
#'
#' @import ggplot2
#' @keywords internal
add_common_aes <- function(gplot, txtsize, scale_name = waiver(),
                           col = c("none", "full", "bw"),
                           col_aes = c("fill", "color"),
                           lval = 50,
                           greystart = 0.2,
                           greyend = 0.8,
                           continuous = c("none", "x", "y"),
                           n_x_ticks = 6,
                           n_y_ticks = 6,
                           xlim = NULL,
                           ylim = NULL,
                           xtrans = "identity",
                           ytrans = "identity",
                           ...) {
  p <- gplot +
    theme_bw() +
    theme(legend.title = element_text(size = txtsize),
          legend.text = element_text(size = txtsize - 3),
          title = element_text(face = "bold", size = (txtsize + 2)),
          axis.title.x = element_text(face = "bold", size = txtsize - 1),
          axis.title.y = element_text(face = "bold", size = txtsize - 1),
          axis.text.y = element_text(size = txtsize - 2),
          axis.text.x = element_text(size = txtsize - 2))

  col <- match.arg(col)
  col_aes <- match.arg(col_aes, several.ok = TRUE)
  if (col == "full") {
    p <- p +
      scale_color_discrete(name = scale_name, l = lval,
                           aesthetics = col_aes,
                           drop = FALSE)
  }
  if (col == "bw") {
    p <- p +
      scale_color_grey(name = scale_name, start = greystart, end = greyend,
                       aesthetics = col_aes,
                       drop = FALSE)
  }

  # axes and axis ticks
  continuous <- match.arg(continuous, several.ok = TRUE)

  if ("x" %in% continuous) {
    p <- p +
      scale_x_continuous(breaks = number_ticks(n_x_ticks),
                         labels = labfun,
                         limits = xlim,
                         trans = xtrans)
  }
  if ("y" %in% continuous) {
    p <- p +
      scale_y_continuous(breaks = number_ticks(n_y_ticks),
                         labels = labfun,
                         limits = ylim,
                         trans = ytrans)
  }
  return(p)
}

#' used to automatically label continuous scales
#' @keywords internal
#' @param x axis breaks
labfun <- function(x) {
  if (any(x > 999, na.rm = TRUE)) {
    comma(x)
  } else {
    x
  }
}


#' Number of ticks for \code{ggplot2} plots
#'
#' Function for number of ticks in axis of \code{ggplot2} plots.
#' @param n integer giving the desired number of number of ticks in axis of
#' \code{ggplot2} plots. Non-integer values are rounded down.
#' @keywords internal
#' @section Details:
#' Based on function \code{pretty}.
number_ticks <- function(n) {
  function(limits) {
    pretty(limits, n)
  }
}
