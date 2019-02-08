#' One-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a one-way sensitivity analysis (OWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#'
#' @inheritParams metamod
#' @inheritParams predict.metamodel
#' @return A dataframe with the results of the sensitivity analysis.
#' Can be visualized with \code{\link{plot.owsa}}
#'
#' @export
owsa <- function(psa, parms = NULL, ranges = NULL, nsamps = 100,
                 outcome = c("eff", "cost", "nhb", "nmb"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2){

  # create metamodel
  mm <- metamod("oneway", psa, parms, strategies, outcome, wtp, poly.order)

  # Predict Outcomes using MMMR Metamodel fit
  ow <- predict(mm, ranges, nsamps)

  # define classes
  class(ow) <- c("owsa", "data.frame")
  return(ow)
}

#' Plot a sensitivity analysis
#'
#' @param x an owsa object
#' @param txtsize base text size in the plot
#' @param col either full-color ("full") or black and white ("bw")
#' @param ptype plot type. either point or line.
#' @param title plot title.
#' @param n_x_ticks number of axis ticks on the x axis
#' @param n_y_ticks number of axis ticks on the y axis
#' @param size either point size (ptype = "point") or line size (ptype = "line")
#' @param facet_scales whether the x or y axes should be fixed. See \code{\link[ggplot2]{facet_grid}} in the \code{ggplo2} package for
#' more details.
#' @param facet_ncol number of columns in plot facet.
#' The default (NULL) is passed to \code{\link[ggplot2]{facet_wrap}},
#' which determines the number of rows and columns automatically.
#' @param facet_nrow number of rows in plot facet.
#' @param ... further arguments to \code{plot.owsa} (not used)
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot.owsa <- function(x, txtsize = 12,
                      col = c("full", "bw"),
                      ptype = c("line", "point"),
                      title = "",
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      facet_scales = c("free_x", "free_y", "free", "fixed"),
                      facet_nrow = NULL,
                      facet_ncol = NULL,
                      size = 1,
                      ...) {
  scales <- match.arg(facet_scales)
  owsa <- ggplot(data = x,
                 aes_(x = as.name("pranges_samp"), y = as.name("outcome_val"),
                      color = as.name("strategy"))) +
    facet_wrap(facets = "parameter", scales = scales, nrow = facet_nrow, ncol = facet_ncol) +
    ggtitle(title) +
    ylab("E[Outcome]") +
    xlab("Parameter Values") +
    scale_x_continuous(breaks = number_ticks(n_x_ticks)) +
    scale_y_continuous(breaks = number_ticks(n_y_ticks)) +
    common_theme(txtsize)

  # ptype
  ptype <- match.arg(ptype)
  if (ptype == "line") {
    owsa <- owsa + geom_line(aes_(linetype = as.name("strategy")), size = size)
  }
  if (ptype == "point") {
    owsa <- owsa + geom_point(aes_(shape = as.name("strategy")), size = size)
  }

  # color - could move this to separate function
  col <- match.arg(col)
  if (col == "full") {
    owsa <- owsa + scale_colour_hue("strategy", l = 50)
  }
  if (col == "bw") {
    owsa <- owsa + scale_color_grey("strategy", start = 0.3)
  }

  return(owsa)
}
