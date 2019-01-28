#' One-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a one-way sensitivity analysis (OWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#'
#' @param newdata data frame with values of parameter of interest
#' @inheritParams metamod
#' @keywords one-way sensitivity analysis; linear regression metamodel
#' @return A dataframe with the results of the sensitivity analysis.
#' Can be visualized with \code{\link{plot.owsa}}
#'
#' @export
owsa <- function(psa, parm, newdata = NULL,
                 outcome = c("eff", "cost", "nhb", "nmb"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2){

  #Run Multiple Multivariate Regression (MMR) Metamodel
  mm <- metamod(psa, parm, strategies, outcome, wtp, poly.order)

  # Predict Outcomes using MMMR Metamodel fit
  ow <- predict(mm, newdata)

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
                      size = 1,
                      ...) {
  # get parameter name
  parm <- colnames(x)[1]

  #Reshape dataframe for ggplot
  outcome <- melt(x, id.vars = parm, variable.name = "Strategy")

  owsa <- ggplot(data = outcome,
                 aes_(x = as.name(parm), y = as.name("value"),
                      color = as.name("Strategy"))) +
    ggtitle(title) +
    xlab(parm) +
    ylab("E[Outcome]") +
    scale_x_continuous(breaks = number_ticks(n_x_ticks)) +
    scale_y_continuous(breaks = number_ticks(n_y_ticks)) +
    common_theme(txtsize)

  # ptype
  ptype <- match.arg(ptype)
  if (ptype == "line") {
    owsa <- owsa + geom_line(aes_(linetype = as.name("Strategy")), size = size)
  }
  if (ptype == "point") {
    owsa <- owsa + geom_point(aes_(shape = as.name("Strategy")), size = size)
  }

  # color - could move this to separate function
  col <- match.arg(col)
  if (col == "full") {
    owsa <- owsa + scale_colour_hue("Strategy", l = 50)
  }
  if (col == "bw") {
    owsa <- owsa + scale_color_grey("Strategy", start = 0.3)
  }

  return(owsa)
}
