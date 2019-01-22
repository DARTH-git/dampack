#' One-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a one-way sensitivity analysis (OWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#'
#' @param psa psa object
#' @param parm String with the name of the parameter of interest
#' @param newdata data frame with values of parameter of interest
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#' @keywords one-way sensitivity analysis; linear regression metamodel
#' @return A dataframe with the results of the sensitivity analysis.
#' Can be visualized with \code{\link{plot.owsa}}
#'
#' @export
owsa <- function(psa, parm, newdata = NULL,
                 poly.order = 2){

  #Run Multiple Multivariate Regression (MMR) Metamodel
  mm <- metamod(psa$effectiveness, psa, parm, poly.order)

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
#' @param ... further arguments to \code{plot.owsa} (not used)
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot.owsa <- function(x, txtsize = 12, ...) {
  # get parameter name
  parm <- colnames(x)[1]

  #Reshape dataframe for ggplot
  outcome <- melt(x, id.vars = parm, variable.name = "Strategy")

  owsa <- ggplot(data = outcome,
                 aes_(x = as.name(parm), y = as.name("value"), color = as.name("Strategy"))) +
    geom_line() +
    ggtitle("One-way sensitivity analysis") +
    xlab(parm) +
    ylab("E[Outcome]") +
    scale_colour_hue("Strategy", l = 50) +
    scale_x_continuous(breaks = number_ticks(6)) + #Adjust for number of ticks in x axis
    scale_y_continuous(breaks = number_ticks(6)) +
    common_theme(txtsize)
  return(owsa)
}
