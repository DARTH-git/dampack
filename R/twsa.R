#' Two-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a two-way sensitivity analysis (TWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#' @param strategies String vector with the name of the strategies
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest

#' @inheritParams owsa
#'
#' @keywords two-way sensitivity analysis; linear regression metamodel
#' @return twsa A \code{ggplot2} object with the TWSA graph of \code{parm1} and
#' \code{parm2} on the outcome of interest that can be posteriorly formatted
#' with \code{ggplot2} function
#'
#' @export
twsa <- function(psa, parm1, parm2, ranges = NULL,
                 outcome = c("eff", "cost", "nhb", "nmb"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2){
  parms <- c(parm1, parm2)

  # run metamodel
  mm <- metamod("twoway", psa, parms, strategies, outcome, wtp, poly.order)

  # Predict Outcomes using MMMR Metamodel fit
  tw <- predict(mm, ranges)

  # define classes
  class(tw) <- c("twsa", "data.frame")
  return(tw)
}

#' Two-way sensitivity analysis plot
#'
#' @param x a twsa object
#' @inheritParams plot.owsa
#' @param maximize If \code{TRUE}, plot of strategy with maximum expected outcome
#' (default); if \code{FALSE}, plot of strategy with minimum expected outcome
#' @param title Title for the plot
#'
#' @import ggplot2
#' @import dplyr
#' @export
plot.twsa <- function(x, txtsize = 12, maximize = TRUE,
                      title = NULL, ...) {

  # parameter names
  parms <- names(x)[c(1, 2)]
  parm1 <- parms[1]
  parm2 <- parms[2]

  # get optimal strategy
  # thanks to
  # https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
  if (maximize) {
    obj_fn <- which.max
  } else {
    obj_fn <- which.min
  }
  opt_df <- x %>%
    group_by(.data[[parm1]], .data[[parm2]]) %>%
    slice(obj_fn(.data$outcome_val))
  ggplot(opt_df, aes_(x = as.name(parm1), y = as.name(parm2))) +
    geom_tile(aes_(fill = as.name("strategy"))) +
    theme_bw() +
    ggtitle(title) +
    scale_fill_discrete("Optimal Strategy: ", l = 50) +
    scale_x_continuous(breaks = number_ticks(6)) +
    scale_y_continuous(breaks = number_ticks(6)) +
    xlab(parm1) +
    ylab(parm2) +
    common_theme(txtsize)
}
