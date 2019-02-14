#' Two-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a two-way sensitivity analysis (TWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest

#' @inheritParams metamodel
#' @inheritParams predict.metamodel
#'
#' @return twsa A \code{ggplot2} object with the TWSA graph of \code{parm1} and
#' \code{parm2} on the outcome of interest.
#'
#' @export
twsa <- function(psa, parm1, parm2, ranges = NULL,
                 nsamp = 100,
                 outcome = c("eff", "cost", "nhb", "nmb"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2){
  parms <- c(parm1, parm2)

  # run metamodel
  mm <- metamodel("twoway", psa, parms, strategies, outcome, wtp, poly.order)

  # Predict Outcomes using MMMR Metamodel fit
  tw <- predict(mm, ranges, nsamp)

  # define classes
  class(tw) <- c("twsa", "data.frame")
  return(tw)
}

#' Two-way sensitivity analysis plot
#'
#' @param x a twsa object
#' @inheritParams add_common_aes
#' @param maximize If \code{TRUE}, plot of strategy with maximum expected outcome
#' (default); if \code{FALSE}, plot of strategy with minimum expected outcome
#'
#' @import ggplot2
#' @import dplyr
#' @export
plot.twsa <- function(x, maximize = TRUE,
                      col = c("full", "bw"),
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      txtsize = 12, ...) {

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
  g <- ggplot(opt_df, aes_(x = as.name(parm1), y = as.name(parm2))) +
    geom_tile(aes_(fill = as.name("strategy"))) +
    theme_bw() +
    xlab(parm1) +
    ylab(parm2)
  col <- match.arg(col)
  add_common_aes(g, txtsize, col = col, col_aes = "fill",
                 scale_name = "Strategy",
                 continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks,
                 n_y_ticks = n_y_ticks,
                 xexpand = c(0, 0),
                 yexpand = c(0, 0))
}
