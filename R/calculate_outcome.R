#' A function that is used to calculate all outcomes
#'
#' @param outcome choice of outcome
#' @param cost data frame with costs
#' @param effect data frame with effects
#' @param wtp willingness-to-pay threshold
#'
#' @keywords internal
calculate_outcome <- function(outcome = c("nhb", "nmb", "eff", "cost", "nhb_loss", "nmb_loss"),
                              cost, effect, wtp) {
  outcome <- match.arg(outcome)
  if (outcome == "eff") {
    y <- effect
  } else if (outcome == "cost") {
    y <- cost
  } else {
    if (is.null(wtp)) {
      # the call. = FALSE makes the error message more clear
      stop("wtp must be provided for NHB and NMB",  call. = FALSE)
    }
    if (is.null(cost)) {
      stop("must provide cost for NHB and NMB.",  call. = FALSE)
    }
    if (outcome == "nhb") {
      y <- effect - cost / wtp
    }
    if (outcome == "nmb") {
      y <- effect * wtp - cost
    }
    if (outcome == "nhb_loss") {
      nhb <- calculate_outcome("nhb", cost, effect, wtp)
      optimal_strat <- which.max(colMeans(nhb))
      y <-  nhb[, optimal_strat] - nhb
    }
    if (outcome == "nmb_loss") {
      nmb <- calculate_outcome("nmb", cost, effect, wtp)
      optimal_strat <- which.max(colMeans(nmb))
      y <- nmb[, optimal_strat] - nmb
    }
  }
  return(y)
}
