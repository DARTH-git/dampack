#' A function that is used to calculate all outcomes
#'
#' @param outcome choice of outcome
#' @param cost data frame with costs
#' @param effect data frame with effects
#' @param wtp willingness-to-pay threshold
#'
#' @keywords internal
calculate_outcome <- function(outcome = c("nhb", "nmb", "eff", "cost", "nhb_loss",
                                          "nmb_loss", "nhb_loss_voi", "nmb_loss_voi"),
                              cost, effect, wtp) {
  outcome <- match.arg(outcome)
  n_sim <- nrow(cost)
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
    if (outcome == "nhb_loss" | outcome == "nmb_loss") {
      if (outcome == "nhb_loss") {
        net_outcome <- "nhb"
      }
      if (outcome == "nmb_loss") {
        net_outcome <- "nmb"
      }
      netben <- calculate_outcome(net_outcome, cost, effect, wtp)
      max_str_rowwise <- max.col(netben)
      y <-  netben[cbind(1:n_sim, max_str_rowwise)] - netben
    }
    if (outcome == "nhb_loss_voi" | outcome == "nmb_loss_voi") {
      if (outcome == "nhb_loss_voi") {
        net_outcome <- "nhb"
      }
      if (outcome == "nmb_loss_voi") {
        net_outcome <- "nmb"
      }
      netben <- calculate_outcome(net_outcome, cost, effect, wtp)
      max_str <- which.max(colMeans(netben))
      y <- netben - netben[cbind(1:n_sim), max_str]
    }
  }
  return(y)
}
