calculate_outcome <- function(outcome = c("nhb", "nmb", "eff", "cost", "nhb_loss", "nmb_loss"),
                              cost, effect, wtp) {
  outcome <- match.arg(outcome)
  n_sim <- nrow(cost)
  if (outcome == "eff") {
    y <- effect
  } else if (outcome == "cost") {
    y <- cost
  } else {
    if (is.null(wtp)) {
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
      max_str <- max.col(netben)
      y <-  netben[cbind(1:n_sim, max_str)] - netben
    }
  }
  return(y)
}
