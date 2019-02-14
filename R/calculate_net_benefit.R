
calculate_net_benefit <- function(outcome = c("nhb", "nmb"),
                                  cost, effect, wtp) {
  if (is.null(wtp)) {
    stop("wtp must be provided")
  }
  outcome <- match.arg(outcome)
  if (outcome == "nhb") {
    y <- effect - cost / wtp
  }
  if (outcome == "nmb") {
    y <- effect * wtp - cost
  }
  return(y)
}
