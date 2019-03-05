
calculate_net_benefit <- function(outcome = c("nhb", "nmb"),
                                  cost, effect, wtp) {
  if (is.null(wtp)) {
    stop("wtp must be provided",  call. = FALSE)
  }
  if (is.null(cost)) {
    stop("must provide cost for NHB and NMB.",  call. = FALSE)
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
