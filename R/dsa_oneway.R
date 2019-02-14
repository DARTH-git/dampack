#' create deterministic sensitivity analysis object
#'
#' @param params parameter values associated with
#' @export
create_dsa_oneway <- function(params, effectiveness, strategies,
                              cost = NULL, currency = "$") {
  # todo: checks on input
  dsa <- list(
    params = params,
    effectiveness = effectiveness,
    strategies = strategies,
    cost = cost,
    currency = currency
  )
  class(dsa) <- "dsa_oneway"
  dsa
}
