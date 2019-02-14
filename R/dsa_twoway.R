#' create two-way deterministic sensitivity analysis object
#'
#' @param params parameter values associated with
create_dsa_twoway <- function(params, effectiveness, strategies,
                              cost = NULL, currency = "$") {
  # todo: checks on input
  dsa <- list(
    params = params,
    effectiveness = effectiveness,
    strategies = strategies,
    cost = cost,
    currency = currency
  )
  class(dsa) <- "dsa_twoway"
  dsa
}
