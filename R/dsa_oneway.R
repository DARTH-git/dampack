#' Create one-way deterministic sensitivity analysis object
#'
#' @description The object returned by this function can be passed to
#' \code{\link{owsa}} to do a one-way sensitivity analysis on each
#' parameter of interest.
#'
#' @inheritParams create_sa
#'
#' @param parameters parameter values associated with effectiveness and outcomes.
#' The table must have two columns, with each parameter name in the first column
#' and the associated parameter value in the second column:
#'
#' \tabular{ll}{
#' parameter      \tab value \cr
#' parm1 name     \tab parm1 val1 \cr
#' ...            \tab ... \cr
#' parm2 name     \tab  parm2 val1 \cr
#' ...            \tab ... \cr
#' }
#'
#' @export
create_dsa_oneway <- function(parameters, effectiveness, strategies,
                              cost = NULL, currency = "$") {
  # parameter names
  colnames(parameters) <- c("parameter", "parmval")
  parnames <- unique(parameters$parameter)

  # check object structure and define dsa
  dsa <- create_sa(parameters, parnames, effectiveness,
                   strategies, cost, currency)
  class(dsa) <- c("dsa_oneway", class(dsa))
  dsa
}
