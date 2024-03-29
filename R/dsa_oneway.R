#' Create one-way deterministic sensitivity analysis object
#'
#' @description The object returned by this function can be passed to
#' \code{\link{owsa}} to do a one-way sensitivity analysis on each
#' parameter of interest.
#'
#' @inheritParams create_sa
#'
#' @param parameters parameter values associated with costs, effectiveness, or other outcomes.
#' The table must have two columns, with each parameter name in the first column
#' and the associated parameter value in the second column:
#'
#' \tabular{ll}{
#' parameter      \tab value \cr
#' param1 name     \tab param1 val1 \cr
#' ...            \tab ... \cr
#' param2 name     \tab  param2 val1 \cr
#' ...            \tab ... \cr
#' }
#'
#' @return a class \code{dsa_oneway} object that can be passed to the \code{\link{owsa}} function
#' to visualize the one-way sensitivity analyses contained in the object.
#' @export
create_dsa_oneway <- function(parameters, effectiveness = NULL, strategies,
                              cost = NULL, currency = "$", other_outcome = NULL) {
  # parameter names
  colnames(parameters) <- c("parameter", "paramval")
  parnames <- unique(parameters$parameter)

  # check object structure and define dsa
  dsa <- create_sa(parameters, parnames, effectiveness,
                   strategies, cost, currency, other_outcome)
  class(dsa) <- c("dsa_oneway", class(dsa))
  dsa
}
