#' Create one-way deterministic sensitivity analysis object
#'
#' @description The object returned by this function can be passed to
#' \code{\link{owsa}} to do a one-way sensitivity analysis on each
#' parameter of interest.
#'
#' @inheritParams create_sa
#'
#' @param parameters parameter values associated with effectiveness and outcomes.
#' The table must have two columns, one for each parameter.
#' The parameter names must be the column names.
#'
#' \tabular{ll}{
#' param1 name     \tab param2 name \cr
#' param1 val1     \tab param2 val1 \cr
#' param1 val2     \tab param2 val2 \cr
#' ...            \tab ... \cr
#' }
#'
#' @export
create_dsa_twoway <- function(parameters, effectiveness = NULL, strategies,
                              cost = NULL, currency = "$", other_outcome  = NULL) {
  # parameter names and dataframe structure
  parameters <- check_df_and_coerce(parameters)
  parnames <- colnames(parameters)
  if (is.null(parnames)) {
    stop("parameter dataframe must have column names")
  }

  if (ncol(parameters) != 2) {
    stop(paste0("Two-way sensitivity analysis only supported for 2 parameters.",
                "parameter dataframe must have exactly 2 columns"))
  }

  # create dsa object
  dsa <- create_sa(parameters, parnames, effectiveness, strategies,
                   cost, currency, other_outcome)
  class(dsa) <- c("dsa_twoway", class(dsa))
  dsa
}
