#' Create two-way deterministic sensitivity analysis object
#'
#' @description The object returned by this function can be passed to
#' \code{\link{twsa}} to do a two-way sensitivity analysis on the two
#' parameters of interest.
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
#' @return a class \code{dsa_twoway} object that can be passed to the \code{\link{twsa}} function
#' to visualize the two-way sensitivity analysis contained in the object.
#'
#' @seealso \code{\link{run_twsa_det}}, which runs a model function over the parameter ranges,
#' builds this object internally, and returns the result of \code{\link{twsa}}.
#'
#' @examples
#' # Outcomes of two strategies on a 5 x 5 grid of the failure probability p_fail and
#' # the cost of the new treatment c_trt
#' df_params <- expand.grid(p_fail = seq(0.1, 0.5, length.out = 5),
#'                          c_trt = seq(5000, 25000, length.out = 5))
#' df_cost <- data.frame(Standard = 20000 * df_params$p_fail,
#'                       New = df_params$c_trt + 10000 * df_params$p_fail)
#' df_effect <- data.frame(Standard = 10 - 2 * df_params$p_fail,
#'                         New = 10 - df_params$p_fail)
#'
#' l_dsa <- create_dsa_twoway(parameters = df_params,
#'                            effectiveness = df_effect,
#'                            strategies = c("Standard", "New"),
#'                            cost = df_cost)
#' l_dsa
#'
#' # strategy with the highest net monetary benefit at a WTP of 50,000
#' df_twsa <- twsa(l_dsa, outcome = "nmb", wtp = 5e4)
#' plot(df_twsa)
#' @export
create_dsa_twoway <- function(parameters, effectiveness = NULL, strategies,
                              cost = NULL, currency = "$", other_outcome  = NULL) {
  # parameter names and dataframe structure
  parameters <- check_df_and_coerce(parameters)
  v_parnames <- colnames(parameters)
  if (is.null(v_parnames)) {
    stop("parameter dataframe must have column names")
  }

  if (ncol(parameters) != 2) {
    stop(paste0("Two-way sensitivity analysis only supported for 2 parameters.",
                "parameter dataframe must have exactly 2 columns"))
  }

  # create dsa object
  dsa <- create_sa(parameters, v_parnames, effectiveness, strategies,
                   cost, currency, other_outcome)
  class(dsa) <- c("dsa_twoway", class(dsa))
  dsa
}
