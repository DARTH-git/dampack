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
#'
#' @seealso \code{\link{run_owsa_det}}, which runs a model function over the parameter ranges,
#' builds this object internally, and returns the result of \code{\link{owsa}}.
#'
#' @examples
#' # Outcomes of two strategies, evaluated at 5 values of the failure probability p_fail
#' # (with c_trt at its base case of 15,000) and at 5 values of the cost of the new
#' # treatment c_trt (with p_fail at its base case of 0.3)
#' v_p_fail <- seq(0.1, 0.5, length.out = 5)
#' v_c_trt <- seq(5000, 25000, length.out = 5)
#' df_params <- data.frame(parameter = rep(c("p_fail", "c_trt"), each = 5),
#'                         value = c(v_p_fail, v_c_trt))
#' v_p_fail_all <- c(v_p_fail, rep(0.3, 5))
#' v_c_trt_all <- c(rep(15000, 5), v_c_trt)
#' df_cost <- data.frame(Standard = 20000 * v_p_fail_all,
#'                       New = v_c_trt_all + 10000 * v_p_fail_all)
#' df_effect <- data.frame(Standard = 10 - 2 * v_p_fail_all,
#'                         New = 10 - v_p_fail_all)
#'
#' l_dsa <- create_dsa_oneway(parameters = df_params,
#'                            effectiveness = df_effect,
#'                            strategies = c("Standard", "New"),
#'                            cost = df_cost)
#' l_dsa
#'
#' # one-way sensitivity analysis of the net monetary benefit at a WTP of 50,000
#' df_owsa <- owsa(l_dsa, outcome = "nmb", wtp = 5e4)
#' plot(df_owsa)
#' owsa_opt_strat(df_owsa)
#' @export
create_dsa_oneway <- function(parameters, effectiveness = NULL, strategies,
                              cost = NULL, currency = "$", other_outcome = NULL) {
  # parameter names
  colnames(parameters) <- c("parameter", "paramval")
  v_parnames <- unique(parameters$parameter)

  # check object structure and define dsa
  dsa <- create_sa(parameters, v_parnames, effectiveness,
                   strategies, cost, currency, other_outcome)
  class(dsa) <- c("dsa_oneway", class(dsa))
  dsa
}
