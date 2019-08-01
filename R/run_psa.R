#' Calculate outcomes for a PSA using a user-defined function.
#'
#' @description \code{run_psa} calculates outcomes using a user-defined function and creates PSA objects
#' corresponding to the specified outcomes.
#'
#' @param psa_samp A dataframe with samples of parameters for a probabilistic sensitivity analysis (PSA)
#' @param FUN Function that takes the parameter values in \code{psa_samp} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}.
#' @param cost_outcome String within \code{outcomes} that is designated as the cost outcome
#' in the creation of \code{psa} objects for use in cost-effectiveness analyses.
#' @param effectiveness_outcome String within \code{outcomes} that is designated as the effectiveness outcome
#' in the creation of \code{psa} objects for use in cost-effectiveness analyses.
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param currency symbol for the currency being used (ex. "$", "£")
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#'
#' @return
#' A list containing PSA objects for each outcome in \code{outcomes},
#' as well as an additional PSA object containing both effectiveness and cost for CEA
#' if cost_outcome and effectiveness_outcome were both supplied.
#'
#' @seealso
#' \code{\link{run_psa}},
#' \code{\link{make_psa_obj}},
#' \code{\link{gen_psa_samp}},
#'
#' @examples
#'
#' #load package
#' library(dampack)
#'
#' #example of function that produces output required by run_psa
#' test_func <- function(params, extra_param) {
#'   normal_param <- params[["normal_param"]]
#'   lognorm_param <- params[["lognorm_param"]]
#'   beta_param <- params[["beta_param"]]
#'   gamma_param <- params[["gamma_param"]]
#'   level1 <- params[["level1"]]
#'   level2 <- params[["level2"]]
#'   level3 <- params[["level3"]]
#'   bootstrap_param <- params[["bootstrap_param"]]

#'  effect1 <- normal_param + lognorm_param * extra_param + beta_param + gamma_param +
#'    level1 + level2 + level3 + bootstrap_param
#'  cost1 <- - normal_param - lognorm_param - beta_param - gamma_param
#'   - level1 - level2 - level3 - bootstrap_param
#'
#'  effect2 <- normal_param - lognorm_param * extra_param + beta_param - gamma_param +
#'             level1 - level2 + level3 - bootstrap_param
#'  cost2 <- - normal_param + lognorm_param - beta_param + gamma_param
#'           - level1 + level2 - level3 + bootstrap_param
#'
#'  output <- data.frame(strategies = c("mystrat1", "mystrat2"),
#'                       effect = c(effect1, effect2),
#'                       cost = c(cost1, cost2))
#'
#'  return(output)
#'}
#'
#' #generate parameter data.frame from parent distributions
#' psa_df <- gen_psa_samp(params = c("normal_param", "lognorm_param", "beta_param",
#'                                "gamma_param", "dirichlet_param", "bootstrap_param"),
#'                     dist = c("normal", "log-normal", "beta",
#'                      "gamma", "dirichlet", "bootstrap"),
#'                     parameterization_type = c("mean, sd", "mean, sd", "mean, sd", "mean, sd",
#'                                               "value, mean_prop, sd", "value, weight"),
#'                     dist_params = list(c(1, 2), c(1, 3), c(.5, .2), c(100, 1),
#'                                        data.frame(value = c("level1", "level2", "level3"),
#'                                                   mean_prop = c(.1, .4, .5),
#'                                                   sd = c(.05, .01, .1)),
#'                                        data.frame(value = c(1, 2, 4, 6, 7, 8),
#'                                                   weight = c(1, 1, 1, 1, 1, 4))),
#'                     nsamp = 100)
#'
#'#run psa using psa parameter data.frame
#'run_psa(psa_df, test_func, outcomes = c("cost", "effect"), cost_outcome = "cost",
#'        effectiveness_outcome = "effect", strategies = c("customstrat1", "customstrat2"),
#'        currency = "$", extra_param = 1.5)
#'
#'
#' @export

run_psa <- function(psa_samp, FUN, outcomes = NULL, cost_outcome = NULL,
                    effectiveness_outcome = NULL,
                    strategies = NULL, currency = "$", ...){

  opt_arg_val <- list(...)
  fun_input_test <- c(list(psa_samp[1, ]), opt_arg_val)

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_test)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by the parameter values and ...")
  }
  userfun <- do.call(FUN, fun_input_test)

  if (is.null(strategies)){
    strategies <- paste0("st_", userfun[, 1])
  }

  if (!is.data.frame(userfun)) {
    stop("FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
  }

  if (length(strategies) != length(userfun[, 1])){
    stop("number of strategies is not the same as the number of strategies in user defined FUN")
  }

  v_outcomes <- colnames(userfun)[-1]

  if (!all(outcomes %in% v_outcomes)){
    stop("at least one outcome is not in FUN outcomes")
  }


  sim_out_ls <- vector(mode = "list", length = nrow(psa_samp))

  for (i in 1:nrow(psa_samp)){
    fun_input_ls <- c(list(psa_samp[i, ]), opt_arg_val)
    sim_out_ls[[i]] <- do.call(FUN, fun_input_ls)
  }

  n_outcomes <- length(outcomes)
  sim_out_df <- vector(mode = "list", length = n_outcomes)
  names(sim_out_df) <- outcomes
  for (j in 1:n_outcomes){
    sim_out_df[[j]] <- lapply(sim_out_ls,
                              function(x, tmp_out = outcomes[j]) {
                                x[[outcomes[j]]]
                              })
    sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
    colnames(sim_out_df[[j]]) <- strategies
  }



    psa_out <- vector(mode = "list", length = n_outcomes)
    for (j in 1:n_outcomes){
      psa_out[[j]] <- make_psa_obj(cost = NULL, effectiveness = sim_out_df[[j]],
                                   parameters = psa_samp[, -1], strategies = strategies,
                                   currency = "$")
    }

    names(psa_out) <- outcomes

    if (!is.null(cost_outcome) & !is.null(effectiveness_outcome)) {
      cea_psa <- make_psa_obj(cost = sim_out_df[[cost_outcome]],
                              effectiveness = sim_out_df[[effectiveness_outcome]],
                              parameters = psa_samp[, -1], strategies = strategies,
                              currency = currency)
      psa_out <- append(list(cea_psa), psa_out)
      names(psa_out) <- c("cea_psa", outcomes)
    }


    return(psa_out)
  }
