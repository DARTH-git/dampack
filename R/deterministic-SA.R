#' Compute one-way sensitivity analysis (OWSA)
#'
#' @description This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#'
#' @param parms Vector with strings with the name of the parameters of interest
#' @param pars_df A data.frame with 4 columns with following column order: "pars",
#' "basecase", "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}
#' @param nsamps number of parameter values. If \code{NULL}, 100 parameter values are
#' used
#' @param FUN Function that takes the basecase in \code{pars_df} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param outcome_type The type of outcome is either "eff" or "cost". The default
#' outcome_type is "eff"
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#' @return A dataframe with the results of the sensitivity analysis. Can be
#' visualized with \code{plot.owsa}, \code{owsa_opt_strat} and
#' \code{owsa_tornado} from \code{dampack}
#'
#' @section Details:
#' \itemize{
#' \item \code{pars_df}
#' \itemize{
#' \item "pars" are the names of the input parameters in the
#' user defined function. "pars" should include all parameters of interest provided in
#' \code{parms}.
#' \item "basecase" are the base value of input parameters in user defined \code{FUN}.
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.
#' Users only need to provide the "min" and "max" of the parameters of interest.
#' For the rest of the parameter inputs into the user defined function,
#' "min" and "max" can be any value or \code{NA} but these values are not evaluated in
#' \code{owsa_det}}
#' }
#'
#' @export
owsa_det <- function(parms = NULL, pars_df, nsamps = 100, FUN, outcome,
                     outcome_type = "eff", strategies = NULL, ...){

  if (is.null(parms)) {
    parms <- as.character(pars_df[, 1])
    warning("assuming the pars in pars_df are the parameters of interest")
  }

  if (!is.data.frame(pars_df)) stop("pars_df must be a data.frame")

  if (ncol(pars_df) != 4) stop("pars_df must have 4 columns: 'pars', 'basecase', 'min', and 'max'")

  params_basecase <- pars_df[, 2]
  names(params_basecase) <- as.character(pars_df[, 1])
  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (!all(parms %in% pars_df[, 1])) {
    stop("parms should be in the parameters provided in pars_df")
  }

  if (!all(is.numeric(pars_df[, 2]), is.numeric(pars_df[, 3]), is.numeric(pars_df[, 4]))) {
    stop("basecase, min and max in pars_df must be numeric")
  }

  ix <- match(parms, pars_df$pars)
  if (!all( (pars_df[ix, 2] >= pars_df[ix, 3]) &
            (pars_df[ix, 2] <= pars_df[ix, 4]))) {
    stop("basecase has to be in between min and max")
  }

  names(pars_df) <- c("pars", "basecase", "min", "max")

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_ls)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))) {
    stop("FUN is not well defined by 'params_basecase' and ...")
  }

  userfun <- do.call(FUN, fun_input_ls)
  if (is.null(strategies)){
    strategies <- paste0("st_", userfun[, 1])
  }

  if (!is.data.frame(userfun)) {
    stop("FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
  }

  if (length(strategies) != length(userfun[, 1])){
    stop("number of strategies is not the same as the number of strategies in user defined FUN")
  }

  if (length(outcome) > 1) stop("only one outcome of interest is allowed once at a time")

  v_outcomes <- colnames(userfun)[-1]

  if (!(outcome %in% v_outcomes)){
    stop("outcome is not in FUN outcomes")
  }

  parm_table_all <- NULL
  sim_out_df_all <- NULL
  n_parms <- length(parms)

  for (i in 1:n_parms) {
    # Generate matrix of inputs
    pars_i <- parms[i]
    ix <- which(pars_df$pars == pars_i)
    pars_range <- pars_df[ix, c("min", "max")]
    v_owsa_input <- t(t(seq(pars_range[[1]],
                            pars_range[[2]],
                            length.out = nsamps)))
    colnames(v_owsa_input) <- pars_i

    # Run model and capture outcome
    sim_out <- lapply(c(1:nsamps),
                      wrapper_of_user_model,
                      user_fun = FUN,
                      parm_name = pars_i,
                      tmp_input = fun_input_ls,
                      tmp_replace = v_owsa_input)

    sim_out_df <- lapply(sim_out,
                         function(x, tmp_out = outcome) {
                           x[[outcome]]
                           })

    sim_out_df <- as.data.frame(do.call(rbind, sim_out_df))
    colnames(sim_out_df) <- strategies

    parm_table <- data.frame(parameter = rep(pars_i, nsamps),
                             parmval = unname(v_owsa_input))

    parm_table_all <- rbind(parm_table_all, parm_table)
    sim_out_df_all <- rbind(sim_out_df_all, sim_out_df)
  }

  parm_table_all$parameter <- as.character(parm_table_all$parameter)
  df_owsa <- create_dsa_oneway(parm_table_all, sim_out_df_all, strategies)

  owsa_out <- owsa(df_owsa, outcome = outcome_type)
  return(owsa_out)
}

#' Two-way sensitivity analysis (TWSA)
#'
#' @description This function runs a deterministic two-way sensitivity analysis (TWSA) on a
#' given function that produces outcomes.
#'
#' @param parm1 String with the name of the first parameter of interest
#' @param parm2 String with the name of the second parameter of interest
#' @param pars_df A data.frame with 4 columns with following column order: "pars",
#' "basecase", "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}
#' @param nsamps number of parameter values. If \code{NULL}, 40 parameter values are
#' used
#' @param FUN Function that takes the basecase in \code{pars_df} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param outcome_type The type of outcome is either "eff" or "cost". The default
#' outcome_type is "eff"
#' @param strategies vector of strategy names. The default (NULL) will use
#' strategy names in FUN
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#' @return A dataframe with the results of the sensitivity analysis. Can be
#' visualized with ??
#'
#' @section Details:
#' \itemize{
#' \item \code{pars_df}
#' \itemize{
#' \item "pars" are the names of the input parameters in the
#' user defined function. "pars" should include all parameters of interest provided in
#' \code{parms}.
#' \item "basecase" are the base value of input parameters in user defined \code{FUN}.
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.
#' Users only need to provide the "min" and "max" of the parameters of interest.
#' For the rest of the parameter inputs into the user defined function,
#' "min" and "max" can be any value or \code{NA} but these values are not evaluated in
#' \code{twsa_det}}
#' }
#'
#' @export
twsa_det <- function(parm1, parm2, pars_df, nsamps = 40, FUN, outcome,
                     outcome_type = "eff", strategies = NULL, ...){

  if (!is.data.frame(pars_df)) stop("pars_df must be a data.frame")

  if (ncol(pars_df) != 4) stop("pars_df must have 4 columns: 'pars', 'basecase', 'min', and 'max'")

  poi <- unique(c(parm1, parm2))
  params_basecase <- pars_df[, 2]
  names(params_basecase) <- as.character(pars_df[, 1])
  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (length(poi) != 2) {
    stop("two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")
  }

  if (!all(poi %in% names(params_basecase))){
    stop("parm1 and parm2 should be in the parameters provided in pars_df")
  }

  if (!all(is.numeric(pars_df[, 2]), is.numeric(pars_df[, 3]), is.numeric(pars_df[, 4]))) {
    stop("basecase, min and max in pars_df must be numeric")
  }

  ix <- match(poi, pars_df$pars)
  if (!all( (pars_df[ix, 2] >= pars_df[ix, 3]) &
            (pars_df[ix, 2] <= pars_df[ix, 4]))) {
    stop("basecase has to be in between min and max")
  }

  names(pars_df) <- c("pars", "basecase", "min", "max")

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_ls)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by the basecase parameter values and ...")
  }

  userfun <- do.call(FUN, fun_input_ls)
  if (is.null(strategies)){
    strategies <- paste0("st_", userfun[, 1])
  }

  if (!is.data.frame(userfun)) {
    stop("FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
  }

  if (length(strategies) != length(userfun[, 1])){
    stop("number of strategies is not the same as the number of strategies in user defined FUN")
  }

  if (length(outcome) > 1) stop("only one outcome of interest is allowed once at a time")

  v_outcomes <- colnames(userfun)[-1]

  if (!(outcome %in% v_outcomes)){
    stop("outcome is not part of FUN outcomes")
  }

  ### Generate matrix of inputs
  range_df <- pars_df[ix, c("min", "max")]
  param_table <- expand.grid(parm1 = seq(range_df[1, "min"],
                                         range_df[1, "max"],
                                         length.out = nsamps),
                             parm2 = seq(range_df[2, "min"],
                                         range_df[2, "max"],
                                         length.out = nsamps))
  colnames(param_table) <- poi

  # Run model and capture outcome
  n_run <- nrow(param_table)
  sim_out <- lapply(c(1:n_run),
                    wrapper_of_user_model,
                    user_fun = FUN,
                    parm_name = poi,
                    tmp_input = fun_input_ls,
                    tmp_replace = param_table)

  sim_out_df <- lapply(sim_out,
                       function(x, tmp_out = outcome) {
                         x[[outcome]]
                       })
  sim_out_df <- as.data.frame(do.call(rbind, sim_out_df))
  colnames(sim_out_df) <- strategies

  df_twsa <- create_dsa_twoway(param_table, sim_out_df, strategies)

  twsa_out <- twsa(df_twsa, outcome = outcome_type)

  return(twsa_out)
}


#' Wrapper function for owsa_det and twsa_det
#'
#' @param x iteration
#' @param user_fun \code{FUN} input from users
#' @param parm_name user-defined list of parameters of interest
#' @param tmp_input basecase values
#' @param tmp_replace values from predetermined PSA samples that will replace some values in \code{tmp_input}
#'
#' @keywords internal
wrapper_of_user_model <- function(x, user_fun, parm_name,
                                  tmp_input, tmp_replace) {
  tmp_input[[1]][parm_name] <- tmp_replace[x, parm_name]
  do.call(user_fun, tmp_input)
}
