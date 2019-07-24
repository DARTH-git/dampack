#' Compute one-way sensitivity analysis (OWSA)
#'
#' @description This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#'
#' @param params Vector with strings with the name of the parameters of interest
#' @param params_all A data.frame with 4 columns with following column order: "pars",
#' "basecase", "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}
#' @param nsamps number of parameter values. If \code{NULL}, 100 parameter values are
#' used
#' @param FUN Function that takes the basecase in \code{params_all} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}
#'  produced by \code{nsamp}
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#' @return A list containing dataframes with the results of the sensitivity analyses.
#' The list will contain a dataframe for each outcome specified.
#' List elements can be visualized with \code{plot.owsa},
#' \code{owsa_opt_strat} and
#' \code{owsa_tornado} from \code{dampack}
#'
#' @section Details:
#' \itemize{
#' \item \code{params_all}
#' \itemize{
#' \item "pars" are the names of the input parameters in the
#' user defined function. "pars" should include all parameters of interest provided in
#' \code{params}.
#' \item "basecase" are the base value of input parameters in user defined \code{FUN}.
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.
#' Users only need to provide the "min" and "max" of the parameters of interest.
#' For the rest of the parameter inputs into the user defined function,
#' "min" and "max" can be any value or \code{NA} but these values are not evaluated in
#' \code{owsa_det}}
#' }
#'
#' @export
owsa_det <- function(params = NULL, params_all, nsamps = 100, FUN,
                     outcomes = NULL, strategies = NULL, ...){

  if (is.null(params)) {
    params <- as.character(params_all[, 1])
    warning("assuming the pars in params_all are the parameters of interest")
  }

  if (!is.data.frame(params_all)) stop("params_all must be a data.frame")

  if (ncol(params_all) != 4) stop("params_all must have 4 columns: 'pars', 'basecase', 'min', and 'max'")

  params_basecase <- params_all[, 2]
  names(params_basecase) <- as.character(params_all[, 1])
  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (!all(params %in% params_all[, 1])) {
    stop("params should be in the parameters provided in params_all")
  }

  if (!all(is.numeric(params_all[, 2]), is.numeric(params_all[, 3]), is.numeric(params_all[, 4]))) {
    stop("basecase, min and max in params_all must be numeric")
  }

  ix <- match(params, params_all$pars)
  if (!all( (params_all[ix, 2] >= params_all[ix, 3]) &
            (params_all[ix, 2] <= params_all[ix, 4]))) {
    stop("basecase has to be in between min and max")
  }

  names(params_all) <- c("pars", "basecase", "min", "max")

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


  v_outcomes <- colnames(userfun)[-1]

  if (!all(outcomes %in% v_outcomes)){
    stop("at least one outcome is not in FUN outcomes")
    }


  param_table_all <- NULL
  sim_out_df <- NULL
  n_params <- length(params)
  n_outcomes <- length(outcomes)
  sim_out_df_all <- vector(mode = "list", length = n_outcomes)

  for (i in 1:n_params) {
    # Generate matrix of inputs
    pars_i <- params[i]
    ix <- which(params_all$pars == pars_i)
    pars_range <- params_all[ix, c("min", "max")]
    v_owsa_input <- t(t(seq(pars_range[[1]],
                            pars_range[[2]],
                            length.out = nsamps)))
    colnames(v_owsa_input) <- pars_i

    # Run model and capture outcome(s)
    sim_out <- lapply(c(1:nsamps),
                      wrapper_of_user_model,
                      user_fun = FUN,
                      param_name = pars_i,
                      tmp_input = fun_input_ls,
                      tmp_replace = v_owsa_input)

    for (j in 1:n_outcomes){
      sim_out_df[[j]] <- lapply(sim_out,
                                function(x, tmp_out = outcomes[j]) {
                                  x[[outcomes[j]]]
                                })
      sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
      colnames(sim_out_df[[j]]) <- strategies
      sim_out_df_all[[j]] <- rbind(sim_out_df_all[[j]], sim_out_df[[j]])
    }

    param_table <- data.frame(parameter = rep(pars_i, nsamps),
                              paramval = unname(v_owsa_input))

    param_table_all <- rbind(param_table_all, param_table)
  }

  param_table_all$parameter <- as.character(param_table_all$parameter)

  df_owsa <- vector(mode = "list", length = n_outcomes)
  owsa_out <- vector(mode = "list", length = n_outcomes)
  for (k in 1:n_outcomes){
    df_owsa[[k]] <- create_dsa_oneway(param_table_all, sim_out_df_all[[k]], strategies)
    owsa_out[[k]] <- owsa(df_owsa[[k]], outcome = "eff")
  }

  names(owsa_out) <- paste0("owsa_", outcomes)

if (n_outcomes == 1) {
  owsa_out <- owsa_out[[1]]
}

  return(owsa_out)
}

#' Two-way sensitivity analysis (TWSA)
#'
#' @description This function runs a deterministic two-way sensitivity analysis (TWSA) on a
#' given function that produces outcomes.
#'
#' @param param1 String with the name of the first parameter of interest
#' @param param2 String with the name of the second parameter of interest
#' @param params_all A data.frame with 4 columns with following column order: "pars",
#' "basecase", "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}
#' @param nsamps number of parameter values. If \code{NULL}, 40 parameter values are
#' used
#' @param FUN Function that takes the basecase in \code{params_all} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}
#'  produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use
#' strategy names in FUN
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#' @return A list containing dataframes with the results of the sensitivity analyses.
#' The list will contain a dataframe for each outcome specified.
#'
#' @section Details:
#' \itemize{
#' \item \code{params_all}
#' \itemize{
#' \item "pars" are the names of the input parameters in the
#' user defined function. "pars" should include all parameters of interest provided in
#' \code{params}.
#' \item "basecase" are the base value of input parameters in user defined \code{FUN}.
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.
#' Users only need to provide the "min" and "max" of the parameters of interest.
#' For the rest of the parameter inputs into the user defined function,
#' "min" and "max" can be any value or \code{NA} but these values are not evaluated in
#' \code{twsa_det}}
#' }
#'
#' @export
twsa_det <- function(param1, param2, params_all, nsamps = 40, FUN, outcomes = NULL,
                     strategies = NULL, ...){

  if (!is.data.frame(params_all)) stop("params_all must be a data.frame")

  if (ncol(params_all) != 4) stop("params_all must have 4 columns: 'pars', 'basecase', 'min', and 'max'")

  poi <- unique(c(param1, param2))
  params_basecase <- params_all[, 2]
  names(params_basecase) <- as.character(params_all[, 1])
  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (length(poi) != 2) {
    stop("two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")
  }

  if (!all(poi %in% names(params_basecase))){
    stop("param1 and param2 should be in the parameters provided in params_all")
  }

  if (!all(is.numeric(params_all[, 2]), is.numeric(params_all[, 3]), is.numeric(params_all[, 4]))) {
    stop("basecase, min and max in params_all must be numeric")
  }

  ix <- match(poi, params_all$pars)
  if (!all( (params_all[ix, 2] >= params_all[ix, 3]) &
            (params_all[ix, 2] <= params_all[ix, 4]))) {
    stop("basecase has to be in between min and max")
  }

  names(params_all) <- c("pars", "basecase", "min", "max")

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

  v_outcomes <- colnames(userfun)[-1]

  if (!all(outcomes %in% v_outcomes)){
      stop("at least one outcome is not in FUN outcomes")
  }

  n_outcomes <- length(outcomes)
  sim_out_df <- NULL

  ### Generate matrix of inputs
  range_df <- params_all[ix, c("min", "max")]
  param_table <- expand.grid(param1 = seq(range_df[1, "min"],
                                         range_df[1, "max"],
                                         length.out = nsamps),
                             param2 = seq(range_df[2, "min"],
                                         range_df[2, "max"],
                                         length.out = nsamps))
  colnames(param_table) <- poi

  # Run model and capture outcome
  n_run <- nrow(param_table)
  sim_out <- lapply(c(1:n_run),
                    wrapper_of_user_model,
                    user_fun = FUN,
                    param_name = poi,
                    tmp_input = fun_input_ls,
                    tmp_replace = param_table)

  for (j in 1:n_outcomes){
    sim_out_df[[j]] <- lapply(sim_out,
                              function(x, tmp_out = outcomes[j]) {
                                x[[outcomes[j]]]
                              })
    sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
    colnames(sim_out_df[[j]]) <- strategies
  }


  df_twsa <- vector(mode = "list", length = n_outcomes)
  twsa_out <- vector(mode = "list", length = n_outcomes)

  for (k in 1:n_outcomes){
    df_twsa[[k]] <- create_dsa_twoway(param_table, sim_out_df[[k]], strategies)
    twsa_out[[k]] <- twsa(df_twsa[[k]], outcome = "eff")
  }

  names(twsa_out) <- paste0("twsa_", outcomes)

  if (n_outcomes == 1) {
    twsa_out <- twsa_out[[1]]
  }

  return(twsa_out)
}


#' Wrapper function for owsa_det and twsa_det
#'
#' @param x iteration
#' @param user_fun \code{FUN} input from users
#' @param param_name user-defined list of parameters of interest
#' @param tmp_input basecase values
#' @param tmp_replace values from predetermined PSA samples that will replace some values in \code{tmp_input}
#'
#' @keywords internal
wrapper_of_user_model <- function(x, user_fun, param_name,
                                  tmp_input, tmp_replace) {
  tmp_input[[1]][param_name] <- tmp_replace[x, param_name]
  do.call(user_fun, tmp_input)
}
