#' Run deterministic one-way sensitivity analysis (OWSA)
#'
#' @description This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#'
#' @param params_range data.frame with 3 columns in the following order: "pars",
#' "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}. "pars" are the parameters of interest and must be a subset of
#' the parameters from \code{params_basecase}.
#' @param params_basecase a named list of basecase values for input parameters needed by \code{FUN},
#' the user-defined function.
#' @param nsamp number of sets of parameter values to be generated. If \code{NULL}, 100 parameter
#' values are used
#' @param FUN function that takes the basecase in \code{params_basecase} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes string vector with the outcomes of interest from \code{FUN}
#'  produced by \code{nsamp}
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#'  should be displayed in console.
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
#' \item \code{params_range}
#' \itemize{
#' \item "pars" are the names of the input parameters of interest. These are the parameters that will
#'  be varied in the deterministic sensitivity analysis. variables in "pars" column
#'  must be a subset of variables in \code{params_basecase}
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.}
#' }
#'
#' @importFrom  utils txtProgressBar
#' @export
run_owsa_det <- function(params_range, params_basecase, nsamp = 100, FUN,
                     outcomes = NULL, strategies = NULL, progress = TRUE,
                     ...) {
  params <- as.character(params_range[, 1])

  if (!is.data.frame(params_range)) stop("params_range must be a data.frame")

  if (ncol(params_range) != 3) stop("params_all must have 3 columns: 'pars', 'min', and 'max'")

  if (!is.list(params_basecase) | is.null(names(params_basecase))) {
    stop("params_basecase must be a named list")
  }

  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (!all(params %in% names(params_basecase))) {
    stop("the first column of params_range should consist only of parameter names from params_basecase")
  }

  if (!all(is.numeric(params_range[, 2]), is.numeric(params_range[, 3]), sapply(params_basecase[params_range[, "pars"]], is.numeric))) {
    stop("min and max in params_range and elements of params_basecase in pars must be numeric")
  }

  if (!all((params_basecase[params] >= params_range[, 2]) &
         (params_basecase[params] <= params_range[, 3]))) {
    stop("basecase has to be in between min and max")
  }

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_ls)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))) {
    stop("FUN is not well defined by 'params_basecase' and ...")
  }

  userfun <- do.call(FUN, fun_input_ls)
  if (is.null(strategies)) {
    strategies <- paste0("st_", userfun[, 1])
  }

  if (!is.data.frame(userfun)) {
    stop("FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
  }

  if (length(strategies) != length(userfun[, 1])) {
    stop("number of strategies is not the same as the number of strategies in user defined FUN")
  }


  v_outcomes <- colnames(userfun)[-1]

  if (!all(outcomes %in% v_outcomes)) {
    stop("at least one outcome is not in FUN outcomes")
    }
  if (is.null(outcomes)) outcomes <- v_outcomes

  param_table_all <- NULL
  sim_out_df <- NULL
  n_params <- nrow(params_range)
  n_outcomes <- length(outcomes)
  sim_out_df_all <- vector(mode = "list", length = n_outcomes)

  for (i in 1:n_params) {
    # Generate matrix of inputs
    pars_i <- params[i]
    pars_range <- params_range[i, c("min", "max")]
    v_owsa_input <- t(t(seq(pars_range[[1]],
                            pars_range[[2]],
                            length.out = nsamp)))
    colnames(v_owsa_input) <- pars_i

    # Define progress bar
    if (progress == TRUE) {
      pb <- txtProgressBar(min = 0, max = nsamp * n_params, style = 3)
    } else {
      pb <- NULL
    }

    # Run model and capture outcome(s)
    sim_out <- lapply(c(1:nsamp),
                      wrapper_of_user_model,
                      user_fun = FUN,
                      param_name = pars_i,
                      tmp_input = fun_input_ls,
                      tmp_replace = v_owsa_input,
                      progress_bar = pb,
                      param_counter = i,
                      nsamp = nsamp)

    for (j in 1:n_outcomes) {
      sim_out_df[[j]] <- lapply(sim_out,
                                function(x, tmp_out = outcomes[j]) {
                                  t(x[outcomes[j]])
                                })
      sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
      colnames(sim_out_df[[j]]) <- strategies
      sim_out_df_all[[j]] <- rbind(sim_out_df_all[[j]], sim_out_df[[j]])
    }

    param_table <- data.frame(parameter = rep(pars_i, nsamp),
                              paramval = unname(v_owsa_input))

    param_table_all <- rbind(param_table_all, param_table)
  }

  param_table_all$parameter <- as.character(param_table_all$parameter)

  df_owsa <- vector(mode = "list", length = n_outcomes)
  owsa_out <- vector(mode = "list", length = n_outcomes)
  for (k in 1:n_outcomes) {
    df_owsa[[k]] <- create_dsa_oneway(parameters = param_table_all,
                                      other_outcome = sim_out_df_all[[k]],
                                      strategies = strategies)
    owsa_out[[k]] <- owsa(df_owsa[[k]], outcome = "eff")
  }

  names(owsa_out) <- paste0("owsa_", outcomes)

if (n_outcomes == 1) {
  owsa_out <- owsa_out[[1]]
}

  return(owsa_out)
}

#' Run deterministic two-way sensitivity analysis (TWSA)
#'
#' @description This function runs a deterministic two-way sensitivity analysis (TWSA) on a
#' given function that produces outcomes.
#'
#' @param params_range data.frame with 2 rows and 3 columns in the following order: "pars",
#' "min", and "max". The number of samples from this range is
#' determined by \code{nsamp}. "pars" are the 2 parameters of interest, which must be a subset of
#' the parameters from \code{params_basecase}.
#' @param params_basecase a named list of basecase values for input parameters needed by \code{FUN},
#' the user-defined function.
#' @param nsamp number of parameter values. If \code{NULL}, 40 parameter values are
#' used
#' @param FUN Function that takes the basecase in \code{params_all} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}
#'  produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use
#' strategy names in FUN
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#'  should be displayed in console.
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#' @return A list containing dataframes with the results of the sensitivity analyses.
#' The list will contain a dataframe for each outcome specified.
#'
#' @section Details:
#' \itemize{
#' \item \code{params_range}
#' \itemize{
#' \item "pars" are the names of the two input parameters of interest. The two variables in "pars" column
#'  must be a subset of variables in \code{params_basecase}
#' \item "min" and "max" are the mininum and maximum values of the parameters of interest.}
#' }
#'
#' @importFrom  utils txtProgressBar
#' @export
run_twsa_det <- function(params_range, params_basecase, nsamp = 40, FUN, outcomes = NULL,
                     strategies = NULL, progress = TRUE, ...) {
  if (!is.data.frame(params_range)) stop("params_range must be a data.frame")

  if (ncol(params_range) != 3) stop("params_all must have 3 columns: 'pars', 'min', and 'max'")

  if (!is.list(params_basecase) | is.null(names(params_basecase)))
    stop("params_basecase must be a named list")

  poi <- unique(as.character(params_range[, 1]))

  opt_arg_val <- list(...)
  fun_input_ls <- c(list(params_basecase), opt_arg_val)

  if (length(poi) != 2) {
    stop("two-way sensitivity analysis only allows for and requires 2 different paramters of interest at a time")
  }

  if (!all(poi %in% names(params_basecase))) {
    stop("the first column of params_range should consist only of parameter names from params_basecase")
  }

  if (!all(is.numeric(params_range[, 2]), is.numeric(params_range[, 3]), sapply(params_basecase[params_range[, "pars"]], is.numeric))) {
    stop("min and max in params_range and elements of params_basecase in pars must be numeric")
  }

  if (!all((params_basecase[poi] >= params_range[, 2]) &
           (params_basecase[poi] <= params_range[, 3]))) {
    stop("basecase has to be in between min and max")
  }

  names(params_range) <- c("pars", "min", "max")

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_ls)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))) {
    stop("FUN is not well defined by the basecase parameter values and ...")
  }

  userfun <- do.call(FUN, fun_input_ls)
  if (is.null(strategies)) {
    strategies <- paste0("st_", userfun[, 1])
  }

  if (!is.data.frame(userfun)) {
    stop("FUN should return a data.frame with >= 2 columns. 1st column is strategy name; the rest are outcomes")
  }

  if (length(strategies) != length(userfun[, 1])) {
    stop("number of strategies is not the same as the number of strategies in user defined FUN")
  }

  v_outcomes <- colnames(userfun)[-1]

  if (!all(outcomes %in% v_outcomes)) {
      stop("at least one outcome is not in FUN outcomes")
  }
  if (is.null(outcomes)) outcomes <- v_outcomes

  n_outcomes <- length(outcomes)
  sim_out_df <- NULL

  ### Generate matrix of inputs
  range_df <- params_range[, c("min", "max")]
  param_table <- expand.grid(param1 = seq(range_df[1, "min"],
                                         range_df[1, "max"],
                                         length.out = nsamp),
                             param2 = seq(range_df[2, "min"],
                                         range_df[2, "max"],
                                         length.out = nsamp))
  colnames(param_table) <- poi

  # Run model and capture outcome
  n_run <- nrow(param_table)

  # Define progress bar
  if (progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = n_run, style = 3)
  } else {
    pb <- NULL
  }


  sim_out <- lapply(c(1:n_run),
                    wrapper_of_user_model,
                    user_fun = FUN,
                    param_name = poi,
                    tmp_input = fun_input_ls,
                    tmp_replace = param_table,
                    progress_bar = pb,
                    param_counter = 1,
                    nsamp = n_run)

  for (j in 1:n_outcomes) {
    sim_out_df[[j]] <- lapply(sim_out,
                              function(x, tmp_out = outcomes[j]) {
                                t(x[outcomes[j]])
                              })
    sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
    colnames(sim_out_df[[j]]) <- strategies
  }


  df_twsa <- vector(mode = "list", length = n_outcomes)
  twsa_out <- vector(mode = "list", length = n_outcomes)

  for (k in 1:n_outcomes) {
    df_twsa[[k]] <- create_dsa_twoway(parameters = param_table,
                                      other_outcome = sim_out_df[[k]],
                                      strategies = strategies)
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
#' @param tmp_replace values from predetermined DSA samples that will replace some values in \code{tmp_input}
#'
#' @importFrom  utils setTxtProgressBar
#' @keywords internal
wrapper_of_user_model <- function(x, user_fun, param_name,
                                  tmp_input, tmp_replace,
                                  progress_bar = NULL, param_counter = NULL, nsamp = NULL) {
  tmp_input[[1]][param_name] <- tmp_replace[x, param_name]
  if (!is.null(progress_bar)) {
    setTxtProgressBar(progress_bar, x + (param_counter - 1) * nsamp)
  }
  do.call(user_fun, tmp_input)
}
