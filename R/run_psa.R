#' @param FUN Function that takes the basecase in \code{params_all} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a dataframe
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}.
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param ... Additional arguments to user-defined \code{FUN}


run_psa <- function(psa_samp, FUN, outcomes = NULL, strategies = NULL, ...){

  FUN <- test_func
  psa_samp <- test
  strategies = NULL
  opt_arg_val <- list(cheetos = 1.5)
  outcomes <- c("cost", "effect")

  fun_input_test <- c(list(psa_samp[1,]), opt_arg_val)

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_test)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))){
    stop("FUN is not well defined by the parameter values and ...")
  }

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

  opt_arg_val <- list(...)
  sim_out_ls <- vector(mode = "list", length = nrow(psa_samp))

  for (i in 1:nrow(psa_samp)){
    fun_input_ls <- c(list(psa_samp[i,]), opt_arg_val)
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

}



#' Wrapper function for owsa_det and twsa_det
#'
#' @param x iteration
#' @param user_fun \code{FUN} input from users
#' @param param_name user-defined list of parameters of interest
#' @param tmp_input basecase values
#' @param tmp_replace values from predetermined DSA samples that will replace some values in \code{tmp_input}
#'
#' @keywords internal
wrapper_of_user_model <- function(x, user_fun, tmp_input) {
  tmp_input[[1]][param_name] <- tmp_replace[x, param_name]
  do.call(user_fun, tmp_input)
}




test_func <- function(params, cheetos){
  normalboi <- params[["normalboi"]]
  loggyboi <- params[["loggyboi"]]

  effect <- normalboi + loggyboi *cheetos
  cost <- -normalboi - loggyboi

  output <- data.frame(strategy = "mystrat",
                       effect = effect,
                       cost = cost)
  return(output)
}
