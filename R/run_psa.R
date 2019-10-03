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
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param ... Additional arguments to user-defined \code{FUN}
#'
#'
#' @return
#' A list containing PSA objects for each outcome in \code{outcomes}.
#'
#' @seealso
#' \code{\link{run_psa}},
#' \code{\link{make_psa_obj}},
#' \code{\link{gen_psa_samp}},
#' @export

run_psa <- function(psa_samp, FUN, outcomes = NULL,
                    strategies = NULL, ...){

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
      psa_out[[j]] <- make_psa_obj(cost = NULL, effectiveness = NULL,
                                   other_outcome = sim_out_df[[j]],
                                   parameters = psa_samp[, -1], strategies = strategies,
                                   currency = "$")
    }

    names(psa_out) <- outcomes
    return(psa_out)
  }
