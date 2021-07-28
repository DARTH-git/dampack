#' Calculate outcomes for a PSA using a user-defined function.
#'
#' @description \code{run_psa} calculates outcomes using a user-defined function and creates PSA objects
#' corresponding to the specified outcomes.
#'
#' @param psa_samp A data frame with samples of parameters for a probabilistic sensitivity analysis (PSA)
#' @param params_basecase a named list of base case values for input parameters needed by \code{FUN},
#' the user-defined function.
#' @param FUN Function that takes the parameter values in \code{psa_samp} and \code{...} to
#' produce the \code{outcome} of interest. The \code{FUN} must return a data frame
#' where the first column are the strategy names and the rest of the columns must be outcomes.
#' @param outcomes String vector with the outcomes of interest from \code{FUN}.
#' @param strategies vector of strategy names. The default \code{NULL} will use
#' strategy names in \code{FUN}
#' @param currency symbol for the currency being used (ex. "$", "£")
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#'  should be displayed in console.
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
run_psa <- function(psa_samp, params_basecase = NULL, FUN, outcomes = NULL,
                    strategies = NULL, currency = "$", progress = TRUE, ...) {
  opt_arg_val <- list(...)

  if (!is.null(params_basecase)) {
      replace_var <- names(psa_samp)[names(psa_samp) %in% names(params_basecase)]
      new_params <- params_basecase
      # replace values in new_params
      new_params[replace_var] <- psa_samp[1, replace_var]
      fun_input_test <- c(list(new_params), opt_arg_val)
  } else {
    fun_input_test <- c(list(psa_samp[1, ]), opt_arg_val)
  }

  jj <- tryCatch({
    userfun <- do.call(FUN, fun_input_test)
  },
  error = function(e) NA)
  if (is.na(sum(is.na(jj)))) {
    stop("FUN is not well defined by the parameter values and ...")
  }
  userfun <- do.call(FUN, fun_input_test)

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

  sim_out_ls <- vector(mode = "list", length = nrow(psa_samp))



  if (!is.null(params_basecase)) {
    replace_var <- names(psa_samp)[names(psa_samp) %in% names(params_basecase)]
    new_params <- params_basecase
    for (i in seq_len(nrow(psa_samp))) {
      # replace values in new_params
      new_params[replace_var] <- psa_samp[i, replace_var]
      fun_input_ls <- c(list(new_params), opt_arg_val)
      sim_out_ls[[i]] <- do.call(FUN, fun_input_ls)
      # display progress every 10%
      if (progress == TRUE) {
        if (i / (nrow(psa_samp) / 10) == round(i / (nrow(psa_samp) / 10), 0) & progress == TRUE) {
          cat("\r", paste(i / nrow(psa_samp) * 100, "% done", sep = " "))
        }
      }
    }
  } else {
    for (i in seq_len(nrow(psa_samp))) {
      fun_input_ls <- c(list(psa_samp[i, ]), opt_arg_val)
      sim_out_ls[[i]] <- do.call(FUN, fun_input_ls)
      # display progress every 10%
      if (progress == TRUE) {
        if (i / (nrow(psa_samp) / 10) == round(i / (nrow(psa_samp) / 10), 0) & progress == TRUE) {
          cat("\r", paste(i / nrow(psa_samp) * 100, "% done", sep = " "))
        }
      }
    }
  }

  n_outcomes <- length(outcomes)
  sim_out_df <- vector(mode = "list", length = n_outcomes)
  names(sim_out_df) <- outcomes
  for (j in 1:n_outcomes) {
    sim_out_df[[j]] <- lapply(sim_out_ls,
                              function(x, tmp_out = outcomes[j]) {
                                t(x[[outcomes[j]]])
                              })
    sim_out_df[[j]] <- as.data.frame(do.call(rbind, sim_out_df[[j]]))
    colnames(sim_out_df[[j]]) <- strategies
  }



    psa_out <- vector(mode = "list", length = n_outcomes)
    for (j in 1:n_outcomes) {
      psa_out[[j]] <- make_psa_obj(cost = NULL, effectiveness = NULL,
                                   other_outcome = sim_out_df[[j]],
                                   parameters = psa_samp[, -1], strategies = strategies,
                                   currency = currency)
    }


    names(psa_out) <- outcomes

    return(psa_out)
  }
