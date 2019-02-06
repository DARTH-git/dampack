#' linear regression metamodeling
#'
#' @param psa psa object
#' @param parms String with the name of the parameter of interest
#' @param strategies vector of strategies to consider. the default (NULL) is that all strategies are considered. The
#' @param outcome either effectiveness ("eff"), cost, net health benefit ("nhb"), or net monetary benefit ("nmb")
#' @param wtp if outcome is NHB or NMB, must provide the willingness-to-pay threshold
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#'
#' @importFrom stats as.formula formula getCall lm
#' @export
metamod <- function(psa, parms = NULL, strategies = NULL,
                    outcome = c("eff", "cost", "nhb", "nmb"),
                    wtp = NULL,
                    poly.order = 2) {
  # get parameter names
  pnames <- psa$parnames

  # make sure all of parms is in parameter names
  if (is.null(parms)) {
    parms <- pnames
  } else if (!all(parms %in% pnames)) {
    wrong_p <- setdiff(p, pnames)
    stop(paste0("the following parameters are not valid: ",
                paste(wrong_p, collapse = ",")))
  }

  # define dependent variables
  outcome <- match.arg(outcome)

  ## make sure wtp is not null if nmb or nhb
  if (is.null(wtp) & (outcome == "nmb" | outcome == "nhb")) {
    stop("wtp must be provided if nmb or nhb is the outcome to be modeled")
  }

  # define y, the outcome matrix
  if (outcome == "eff") {
    y <- psa$effectiveness
  }
  if (outcome == "cost") {
    y <- psa$cost
  }
  if (outcome == "nhb") {
    y <- psa$effectiveness - psa$cost / wtp
  }
  if (outcome == "nmb") {
    y <- psa$effectiveness * wtp - psa$cost
  }

  # define strategies
  strat <- psa$strategies

  ## make sure all subset strats are in strat
  if (is.null(strategies)) {
    strategies <- strat
  } else if (!all(strategies %in% strat)) {
    wrong_strats <- setdiff(strategies, strat)
    errmsg <- paste0("the following are not in psa$strategies: ",
                     paste(wrong_strats, collapse = ","))
    stop(errmsg)
  }

  # define data for linear model
  dat <- data.frame(y, psa$parameters)

  # list to hold linear models
  lms <- NULL

  # loop over parameters
  for (p in parms) {
    # independent variables not of interest
    other_parms <- pnames[-match(p, pnames)]

    # loop over strategies
    for (s in strategies) {
      lm_form <- as.formula(
        paste0(s, " ~ ",
               paste0("poly(", p, ",", poly.order, ", raw=TRUE) + "),
               paste(other_parms, collapse = " + "))
      )

      mod <- lm(lm_form, data = dat)
      # makes the displayed formula more informative
      mod$call <- call("lm", formula = lm_form, data = quote(dat))
      # for accessing later in predict
      mod$parm_of_int <- p
      mod$strat <- s
      lms[[p]][[s]] <- mod
    }
  }

  metamod <- list(outcome = outcome, mods = lms, wtp = wtp,
                  parms = parms, strategies = strategies,
                  psa = psa)
  # define class
  class(metamod) <- "metamodel"
  return(metamod)
}

#' Print metamodel
#'
#' @param x metamodel to print
#' @param ... further arguments to print
#' @export
print.metamodel <- function(x, ...) {
  cat("metamodel object", "\n",
      "-------------------------", "\n",
      "a list of the following objects: ",
      paste(names(x), collapse = ","), "\n",
      "\n",
      "some details: ", "\n",
      "-------------------------", "\n",
      "mods: a nested list of linear metamodels \n",
      "outcome: ", x$outcome, "\n",
      "WTP: ", x$wtp, "\n",
      "strategies: ", paste(x$strategies, collapse = ","), "\n",
      "parameters modeled: ", paste(x$parms, collapse = ","), "\n",
      "To access the linear model for strategy s and parameter p, ", "\n",
      "type metamod$mods[[p]][[s]] at the console ",
      "(where p and s are replaced with their respective strings).",
      sep = "")
}

#' Summary of metamodel
#'
#' @param object metamodel to summarize
#' @param ... further arguments to summary
#' @export
summary.metamodel <- function(object, ...) {
  summary_df <- NULL
  for (p in object$parms) {
    for (s in object$strategies) {
      lm_summary <- summary(object$mods[[p]][[s]])
      r2 <- lm_summary$r.squared
      df_new_row <- data.frame("parm" = p, "strat" = s, "rsquared" = r2)
      summary_df <- rbind(summary_df, df_new_row)
    }
  }
  return(summary_df)
}

#' Predict from a metamodel
#'
#' @param object object with class "metamodel"
#' @param ranges A named list of the form c("parm" = c(0, 1), ...)
#' that gives the ranges for the parameter of interest. If NULL,
#' parameter values from the middle 95% of the PSA samples are used. The number of samples
#' from this range is determined by \code{nsamp}.
#' @param nsamp number of samples from ranges
#' @param ... further arguments to \code{predict} (not used)
#'
#' @importFrom stats quantile predict
#' @export
predict.metamodel <- function(object, ranges = NULL, nsamp = 100, ...) {
  # all parameters in psa
  psa_parms <- object$parms

  # get original psa parameter df
  psa_parmvals <- object$psa$parameters

  # set of linear models
  mods <- object$mods

  # strategies
  strats <- object$strategies

  # data frame to be used in predict - mean values repeated
  # the values for the parameter of interest are replaced
  # in each pass through the loop
  # Generate matrix to use for prediction
  # use the means of all parameters other than parameter of interest
  pdata <- data.frame(matrix(colMeans(psa_parmvals),
                             nrow = nsamp,
                             ncol = ncol(psa_parmvals),
                             byrow = T))

  # Name data frame's columns with parameters' names
  colnames(pdata) <- colnames(psa_parmvals)

  # these are parameters that are included in ranges
  if (is.null(ranges)) {
    q_parms <- psa_parms
  } else {
    q_parms <- names(ranges)
  }

  # predict outcomes from linear metamodels
  ## make list to hold outcome dfs
  outcome_dfs <- vector(mode = "list",
                        length = length(strats) * length(q_parms))
  counter <- 1
  for (p in q_parms) {
    p_range <- ranges[p]
    if (is.null(p_range)) {
      p_psa_vals <- psa_parmvals[, p]
      prange <- quantile(p_psa_vals, c(0.025, 0.975))
    }
    # define evenly spaced samples from parameter range
    pranges_samp <- seq(prange[1], prange[2], length.out = nsamp)

    # create new data from pranges_samp
    newdata <- data.frame(pranges_samp)

    # replace values for parameter of interest
    this_p_data <- pdata
    this_p_data[, p] <- newdata

    # predict values from linear model
    # for each strategy
    for (s in strats) {
      mod <- mods[[p]][[s]]
      outcome_dfs[[counter]] <- data.frame("parameter" = p, "strategy" = s,
                            "param_val" = newdata,
                            "outcome_val" = predict(mod, newdata = this_p_data),
                            stringsAsFactors = FALSE)
      counter <- counter + 1
    }
  }
  combined_df <- bind_rows(outcome_dfs)
  return(combined_df)
}
