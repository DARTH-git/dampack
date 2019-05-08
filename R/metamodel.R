#' Linear regression metamodeling
#'
#' @description This function estimates a linear regression metamodel for
#' a given decision-analytic model by using the results of a probabilistic sensitivity analysis (PSA)
#'
#' @details
#' The most important option is \code{analysis}, which can be either \code{"oneway"}
#' or \code{twoway}. If \code{analysis == "oneway"}, a separate metamodel is created
#' for each combination of the parameters in \code{parms} and strategies in \code{strategies}
#' (by default, this is all strategies and parameters).
#'
#' If \code{analysis == "twoway"}, \code{parms} must be a vector of two parameters, and a metamodel
#' is created with these two parameters for each strategy in \code{strategies}.
#'
#' @param analysis either "oneway" or "twoway"
#' @param psa psa object
#' @param parms String with the name of the parameter of interest
#' @param strategies vector of strategies to consider. The default (NULL) is that all strategies are considered.
#' @param outcome either effectiveness ("eff"), cost, net health benefit ("nhb"), or net monetary benefit ("nmb")
#' @param wtp if outcome is NHB or NMB, must provide the willingness-to-pay threshold
#' @param type type of metamodel
#' @param poly.order Order of polynomial for the linear regression metamodel.
#' Default: 2
#' @inheritParams mgcv::s
#'
#' @return
#' A metamodel object, which contains a list of metamodels and other relevant information.
#'
#' @seealso
#' \code{\link{predict.metamodel}},
#' \code{\link{make_psa_obj}},
#' \code{\link{owsa}},
#' \code{\link{twsa}}
#'
#' @importFrom stats as.formula formula getCall lm
#' @export
metamodel <- function(analysis = c("oneway", "twoway", "multiway"),
                      psa, parms = NULL, strategies = NULL,
                      outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss"),
                      wtp = NULL,
                      type = c("linear", "gam", "poly"), poly.order = 2, k = -1) {
  # get parameter names
  pnames <- psa$parnames

  # analysis
  analysis <- match.arg(analysis)

  # type of model
  type <- match.arg(type)

  # make sure all of parms is in parameter names
  if (is.null(parms)) {
    parms <- pnames
  } else if (!all(parms %in% pnames)) {
    wrong_p <- setdiff(p, pnames)
    stop(paste0("the following parameters are not valid: ",
                paste(wrong_p, collapse = ",")))
  } else if (length(parms) != 2 & analysis == "twoway") {
    stop("If analysis == twoway, exactly 2 parms must be provided.")
  }

  # define dependent variables
  outcome <- match.arg(outcome)

  # define y, the outcome matrix
  y <- calculate_outcome(outcome, psa$cost, psa$effectiveness, wtp)

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

  # analysis: either oneway or twoway
  if (analysis == "oneway") {
    # loop over parameters
    for (p in parms) {
      # loop over strategies
      for (s in strategies) {
        mod <- mm_run_reg(s, p, dat, pnames, type, poly.order, k)
        mod$parm_of_int <- p
        mod$strat <- s
        lms[[p]][[s]] <- mod
      }
    }
  }
  if (analysis == "twoway" | analysis == "multiway") {
    # loop over strategies
    for (s in strategies) {
      mod <- mm_run_reg(s, parms, dat, pnames, type, poly.order, k)
      # for accessing later in predict
      mod$parm_of_int <- parms
      mod$strat <- s
      lms[[s]] <- mod
    }
  }

  metamodel <- list(outcome = outcome, mods = lms, wtp = wtp,
                    parms = parms, strategies = strategies,
                    psa = psa, analysis = analysis,
                    type = type, poly.order = poly.order, k = k)
  # define class
  class(metamodel) <- "metamodel"
  return(metamodel)
}

#' Build formula and run linear regression for metamodel
#' @param dep dependent variable in regression
#' @param dat data to use in regression
#' @param all_parms all parms in PSA
#'
#' @importFrom mgcv gam
#' @keywords internal
#' @inheritParams metamodel
mm_run_reg <- function(dep, parms, dat, all_parms, type, poly.order, k) {
  n_parms <- length(parms)

  if (type == "linear") {
    # build formula
    ## dependent variable
    fdep <- paste0(dep, " ~ ")

    ## parameters
    fparm <- paste(all_parms, collapse = " + ")

    ## combine
    f <- as.formula(paste0(fdep, fparm))

    # run metamodel
    metamod <- lm(f, data = dat)
    metamod$call <- call("lm", formula = f, data = quote(dat))
  }
  if (type == "poly") {
    # build formula
    ## dependent variable
    fbeg <- paste0(dep, " ~ ")

    ## parameters of interest
    fparm <- ""
    for (p in parms) {
      fparm <- paste0(fparm, "poly(", p, ",", poly.order, ", raw=TRUE) + ")
    }

    ## other parameters
    other_parms <- all_parms[-match(parms, all_parms)]
    fend <- paste(other_parms, collapse = " + ")

    ## combine
    f <- as.formula(paste0(fbeg, fparm, fend))

    # run metamodel
    metamod <- lm(f, data = dat)
    metamod$call <- call("lm", formula = f, data = quote(dat))
  }
  if (type == "gam") {
    # build formula
    fbeg <- paste0(dep, " ~ ")

    ## parameters of interest
    fparm <- ""
    if (n_parms == 1) {
      fparm <- paste0(fparm, "s(", parms, ", k=", k, ")")
    } else {
      for (p in parms) {
        fparm <- paste0(fparm, "s(", p, ", k=", k, ")")
      }
      # add interactions
      fparm <- paste0("ti(", paste(parms, collapse = ", "), ", k = ", k, ")")
    }

    f <- as.formula(paste0(fbeg, fparm))

    metamod <- gam(f, data = dat)
  }
  return(metamod)
}


#' Print metamodel
#'
#' @param x metamodel to print
#' @param ... further arguments to print
#' @export
print.metamodel <- function(x, ...) {
  wtp <- x$wtp
  cat("metamodel object", "\n",
      "-------------------------", "\n",
      "a list of the following objects: ",
      paste(names(x), collapse = ", "), "\n",
      "\n",
      "some details: ", "\n",
      "-------------------------", "\n",
      "analysis: this is a ", substr(x$analysis, 1, 3), "-way metamodel \n",
      "mods: a nested list of linear metamodels \n",
      "outcome: ", x$outcome, "\n",
      "WTP: ", ifelse(!is.null(wtp), wtp, "NA"), "\n",
      "strategies: ", paste(x$strategies, collapse = ", "), "\n",
      "parameters modeled: ", paste(x$parms, collapse = ", "), "\n",
      sep = "")
}

#' Summary of metamodel
#'
#' @param object metamodel to summarize
#' @param ... further arguments to summary
#' @export
summary.metamodel <- function(object, ...) {
  analysis <- object$analysis
  summary_df <- NULL
  if (analysis == "oneway") {
    for (p in object$parms) {
      for (s in object$strategies) {
        lm_summary <- summary(object$mods[[p]][[s]])
        r2 <- lm_summary$r.squared
        df_new_row <- data.frame("parm" = p, "strat" = s, "rsquared" = r2)
        summary_df <- rbind(summary_df, df_new_row)
      }
    }
  }
  if (analysis == "twoway") {
    parms <- object$parms
    for (s in object$strategies) {
      lm_summary <- summary(object$mods[[s]])
      r2 <- lm_summary$r.squared
      df_new_row <- data.frame("parm1" = parms[1], "parm2" = parms[2],
                               "strat" = s, "rsquared" = r2)
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
#' parameter values from the middle 95\% of the PSA samples are used. The number of samples
#' from this range is determined by \code{nsamp}.
#' @param nsamp number of samples from ranges
#' @param ... further arguments to \code{predict} (not used)
#'
#' @importFrom stats quantile predict
#' @export
predict.metamodel <- function(object, ranges = NULL, nsamp = 100, ...) {
  # type checking
  ## make sure ranges is NULL or list
  if (!is.null(ranges)) {
    if (!inherits(ranges, "list")) {
      stop("ranges must be a named list of vectors")
    }

    ## all elements of ranges must have length 2 or be NULL
    for (i in ranges) {
      if (!is.null(i) & length(i) != 2) {
        stop("all entries in ranges must have length 2 or be NULL")
      }
    }
  }

  # all parameters in psa
  psa_parms <- object$parms

  # get original psa parameter df
  psa_parmvals <- object$psa$parameters

  # set of linear models
  mods <- object$mods

  # strategies
  strats <- object$strategies

  # analysis type
  analysis <- object$analysis

  # data frame to be used in predict - mean values repeated
  # the values for the parameter of interest are replaced
  # in each pass through the loop
  if (analysis == "oneway") {
    pred_data_nrow <- nsamp
  }
  if (analysis == "twoway") {
    pred_data_nrow <- nsamp ^ 2
  }
  pdata <- data.frame(matrix(colMeans(psa_parmvals),
                             nrow = pred_data_nrow,
                             ncol = ncol(psa_parmvals),
                             byrow = TRUE))

  # Name data frame's columns with parameters' names
  colnames(pdata) <- colnames(psa_parmvals)

  # these are parameters that are included in ranges
  if (is.null(ranges)) {
    q_parms <- psa_parms
  } else {
    # get parameters associated with ranges
    q_parms <- names(ranges)
    if (!all(q_parms %in% psa_parms)) {
      wrong_parms <- setdiff(q_parms, psa_parms)
      stop(paste0("The following range names were not found in psa$parameters:\n",
                  paste(wrong_parms, collapse = ", ")))
    }
  }

  # predict outcomes from linear metamodels
  if (analysis == "oneway") {
    ## make list to hold outcome dfs
    outcome_dfs <- vector(mode = "list",
                          length = length(strats) * length(q_parms))
    counter <- 1
    for (p in q_parms) {
      # define evenly spaced samples from parameter range
      param_val <- make_parm_seq(p, ranges, nsamp, psa_parmvals)

      # create new data from param_val
      newdata <- data.frame(param_val)

      # replace values for parameter of interest
      this_p_data <- pdata
      this_p_data[, p] <- newdata

      # predict values from linear model
      # for each strategy
      for (s in strats) {
        mod <- mods[[p]][[s]]
        outcome_dfs[[counter]] <- data.frame("parameter" = p, "strategy" = s,
                                             "param_val" = newdata,
                                             "outcome_val" = predict(mod, newdata = this_p_data, type = "response"),
                                             stringsAsFactors = FALSE)
        counter <- counter + 1
      }
    }
  }
  if (analysis == "twoway") {
    outcome_dfs <- vector(mode = "list",
                          length = length(strats))
    counter <- 1
    p1 <- psa_parms[1]
    p2 <- psa_parms[2]
    p1_samp <- make_parm_seq(p1, ranges, nsamp, psa_parmvals)
    p2_samp <- make_parm_seq(p2, ranges, nsamp, psa_parmvals)
    p1p2_data <- data.frame(expand.grid(p1_samp, p2_samp))
    pdata[, c(p1, p2)] <- p1p2_data

    for (s in strats) {
      mod <- mods[[s]]
      outcome <- predict(mod, newdata = pdata)
      outcome_df <- data.frame("p1" = pdata[, p1], "p2" = pdata[, p2],
                                           "strategy" = s, "outcome_val" = outcome,
                                           stringsAsFactors = FALSE)
      names(outcome_df)[1:2] <- c(p1, p2)
      outcome_dfs[[counter]] <- outcome_df
      counter <- counter + 1
    }
  }
  combined_df <- bind_rows(outcome_dfs)
  return(combined_df)
}

#' make a parameter sequence
#'
#' @param p parameter of interest
#' @param ranges named vector of parameter ranges
#' @param nsamp number of points from the range
#' @param psa_parmvals sampled values from the PSA. used to calculate the
#' range if none is supplied
#' @keywords internal
make_parm_seq <- function(p, ranges, nsamp, psa_parmvals) {
  p_range <- ranges[[p]]

  # define default range if necesary
  p_psa_vals <- psa_parmvals[, p]
  if (is.null(p_range)) {
    p_range <- quantile(p_psa_vals, c(0.025, 0.975))
  } else {
    # throw warning if outside of psa range
    psa_range <- range(p_psa_vals)
    if (p_range[1] < psa_range[1] | p_range[2] > psa_range[2]) {
      warning(paste0("The requested range for ", p, " is outside of the PSA range.\n",
                     "requested range: [", paste(p_range, collapse = ","), "]\n",
                     "PSA range: [", paste(psa_range, collapse = ", "), "]\n.",
                     "Please interpret results with caution."))
    }
  }
  # define evenly spaced samples from parameter range
  seq(p_range[1], p_range[2], length.out = nsamp)
}
