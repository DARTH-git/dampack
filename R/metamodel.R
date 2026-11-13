#' Linear regression metamodeling
#'
#' @description This function estimates a linear regression metamodel for
#' a given decision-analytic model by using the results of a probabilistic sensitivity analysis (PSA)
#'
#' @details
#' The most important option is \code{analysis}, which can be either \code{"oneway"}
#' or \code{twoway}. If \code{analysis == "oneway"}, a separate metamodel is created
#' for each combination of the parameters in \code{params} and strategies in \code{strategies}
#' (by default, this is all strategies and parameters).
#'
#' If \code{analysis == "twoway"}, \code{params} must be a vector of two parameters, and a metamodel
#' is created with these two parameters for each strategy in \code{strategies}.
#'
#' @param analysis either "oneway" or "twoway"
#' @param psa psa object
#' @param params string vector with the name(s) of the parameter of interest. Defaults to all.
#' @param strategies vector of strategies to consider. The default (NULL) is that all strategies are considered.
#' @param outcome either effectiveness ("eff"), cost ("cost"), net health benefit ("nhb"),
#' net monetary benefit ("nmb"), or the opportunity loss in terms of NHB or
#' NMB ("nhb_loss" and "nmb_loss", respectively). "nmb_loss_voi" and "nhb_loss_voi" are only
#' used in internal function calls of metamodel within other VOI functions.
#' @param wtp if outcome is NHB or NMB (or the associated loss), must provide the willingness-to-pay threshold
#' @param type type of metamodel
#' @param poly.order order of polynomial for the linear regression metamodel.
#' Default: 2
#' @inheritParams mgcv::s
#'
#' @return A metamodel object, which contains a list of metamodels and other relevant information.
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
                      psa, params = NULL, strategies = NULL,
                      outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss",
                                  "nhb_loss_voi", "nmb_loss_voi"),
                      wtp = NULL,
                      type = c("linear", "gam", "poly"), poly.order = 2, k = -1) {

  # get parameter names
  v_pnames <- psa$parnames

  # analysis
  analysis <- match.arg(analysis)

  # type of model
  type <- match.arg(type)

  # make sure all of params is in parameter names
  if (is.null(params)) {
    params <- v_pnames
  } else if (!all(params %in% v_pnames)) {
    v_wrong_p <- setdiff(params, v_pnames)
    stop(paste0("the following parameters are not valid: ",
                paste(v_wrong_p, collapse = ",")))
  } else if (length(params) != 2 && analysis == "twoway") {
    stop("If analysis == twoway, exactly 2 params must be provided.")
  }

  # define dependent variables
  outcome <- match.arg(outcome)

  # define y, the outcome matrix
  df_y <- calculate_outcome(outcome, psa$cost, psa$effectiveness, wtp)

  # define strategies
  v_strat <- psa$strategies

  ## make sure all subset strats are in strat
  if (is.null(strategies)) {
    strategies <- v_strat
  } else if (!all(strategies %in% v_strat)) {
    v_wrong_strats <- setdiff(strategies, v_strat)
    errmsg <- paste0("the following are not in psa$strategies: ",
                     paste(v_wrong_strats, collapse = ","))
    stop(errmsg)
  }

  # define data for linear model
  # check.names = FALSE keeps strategy names (the response columns) as typed
  df_dat <- data.frame(df_y, psa$parameters, check.names = FALSE)

  # make sure there are not more parameters to estimate than there are psa samples
  if (nrow(df_dat) < length(params)) {
    errmsg <- paste0("The number of parameters to be estimated by the metamodel
                     cannot be greater than the number of PSA samples")
    stop(errmsg)
  }

  # list to hold linear models
  lms <- NULL

  # analysis: either oneway or twoway
  if (analysis == "oneway") {
    # loop over parameters
    for (p in params) {
      # loop over strategies
      for (s in strategies) {
        mod <- mm_run_reg(analysis, s, p, df_dat, type, poly.order, k)
        mod$param_of_int <- p
        mod$strat <- s
        lms[[p]][[s]] <- mod
      }
    }
  }
  if (analysis == "twoway" || analysis == "multiway") {
    # loop over strategies
    for (s in strategies) {
      mod <- mm_run_reg(analysis, s, params, df_dat, type, poly.order, k)
      # for accessing later in predict
      mod$param_of_int <- params
      mod$strat <- s
      lms[[s]] <- mod
    }
  }

  metamodel <- list(outcome = outcome, mods = lms, wtp = wtp,
                    params = params, strategies = strategies,
                    psa = psa, analysis = analysis,
                    type = type, poly.order = poly.order, k = k)
  # define class
  class(metamodel) <- "metamodel"
  metamodel
}

#' Build formula and run linear regression for metamodel
#' @param dep dependent variable in regression
#' @param dat data to use in regression
#' @param params all params in PSA
#'
#' @importFrom mgcv gam
#' @keywords internal
#' @return a statistical metamodel
#' @inheritParams metamodel
mm_run_reg <- function(analysis, dep, params, dat, type, poly.order, k) {

  n_params <- length(params)

  if (type == "gam" && k < 3 && n_params != 1) {
    k <- 3
    warning("k has been set to its minimum value of 3")
  }

  if (type == "gam" && ((k - 1) ^ n_params > 500)) {
    stop(paste0("\nIn your proposed metamodel, k-1 to the power of the number of parameters must not
 be greater than 500 (i.e. (k-1)^n_params < 500). This proposed model has ", n_params, " parameters,
 and a k value of ", k, ". Models that exceed this approximate threshold will fail to execute
 due to memory constraints and/or take an excessively long time to fit."))
  }

  if (type == "linear") {
    # build formula
    ## dependent variable
    fbeg <- paste0(backtick_name(dep), " ~ ")

    if (n_params > 1) {
      ## parameters of interest
      fparam <- paste(params, collapse = " + ")
    } else {
      ## parameter of interest
      fparam <- params
    }

    ## combine
    f <- as.formula(paste0(fbeg, fparam))

    # run metamodel
    metamod <- lm(f, data = dat)
    metamod$call <- call("lm", formula = f, data = quote(dat))
  }

  if (type == "poly") {
    # build formula
    ## dependent variable
    fbeg <- paste0(backtick_name(dep), " ~ ")

    ## parameters of interest
    fparam <- ""
    l_ps <- lapply(params, function(p) paste("poly(", p, ",", poly.order, ", raw=TRUE)"))

    if (analysis == "twoway") {
      l_ps <- append(l_ps,
                     paste0("poly(", params[1], ",", poly.order, ", raw=TRUE):",
                            "poly(", params[2], ",", poly.order, ", raw=TRUE)"))
    }

    fparam <- paste(l_ps, collapse = " + ")
    ## combine
    f <- as.formula(paste0(fbeg, fparam))

    # run metamodel
    metamod <- lm(f, data = dat)
    metamod$call <- call("lm", formula = f, data = quote(dat))
  }

  if (type == "gam") {
    # mgcv::gam() rebuilds the formula from text and drops backticks, so it cannot
    # fit a response with a non-syntactic name (e.g., a strategy named
    # "Standard of care"). Fit on a copy of the data whose response column has
    # a syntactic internal name; predictions and fit statistics do not depend on it.
    response <- ".dampack_response"
    names(dat)[names(dat) == dep] <- response
    dep <- response

    # build formula
    fbeg <- paste0(backtick_name(dep), " ~ ")

    ## parameters of interest
    fparam <- ""
    if (n_params == 1) {
      fparam <- paste0(fparam, "s(", params, ", k=", k, ")")
    } else {
      v_params_k <- paste0("s(", params,  ", k=", k, ")")
      f_s <- paste(v_params_k, collapse = " + ")
      k_ti <- ifelse(k == -1, NA, k)
      fparam <- paste0(f_s, " + ti(", paste(params, collapse = ", "), ", k = ", k_ti, ")")
    }

    f <- as.formula(paste0(fbeg, fparam))

    metamod <- gam(f, data = dat)
  }
  metamod
}


#' Print metamodel
#'
#' @param x metamodel to print
#' @param ... further arguments to print
#' @return None (invisible NULL)
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
      "mods: a nested list of ", x$type, " metamodels \n",
      "outcome: ", x$outcome, "\n",
      "WTP: ", ifelse(!is.null(wtp), wtp, "NA"), "\n",
      "strategies: ", paste(x$strategies, collapse = ", "), "\n",
      "parameters modeled: ", paste(x$params, collapse = ", "), "\n",
      sep = "")
}

#' Summary of metamodel
#'
#' @param object metamodel to summarize
#' @param ... further arguments to summary
#' @return a \code{data.frame} containing the r-squared for each strategy and parameter's metamodel.
#' @export
summary.metamodel <- function(object, ...) {
  analysis <- object$analysis
  type <- object$type
  df_summary <- NULL
  if (analysis == "multiway") {
    stop("metamodel summary not available for multiway analyses")
  }
  if (analysis == "oneway") {
    for (p in object$params) {
      for (s in object$strategies) {
        lm_summary <- summary(object$mods[[p]][[s]])
        if (type == "gam") {
          r2 <- lm_summary$r.sq
        } else {
          r2 <- lm_summary$r.squared
        }
        df_new_row <- data.frame("param" = p, "strat" = s, "rsquared" = r2)
        df_summary <- rbind(df_summary, df_new_row)
      }
    }
  }
  if (analysis == "twoway") {
    v_params <- object$params
    for (s in object$strategies) {
      lm_summary <- summary(object$mods[[s]])
      if (type == "gam") {
        r2 <- lm_summary$r.sq
      } else {
        r2 <- lm_summary$r.squared
      }
      df_new_row <- data.frame("param1" = v_params[1], "param2" = v_params[2],
                               "strat" = s, "rsquared" = r2)
      df_summary <- rbind(df_summary, df_new_row)
    }
  }
  df_summary
}

#' Predict from a one-way or two-way metamodel
#'
#' @param object object with class "metamodel"
#' @param ranges a named list of the form c("param" = c(0, 1), ...)
#' that gives the ranges for the parameter of interest. If NULL,
#' parameter values from the middle 95\% of the PSA samples are used. The number of samples
#' from this range is determined by \code{nsamp}.
#' @param nsamp number of samples from ranges
#' @param ... further arguments to \code{predict} (not used)
#'
#' @importFrom stats quantile predict
#' @return a \code{data.frame} containing the outcome values predicted by the metamodel
#' for each strategy and each combination of parameter values defined by \code{ranges}.
#' @export
predict.metamodel <- function(object, ranges = NULL, nsamp = 100, ...) {

  # analysis type
  analysis <- object$analysis
  # Predicting from multiway metamodels not yet implemented
  if (analysis == "multiway") {
    stop("metamodel predictions not available for multiway analyses")
  }

  # type checking
  ## make sure ranges is NULL or list
  if (!is.null(ranges)) {
    if (!inherits(ranges, "list")) {
      stop("ranges must be a named list of vectors")
    }

    ## all elements of ranges must have length 2 or be NULL
    for (i in ranges) {
      if (!is.null(i) && length(i) != 2) {
        stop("all entries in ranges must have length 2 or be NULL")
      }
    }
  }

  # all parameters in the metamodel
  v_psa_params <- object$params

  # get original psa parameter df
  df_psa_paramvals <- object$psa$parameters

  # set of linear models
  l_mods <- object$mods

  # strategies
  v_strats <- object$strategies

  # data frame to be used in predict - mean values repeated
  # the values for the parameter of interest are replaced
  # in each pass through the loop
  if (analysis == "oneway") {
    pred_data_nrow <- nsamp
  }
  if (analysis == "twoway") {
    pred_data_nrow <- nsamp ^ 2
  }
  df_pdata <- data.frame(matrix(colMeans(df_psa_paramvals),
                                nrow = pred_data_nrow,
                                ncol = ncol(df_psa_paramvals),
                                byrow = TRUE))

  # Name data frame's columns with parameters' names
  colnames(df_pdata) <- colnames(df_psa_paramvals)

  # these are parameters that are included in ranges
  if (is.null(ranges)) {
    v_q_params <- v_psa_params
  } else {
    # get parameters associated with ranges
    v_q_params <- names(ranges)
    if (!all(v_q_params %in% v_psa_params)) {
      v_wrong_params <- setdiff(v_q_params, v_psa_params)
      stop(paste0("The following range names were not found in psa$parameters:\n",
                  paste(v_wrong_params, collapse = ", ")))
    }
  }

  # predict outcomes from linear metamodels
  if (analysis == "oneway") {
    ## make list to hold outcome dfs
    l_outcome <- vector(mode = "list",
                        length = length(v_strats) * length(v_q_params))
    counter <- 1
    for (p in v_q_params) {
      # define evenly spaced samples from parameter range
      v_param_val <- make_param_seq(p, ranges, nsamp, df_psa_paramvals)

      # create new data from param_val
      df_newdata <- data.frame(param_val = v_param_val)

      # replace values for parameter of interest
      df_this_p_data <- df_pdata
      df_this_p_data[, p] <- df_newdata

      # predict values from linear model
      # for each strategy
      for (s in v_strats) {
        mod <- l_mods[[p]][[s]]
        l_outcome[[counter]] <- data.frame("parameter" = p, "strategy" = s,
                                           "param_val" = df_newdata,
                                           "outcome_val" = predict(mod, newdata = df_this_p_data, type = "response"),
                                           stringsAsFactors = FALSE)
        counter <- counter + 1
      }
    }
  }
  if (analysis == "twoway") {
    l_outcome <- vector(mode = "list",
                        length = length(v_strats))
    counter <- 1
    p1 <- v_psa_params[1]
    p2 <- v_psa_params[2]
    v_p1_samp <- make_param_seq(p1, ranges, nsamp, df_psa_paramvals)
    v_p2_samp <- make_param_seq(p2, ranges, nsamp, df_psa_paramvals)
    df_p1p2 <- data.frame(expand.grid(v_p1_samp, v_p2_samp))
    df_pdata[, c(p1, p2)] <- df_p1p2

    for (s in v_strats) {
      mod <- l_mods[[s]]
      outcome <- predict(mod, newdata = df_pdata)
      df_outcome <- data.frame("p1" = df_pdata[, p1], "p2" = df_pdata[, p2],
                               "strategy" = s, "outcome_val" = outcome,
                               stringsAsFactors = FALSE)
      names(df_outcome)[1:2] <- c(p1, p2)
      l_outcome[[counter]] <- df_outcome
      counter <- counter + 1
    }
  }
  df_combined <- bind_rows(l_outcome)
  df_combined
}

#' make a parameter sequence
#'
#' @param p parameter of interest
#' @param ranges named vector of parameter ranges
#' @param nsamp number of points from the range
#' @param psa_paramvals sampled values from the PSA. used to calculate the
#' range if none is supplied
#' @return a vector of parameter values spanning the range specified
#' @keywords internal
make_param_seq <- function(p, ranges, nsamp, psa_paramvals) {
  v_p_range <- ranges[[p]]

  # define default range if necesary
  v_p_psa_vals <- psa_paramvals[, p]
  if (is.null(v_p_range)) {
    v_p_range <- quantile(v_p_psa_vals, c(0.025, 0.975))
  } else {
    # throw warning if outside of psa range
    v_psa_range <- range(v_p_psa_vals)
    if (v_p_range[1] < v_psa_range[1] || v_p_range[2] > v_psa_range[2]) {
      warning(paste0("The requested range for ", p, " is outside of the PSA range.\n",
                     "requested range: [", paste(v_p_range, collapse = ","), "]\n",
                     "PSA range: [", paste(v_psa_range, collapse = ", "), "]\n.",
                     "Please interpret results with caution."))
    }
  }
  # define evenly spaced samples from parameter range
  seq(v_p_range[1], v_p_range[2], length.out = nsamp)
}

#' Quote a name for use in a model formula
#'
#' Wraps a name in backticks so that strategy names with spaces or other
#' characters can be used as the response of a regression metamodel.
#'
#' @param x character name.
#' @return \code{x} wrapped in backticks, with any backtick inside it escaped.
#' @keywords internal
backtick_name <- function(x) {
  paste0("`", gsub("`", "\\\\`", x), "`")
}
