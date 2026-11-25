#' Calculate Expected Value of Sample Information (EVSI)
#'
#' @inheritParams calc_evppi
#' @param n additional sample size
#' @param n0 initial sample size (the effective prior sample size)
#' @param n_by_param if \code{TRUE}, each parameter in the metamodel can have a unique
#' initial and additional sample size. \code{n} and \code{n0} must be numerical
#' vectors of equal length to \code{params}, with each value corresponding to the
#' initial and additional sample sizes for each parameter in the metamodel. By default,
#' \code{n_by_param = FALSE}, and each value of \code{n} and \code{n0} is shared by
#' each parameter in the model. Named vectors of \code{n} and \code{n0} are matched to
#' \code{params} by name; unnamed vectors follow the order of \code{params}.
#' When \code{n_by_param = FALSE}, \code{n0} must be a single numeric value, and
#' \code{n} must be a numerical vector of additional sample sizes for which EVSI is
#' calculated from the metamodel.
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#' should be displayed in console.
#'
#' @return A list containing 1) a data.frame with WTP thresholds, new prospective sample sizes
#'  (if \code{n_by_param == FALSE}), and corresponding EVSIs
#' for the selected parameters and 2) a list of metamodels used to estimate EVSI for each
#' strategy at each willingness to pay threshold.
#'
#' @seealso \code{\link{plot.evsi}}, \code{\link{calc_evppi}}
#'
#' @references
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A Gaussian Approximation Approach for Value of
#' Information Analysis. Med Decis Making. 2018;38(2):174-188.
#' \doi{10.1177/0272989X17715627}
#' \item Strong M, Oakley JE, Brennan A. Estimating Multiparameter Partial Expected Value
#' of Perfect Information from a Probabilistic Sensitivity Analysis Sample: A Nonparametric
#' Regression Approach. Med Decis Making. 2014;34(3):311-326. \doi{10.1177/0272989X13505910}
#' }
#'
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#'
#' # EVSI of a study of n additional patients that informs both failure probabilities,
#' # which were each estimated from an initial study of n0 = 50 patients
#' l_evsi <- calc_evsi(l_psa,
#'                     wtp = 1e5,
#'                     params = c("pFailSurg", "pFailChemo"),
#'                     outcome = "nmb",
#'                     k = 3,
#'                     n = c(10, 50, 100, 200),
#'                     n0 = 50,
#'                     progress = FALSE)
#' l_evsi$df_evsi
#' plot(l_evsi)
#'
#' \donttest{
#' # a separate initial and additional sample size for each parameter
#' l_evsi_by_param <- calc_evsi(l_psa,
#'                              wtp = c(5e4, 1e5, 2e5),
#'                              params = c("pFailSurg", "pFailChemo"),
#'                              outcome = "nmb",
#'                              k = 3,
#'                              n = c(100, 50),
#'                              n0 = c(50, 20),
#'                              n_by_param = TRUE,
#'                              progress = FALSE)
#' l_evsi_by_param$df_evsi
#' }
#'
#' @export
calc_evsi <- function(psa,
                      wtp,
                      params = NULL,
                      outcome = c("nmb", "nhb"),
                      k = -1,
                      n = 100,
                      n0 = 10,
                      n_by_param = FALSE,
                      pop = 1,
                      progress = TRUE) {
  # define parameter values and make sure they correspond to a valid option
  outcome <- match.arg(outcome)
  outcome_input <- outcome

  # adjust outcome type
  outcome <- paste0(outcome, "_loss_voi")

  # number of wtp thresholds
  n_wtps <- length(wtp)

  # number of new sample sizes
  n_n <- length(n)

  if (n_by_param == TRUE) {
    # vector to store evsi
    m_evsi <- rep(0, n_wtps)
  } else {
    # matrix to store evsi
    m_evsi <- matrix(rep(0, n_wtps * n_n), ncol = n_n)
  }

  l_mms <- vector(mode = "list", length = n_wtps)

  # warn about the metamodel specification once, not at every WTP
  n_params <- length(if (is.null(params)) psa$parnames else params)
  if (n_params > 1) {
    mm_gam_spec(k, n_params)
  }

  # calculate evppi at each wtp and new sample size
  for (l in seq_len(n_wtps)) {

    # run the metamodels
    mms <- withCallingHandlers(
      metamodel(analysis = "multiway",
                psa = psa,
                params = params,
                outcome = outcome,
                wtp = wtp[l],
                type = "gam",
                poly.order = 2,
                k = k),
      dampack_metamodel_spec = function(w) invokeRestart("muffleWarning")
    )

    l_mms[[l]] <- mms

    if (progress == TRUE) {
      if (l / (n_wtps / 10) == round(l / (n_wtps / 10), 0)) { # display progress every 10%
        cat("\r", paste(l / n_wtps * 100, "% done", sep = " "))
      }
    }

    if (n_by_param == TRUE) {
      # predict from the regression models
      l_predicted_loss <- lapply(mms$mods, function(m) predict_ga(m, n, n0))

      # bind the columns to get a dataframe
      df_predicted_loss <- bind_cols(l_predicted_loss)

      # calculate the evsi as the average of the row maxima
      v_row_maxes <- apply(df_predicted_loss, 1, max)
      m_evsi[l] <- mean(v_row_maxes) * pop
    } else {
      for (i in seq_len(n_n)) {
        # predict from the regression models
        l_predicted_loss <- lapply(mms$mods, function(m) predict_ga(m, unname(n[i]), n0))

        # bind the columns to get a dataframe
        df_predicted_loss <- bind_cols(l_predicted_loss)

        # calculate the evsi as the average of the row maxima
        v_row_maxes <- apply(df_predicted_loss, 1, max)
        m_evsi[l, i] <- mean(v_row_maxes) * pop
      }
    }
  }

  if (n_by_param == TRUE) {
    df_evsi <- data.frame("WTP" = wtp, "EVSI" = m_evsi)
  } else {
    # data.frame to store EVPPI for each WTP threshold
    df_evsi <- data.frame("WTP" = rep(wtp, n_n),
                          "n" = rep(n, each = n_wtps),
                          "EVSI" = c(m_evsi))
  }
  evsi_ls <- list(df_evsi = df_evsi, metamodel_ls = l_mms)
  class(evsi_ls) <- c("evsi")
  attr(evsi_ls, "currency") <- psa$currency
  attr(evsi_ls, "outcome") <- outcome_input
  evsi_ls
}


#' Function to compute the preposterior for each of the
#' basis functions of the GAM model.
#'
#' @keywords internal
#'
#' @param object gam object with an intercept and smooth terms only
#' (\code{s()} of one parameter, \code{ti()} or \code{te()}).
#' @param n scalar or vector of new sample size to compute evsi on. A vector
#' has one value per parameter; a named vector is matched to the parameters by
#' name, and an unnamed vector follows the order of the parameters in the model.
#' @param n0 scalar or vector of effective prior sample size, in the same
#' format as \code{n}.
#' @return the conditional loss associated with a gam, effective prior sample size,
#' and new sample size
#' @importFrom stats coef
predict_ga <- function(object, n, n0) {
  # Parameter values (all model frame columns but the response)
  df_param_vals <- object$model[, -1, drop = FALSE]
  v_param_names <- colnames(df_param_vals)

  # The Gaussian approximation is defined for an intercept plus smooth terms
  if (object$nsdf != 1) {
    stop("predict_ga() supports GAMs with an intercept and smooth terms only.",
         call. = FALSE)
  }

  # One value of n and n0 per parameter, in the order of v_param_names
  v_n <- match_to_params(n, v_param_names, "n")
  v_n0 <- match_to_params(n0, v_param_names, "n0")

  # Variance reduction factor, by parameter name
  v_vrf <- sqrt(v_n / (v_n + v_n0))
  names(v_vrf) <- v_param_names

  # Initialize matrix for preposterior of total basis functions
  m_x <- matrix(NA, nrow(object$model), length(object$coefficients))
  m_x[, 1] <- 1

  for (sm in object$smooth) {
    if (inherits(sm, "tensor.smooth")) {
      # one factor per margin, matched by the margin's parameter name
      v_vrf_margin <- vapply(sm$margin, function(mg) {
        if (length(mg$term) != 1) {
          stop("predict_ga() supports tensor smooths with one parameter per margin only.",
               call. = FALSE)
        }
        v_vrf[[mg$term]]
      }, numeric(1))
      m_x_frag <- predict_matrix_tensor_smooth_ga(sm, df_param_vals, v_vrf_margin)
    } else {
      if (length(sm$term) != 1 || inherits(sm, "t2.smooth")) {
        stop("predict_ga() supports s() smooths of one parameter, ti() and te() only; ",
             "found ", sm$label, ".", call. = FALSE)
      }
      m_x_frag <- predict_smooth_ga(sm, df_param_vals, v_vrf[[sm$term]])
    }
    m_x[, sm$first.para:sm$last.para] <- m_x_frag
  }

  # Compute conditional loss
  m_x %*% coef(object)
}

#' Match a sample size to the parameters of a metamodel
#'
#' Expands a scalar, or reorders a named vector, so that it has one value per
#' parameter in the order of \code{v_param_names}.
#'
#' @param x scalar, vector with one value per parameter, or named vector.
#' @param v_param_names names of the parameters in the metamodel.
#' @param arg name of the argument, for error messages.
#' @return a numeric vector with one value per parameter.
#' @keywords internal
match_to_params <- function(x, v_param_names, arg) {
  n_params <- length(v_param_names)
  if (!is.numeric(x) || length(x) == 0 || anyNA(x) || any(x < 0)) {
    stop("'", arg, "' must be a non-negative number or a vector of them.", call. = FALSE)
  }
  if (!is.null(names(x))) {
    v_unknown <- setdiff(names(x), v_param_names)
    v_missing <- setdiff(v_param_names, names(x))
    if (length(v_unknown) > 0 || length(v_missing) > 0) {
      stop("The names of '", arg, "' must be the parameters in the metamodel (",
           paste(v_param_names, collapse = ", "), ").",
           if (length(v_unknown) > 0) paste0(" Unknown: ", paste(v_unknown, collapse = ", "), "."),
           if (length(v_missing) > 0) paste0(" Missing: ", paste(v_missing, collapse = ", "), "."),
           call. = FALSE)
    }
    return(unname(x[v_param_names]))
  }
  if (length(x) == 1) {
    return(rep(x, n_params))
  }
  if (length(x) != n_params) {
    stop("'", arg, "' should be either a scalar or a vector with one value per parameter (",
         n_params, ": ", paste(v_param_names, collapse = ", "), ").", call. = FALSE)
  }
  x
}

#' Function to compute the preposterior for each of the
#' basis functions of a smooth for one parameter
#'
#' @keywords internal
#' @return Pre-posterior for each basis function
#' @importFrom mgcv PredictMat
predict_smooth_ga <- function(object, param_vals, vrf = 1) {
  # Produce basis functions for one parameter
  m_x <- PredictMat(object, param_vals)

  # Number of observations
  n_obs <- nrow(m_x)

  # Apply variance reduction to compute the preposterior
  # for each of the basis functions
  # Vector of ones
  m_ones <- matrix(1, n_obs, 1)

  # Compute phi on each of the basis function
  m_x <- vrf * m_x + (1 - vrf) * (m_ones %*% colMeans(m_x))

  m_x
}

#' Predict matrix tensor smooth (GA)
#'
#' @description
#' Function to compute the preposterior for each of the
#' basis functions for one or more parameters and calculates
#' the tensor product if more than one parameter is selected
#' (Heavily based on function Predict.matrix.tensor.smooth from
#' mgcv package)
#' @return tensor product based on list of pre-posterior distributions
#' @keywords internal
#' @importFrom mgcv tensor.prod.model.matrix Predict.matrix PredictMat
predict_matrix_tensor_smooth_ga <- function(object,
                                            param_vals,
                                            vrf = rep(1, ncol(param_vals))) {


  m <- length(object$margin)
  x <- list()
  for (i in seq_len(m)) {
    term <- object$margin[[i]]$term
    dat <- list()
    for (j in seq_along(term)) {
      dat[[term[j]]] <- param_vals[[term[j]]]
    }
    x[[i]] <- if (isTRUE(object$mc[i])) {
      PredictMat(object$margin[[i]], dat, n = length(dat[[1]]))
    } else {
      Predict.matrix(object$margin[[i]], dat)
    }
    n_obs <- nrow(x[[i]])
  }
  mxp <- length(object$XP)
  if (mxp > 0) {
    for (i in seq_len(mxp)) if (!is.null(object$XP[[i]])) {
      x[[i]] <- x[[i]] %*% object$XP[[i]]
    }
  }

  # Apply variance reduction to compute the preposterior
  # for each of the basis functions
  # Vector of ones
  ones <- matrix(1, n_obs, 1)

  # Initialize and fill list with preposterior of basis functions
  # for each parameter
  x_ga <- list()
  for (i in seq_len(m)) {
    x_ga[[i]] <- vrf[i] * x[[i]] + (1 - vrf[i]) * (ones %*% colMeans(x[[i]]))
  }

  # Compute tensor product
  t_ga <- tensor.prod.model.matrix(x_ga)

  t_ga
}

#' Plot of Expected Value of Sample Information (EVSI)
#'
#' @description
#' Plots the \code{evsi} object created by \code{\link{calc_evsi}}.
#' EVSI is either plotted as a function of additional sample size
#' for each willingness-to-pay threshold provided, or
#' as a function of each willingness-to-pay
#' threshold, depending upon the usage of \code{\link{calc_evsi}}
#' used to create the \code{evsi} object.
#'
#' @param x object of class \code{evsi}, produced by function
#'  \code{\link{calc_evsi}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: the currency of the PSA the results were computed from (or $), but it could be any
#'  currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness, in the plural (for example \code{"QALYs"} or
#' \code{"life-years"}). Default: \code{"QALYs"}. The willingness-to-pay axis uses the singular
#' ("$/QALY"), and a value already in the singular is used as given.
#' @inheritParams add_common_aes
#' @concept expected value of sample information
#' @return A \code{ggplot2} plot with the EVSI
#' @seealso \code{\link{calc_evsi}}
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#'
#' # one WTP threshold and several sample sizes: EVSI by additional sample size
#' l_evsi <- calc_evsi(l_psa, wtp = 1e5, params = "pFailSurg", outcome = "nmb",
#'                     n = c(10, 50, 100, 200), n0 = 50, progress = FALSE)
#' plot(l_evsi)
#'
#' \donttest{
#' # several WTP thresholds and one sample size: EVSI by WTP threshold
#' l_evsi_wtp <- calc_evsi(l_psa, wtp = c(5e4, 1e5, 2e5), params = "pFailSurg",
#'                         outcome = "nmb", n = 100, n0 = 50, progress = FALSE)
#' plot(l_evsi_wtp)
#' }
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evsi <- function(x,
                      txtsize = 12,
                      currency = NULL,
                      effect_units = "QALYs",
                      n_y_ticks = 8,
                      n_x_ticks = 20,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlim = c(0, NA),
                      ylim = NULL,
                      col = c("access", "full", "bw"),
                      ...) {
  # Select EVSI data.frame, discarding metamodel list
  EVSI <- WTP_thou <- NULL
  currency <- resolve_currency(currency, x)
  v_units <- voi_units(x, currency, effect_units)
  x <- x[[1]]
  x$WTP_thou <- x$WTP / 1000
  col <- match.arg(col)
  if (length(unique(x$WTP)) == 1) {
    col <- "bw"
  }
  scale_text <- paste("Willingness to Pay\n(Thousand ", currency, "/", per_unit(effect_units), ")", sep = "")

  if (length(unique(x$WTP)) == 1 && "n" %in% names(x)) {
    g <- ggplot(data = x,
                aes(x = n, y = EVSI)) +
      xlab("Additional Sample Size")
  } else if (!("n" %in% names(x)) || ("n" %in% names(x) && length(unique(x$n)) == 1)) {
    g <- ggplot(data = x,
                aes(x = WTP_thou, y = EVSI)) +
      xlab(scale_text)
  } else if (length(unique(x$WTP)) > 1 && length(unique(x$n)) > 1) {
    x$WTP_thou <- as.factor(x$WTP_thou)
    g <- ggplot(data = x,
                aes(x = n, y = EVSI, color = WTP_thou)) +
      xlab("Additional Sample Size")
  } else {
    stop("Insufficient data to plot")
  }

  g <- g +
    geom_line() +
    ylab(paste("EVSI (", v_units, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim, scale_name = scale_text,
                 col = col, ...)
}
