#' Calculate Expected Value of Sample Information (EVSI)
#'
#' @inheritParams calc_evppi
#' @param n sample size of the proposed study (the additional sample size).
#' @param n0 effective prior sample size: the number of observations that the
#' current information about each parameter is worth (see Details).
#' @param n_by_param if \code{TRUE}, each parameter in the metamodel can have its
#' own effective prior sample size and additional sample size. \code{n} and
#' \code{n0} must then be numeric vectors with one value per parameter in
#' \code{params}. By default, \code{n_by_param = FALSE}, and each value of
#' \code{n} and \code{n0} is shared by all the parameters in the model. Named
#' vectors of \code{n} and \code{n0} are matched to \code{params} by name;
#' unnamed vectors follow the order of \code{params}. When
#' \code{n_by_param = FALSE}, \code{n0} must be a single numeric value, and
#' \code{n} must be a numeric vector of additional sample sizes for which EVSI is
#' calculated from the metamodel.
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#' should be displayed in console.
#'
#' @return A list containing 1) a data.frame with WTP thresholds, new prospective sample sizes
#'  (if \code{n_by_param == FALSE}), and corresponding EVSIs
#' for the selected parameters and 2) a list of metamodels used to estimate EVSI for each
#' strategy at each willingness to pay threshold.
#'
#' @details
#' The expected value of sample information (EVSI) is the expected value of a
#' study of \eqn{n} observations that informs the parameters of interest,
#' \eqn{\theta_I}, of a cost-effectiveness analysis of \eqn{D} strategies with
#' net benefits \eqn{B_d}. If \eqn{X_n} are the data the study would collect,
#' \deqn{EVSI = E_{X_n}\left[\max_d E(B_d \mid X_n)\right] - \max_d E(B_d).}{
#' EVSI = E_X[max_d E(B_d | X)] - max_d E(B_d).}
#' \code{calc_evsi} estimates it from the PSA sample alone, with the Gaussian
#' approximation (GA) of Jalal and Alarid-Escudero (2018):
#' \enumerate{
#' \item Determine the optimal strategy with current information,
#' \eqn{d^* = argmax_d \{\bar{B}_d\}}{d* = argmax_d mean(B_d)}, and the opportunity loss of each strategy,
#' \eqn{L_d = B_d - B_{d^*}}{L_d = B_d - B_d*}.
#' \item Fit a metamodel of the loss of each strategy on \eqn{\theta_I}, as in
#' \code{\link{calc_evppi}}, which is a linear combination of basis functions,
#' \deqn{L_d = \beta_0 + \sum_j \beta_j x_j(\theta_I) + \epsilon.}
#' \item Approximate the preposterior distribution, the distribution of
#' \eqn{E(\theta \mid X_n)}{E(theta | X_n)} over the studies that could be observed, by
#' shrinking each PSA sample of a parameter toward its mean by a factor that
#' depends only on \eqn{n} and the effective prior sample size, \eqn{n_0}:
#' \deqn{\tilde{\theta} = E(\theta) + \sqrt{\frac{n}{n + n_0}} \left(\theta - E(\theta)\right).}{
#' theta~ = E(theta) + sqrt(n / (n + n0)) (theta - E(theta)).}
#' This is exact for a normal mean with a conjugate normal prior, whose
#' preposterior variance is \eqn{n / (n + n_0)} times the prior variance, and an
#' approximation otherwise. The GA applies the shrinkage to each basis function
#' of the metamodel,
#' \eqn{\tilde{x}_j = \bar{x}_j + \sqrt{n / (n + n_0)} (x_j - \bar{x}_j)}{x~_j = mean(x_j) + sqrt(n / (n + n0)) (x_j - mean(x_j))};
#' for an interaction, it shrinks the basis of each parameter with that
#' parameter's \eqn{n} and \eqn{n_0} before forming their tensor product.
#' \item Compute the expected conditional loss of each strategy,
#' \eqn{\tilde{L}_d = \beta_0 + \sum_j \beta_j \tilde{x}_j}{L~_d = beta_0 + sum_j beta_j x~_j}, and
#' \deqn{EVSI = \frac{1}{K}\sum_{i=1}^{K}\max_d(\tilde{L}_{d,i}),}{
#' EVSI = (1/K) sum_i max_d(L~_d,i),}
#' where \eqn{K} is the number of PSA samples.
#' }
#' EVSI is 0 for \eqn{n = 0} and tends to the EVPPI of \eqn{\theta_I}
#' (\code{\link{calc_evppi}}) as \eqn{n} grows.
#'
#' The effective prior sample size, \eqn{n_0}, is the number of observations
#' that the current information about a parameter is worth, so that a study of
#' \eqn{n} observations reduces its variance by the factor \eqn{n_0 / (n + n_0)}.
#' With a conjugate prior it can be read from the prior: \eqn{n_0 = \alpha + \beta}
#' for a probability with a beta(\eqn{\alpha}, \eqn{\beta}) prior and binomial
#' data; \eqn{n_0 = \sigma^2 / \sigma_0^2} for a normal mean with prior variance
#' \eqn{\sigma_0^2} and data with variance \eqn{\sigma^2}; and, for a rate
#' with a gamma prior of rate parameter \eqn{\beta} and Poisson counts, the
#' person-time \eqn{\beta}. When no conjugate prior applies, Jalal and
#' Alarid-Escudero (2018, Appendix C) show how to estimate \eqn{n_0} with a
#' summary statistic or by Markov chain Monte Carlo. \eqn{n} and \eqn{n_0} must
#' be in the same units (for example, patients or person-years).
#'
#' Use \code{n_by_param = TRUE} when the study would inform the parameters with
#' different sample sizes, or when they have different effective prior sample
#' sizes. Each parameter is then shrunk with its own factor, which ignores how
#' data on one parameter would also update the parameters correlated with it.
#'
#' @export
calc_evsi <- function(psa,
                      wtp,
                      params = NULL,
                      outcome = c("nhb", "nmb"),
                      k = -1,
                      n = 100,
                      n0 = 10,
                      n_by_param = FALSE,
                      pop = 1,
                      progress = TRUE) {
  # define parameter values and make sure they correspond to a valid option
  outcome <- match.arg(outcome)

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
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @inheritParams add_common_aes
#' @keywords expected value of sample information
#' @return A \code{ggplot2} plot with the EVSI
#' @seealso \code{\link{calc_evsi}}
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evsi <- function(x,
                      txtsize = 12,
                      currency = "$",
                      effect_units = "QALY",
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
  x <- x[[1]]
  x$WTP_thou <- x$WTP / 1000
  col <- match.arg(col)
  if (length(unique(x$WTP)) == 1) {
    col <- "bw"
  }
  scale_text <- paste("Willingness to Pay\n(Thousand ", currency, "/", effect_units, ")", sep = "")

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
    ylab(paste("EVSI (", currency, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim, scale_name = scale_text,
                 col = col, ...)
}
