#' Calculate Expected Value of Sample Information
#'
#' @inheritParams calc_evppi
#' @param n additional sample size
#' @param n0 initial sample size
#' @param n_by_param if \code{TRUE}, each parameter in the metamodel can have a unique
#' initial and additional sample size. \code{n} and \code{n0} must be numerical
#' vectors of equal length to \code{params}, with each value corresponding to the
#' initial and additional sample sizes for each parameter in the metamodel. By default,
#' \code{n_by_param = FALSE}, and each value of \code{n} and \code{n0} is shared by
#' each parameter in the model. When \code{n_by_param = FALSE}, \code{n0} must be a single
#' numeric value, and \code{n} must be a numerical vector of additional sample sizes for
#' which EVSI is calculated from the metamodel.
#'
#' @export
calc_evsi <- function(psa,
                      wtp,
                      params = NULL,
                      outcome = c("nhb", "nmb"),
                      type = c("gam", "poly"),
                      poly.order = 2,
                      k = -1,
                      n = 100,
                      n0 = 10,
                      n_by_param = FALSE,
                      pop = 1) {
  # define parameter values and make sure they correspond to a valid option
  type <- match.arg(type)
  outcome <- match.arg(outcome)

  # adjust outcome type
  outcome <- paste0(outcome, "_loss_voi")

  # number of wtp thresholds
  n_wtps <- length(wtp)

  # number of new sample sizes
  n_n <- length(n)

  if(n_by_param == TRUE) {
    # vector to store evsi
    evsi <- rep(0, n_wtps)
  } else {
    # matrix to store evsi
    evsi <- matrix(rep(0, n_wtps * n_n), ncol = n_n)
  }

  # calculate evppi at each wtp and new sample size
  for (l in seq_len(n_wtps)) {
    # run the metamodels
    mms <- metamodel(analysis = "multiway",
                     psa = psa,
                     params = params,
                     outcome = outcome,
                     wtp = wtp[l],
                     type = type,
                     poly.order = poly.order,
                     k = k)

    if(n_by_param == TRUE) {
      # predict from the regression models
      predicted_loss_list <- lapply(mms$mods, function(m) predict_ga(m, n, n0))

      # bind the columns to get a dataframe
      predicted_loss_df <- bind_cols(predicted_loss_list)

      # calculate the evsi as the average of the row maxima
      row_maxes <- apply(predicted_loss_df, 1, max)
      evsi[l] <- mean(row_maxes) * pop
    } else {
      for (i in seq_len(n_n)) {
        # predict from the regression models
        predicted_loss_list <- lapply(mms$mods, function(m) predict_ga(m, n[i], n0))

        # bind the columns to get a dataframe
        predicted_loss_df <- bind_cols(predicted_loss_list)

        # calculate the evsi as the average of the row maxima
        row_maxes <- apply(predicted_loss_df, 1, max)
        evsi[l, i] <- mean(row_maxes) * pop
      }
    }
  }

  if(n_by_param == TRUE) {
    df_evsi <- data.frame("WTP" = wtp, "EVSI" = evsi)
  } else {
    # data.frame to store EVPPI for each WTP threshold
    df_evsi <- data.frame("WTP" = rep(wtp, n_n),
                          "n" = rep(n, each = n_wtps),
                          "EVSI" = c(evsi))
  }
  class(df_evsi) <- c("evsi", "data.frame")
  return(df_evsi)
}


#' Function to compute the preposterior for each of the
#' basis functions of the GAM model.
#'
#' @keywords internal
#'
#' @param object gam object
#' @param n scalar or vector of new sample size to compute evsi on
#' @param n0 scalar or vector of effective prior sample size
#' @importFrom stats coef
predict_ga <- function(object, n, n0) {

  # Name of parameters
  param_names <- colnames(object$model)

  # Create dataframe with parameter values
  param_vals <- data.frame(object$model[, -1])

  # Name columns of dataframe
  colnames(param_vals) <- param_names[-1]

  # Number of parameters
  n_params <- ncol(param_vals)

  # Sanity checks
  if (!(length(n) == 1 | length(n) == n_params)) {
    stop("Variable 'n' should be either a scalar or a vector
         the same size as the number of parameters")
  }
  if (!(length(n0) == 1 | length(n0) == n_params)) {
    stop("Variable 'n0' should be either a scalar or a vector
         the same size as the number of parameters")
  }

  # Make n & n0 consistent with the number of parameters
  if (length(n) == 1) {
    n <- rep(n, n_params)
  }
  if (length(n0) == 1) {
    n0 <- rep(n0, n_params)
  }

  # Compute variance reduction factor
  vrf <- sqrt(n / (n + n0))

  # Number of smoothers
  n_smooth <- length(object$smooth)

  # Number of total basis functions
  n_col_x <- length(object$coefficients)

  # Number of observations
  n_row_x <- nrow(object$model)

  # Initialize matrix for preposterior of total basis functions
  x <- matrix(NA, n_row_x, n_col_x)
  x[, 1] <- 1

  for (k in 1:n_smooth) {
    klab <- substr(object$smooth[[k]]$label, 1, 1)
    if (klab == "s") {
      x_frag <- predict_smooth_ga(object$smooth[[k]], param_vals, vrf[k])
    } else {
      x_frag <- predict_matrix_tensor_smooth_ga(object$smooth[[k]], param_vals, vrf)
    }
    x[, object$smooth[[k]]$first.para:object$smooth[[k]]$last.para] <- x_frag
  }

  # Coefficients of GAM model
  beta <- coef(object)

  # Compute conditional Loss
  l_tilde <- x %*% beta

  return(l_tilde)
  }

#' Function to compute the preposterior for each of the
#' basis functions of a smooth for one parameter
#'
#' @keywords internal
#'
#' @importFrom mgcv PredictMat
predict_smooth_ga <- function(object, param_vals, vrf = 1) {
  # Produce basis functions for one parameter
  x <- PredictMat(object, param_vals)

  # Number of observations
  n_obs <- nrow(x)

  # Apply variance reduction to compute the preposterior
  # for each of the basis functions
  # Vector of ones
  ones <- matrix(1, n_obs, 1)

  # Compute phi on each of the basis function
  x <- vrf * x + (1 - vrf) * (ones %*% colMeans(x))

  return(x)
}

#' Predict matrix tensor smooth (GA)
#'
#' @description
#' Function to compute the preposterior for each of the
#' basis functions for one or more parameters and calculates
#' the tensor product if more than one parameter is selected
#' (Heavily based on function Predict.matrix.tensor.smooth from
#' mgcv package)
#'
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
    for (j in seq_len(length(term))) {
      dat[[term[j]]] <- param_vals[[term[j]]]
    }
    x[[i]] <- if (!is.null(object$mc[i])) {
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

  return(t_ga)
}

#' Plot of Expected Value of Sample Information (EVSI)
#'
#' @description
#' Plots the \code{evsi} object created by \code{\link{calc_evsi}}.
#'
#' @param x object of class \code{evsi}, produced by function
#'  \code{\link{calc_evsi}}
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units Units of effectiveness. Default: QALY
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
                       col = c("full", "bw"),
                       ...) {
  x$WTP_thou <- x$WTP / 1000
  col <- match.arg(col)
  if(length(unique(x$WTP)) == 1) {
    col = "bw"
  }
  scale_text <- paste("Willingness to Pay\n(Thousand ", currency, "/", effect_units, ")", sep = "")

  if(length(unique(x$WTP)) == 1 & "n" %in% names(x)) {
    g <- ggplot(data = x,
                aes_(x = as.name("n"), y = as.name("EVSI"))) +
      xlab("Additional Sample Size")
  } else if(!("n" %in% names(x))) {
    g <- ggplot(data = x,
                aes_(x = as.name("WTP_thou"), y = as.name("EVSI"))) +
      xlab(scale_text)
  } else {
    x$WTP_thou <- as.factor(x$WTP_thou)
    g <- ggplot(data = x,
                aes_(x = as.name("n"), y = as.name("EVSI"), color = as.name("WTP_thou"))) +
      xlab("Additional Sample Size")
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
