#' Calculate Expected Value of Sample Information
#'
#' @inheritParams calc_evppi
#' @param n additional sample size
#' @param n0 initial sample size
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
                      pop = 1) {
  # define parameter values and make sure they correspond to a valid option
  type <- match.arg(type)
  outcome <- match.arg(outcome)

  # adjust outcome type
  outcome <- paste0(outcome, "_loss_voi")

  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evsi
  evsi <- rep(0, n_wtps)

  # calculate evppi at each wtp
  for (l in 1:n_wtps) {
    # run the metamodels
    mms <- metamodel(analysis = "multiway",
                     psa = psa,
                     params = params,
                     outcome = outcome,
                     wtp = wtp[l],
                     type = type,
                     poly.order = poly.order,
                     k = k)

    # predict from the regression models
    predicted_loss_list <- lapply(mms$mods, function(m) predict_ga(m, n, n0))

    # bind the columns to get a dataframe
    predicted_loss_df <- bind_cols(predicted_loss_list)

    # calculate the evsi as the average of the row maxima
    row_maxes <- apply(predicted_loss_df, 1, max)
    evsi[l] <- mean(row_maxes) * pop
  }

  # data.frame to store EVPPI for each WTP threshold
  df_evsi <- data.frame("WTP" = wtp, "EVSI" = evsi)
  class(df_evsi) <- "data.frame"
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
  for (i in 1:m) {
    term <- object$margin[[i]]$term
    dat <- list()
    for (j in seq_len(term)) {
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
    for (i in 1:mxp) if (!is.null(object$XP[[i]])) {
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
  for (i in 1:m) {
    x_ga[[i]] <- vrf[i] * x[[i]] + (1 - vrf[i]) * (ones %*% colMeans(x[[i]]))
  }

  # Compute tensor product
  t_ga <- tensor.prod.model.matrix(x_ga)

  return(t_ga)
}
