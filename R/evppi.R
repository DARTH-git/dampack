#' Estimation of the Expected Value of Partial Perfect Information (EVPPI)
#' using a linear regression metamodel approach
#'
#' \code{evppi} is used to estimate the Expected Value of Partial Perfect
#' Information (EVPPI) using a linear regression metamodel approach from a
#' probabilistic sensitivity analysis (PSA) dataset.
#'
#' @param psa object of class psa, produced by \code{\link{make_psa_obj}}
#' @param wtp willingness-to-pay threshold
#' @param params A vector of parameter names to be analyzed in terms of EVPPI.
#' @param outcome either net monetary benefit (\code{"nmb"})
#' or net health benefit (\code{"nhb"})
#' @param type either generalized additive models (\code{"gam"}) or
#' polynomial models (\code{"poly"})
#' @param poly.order order of the polynomial, if \code{type == "poly"}
#' @param k basis dimension, if \code{type == "gam"}
#' @param pop scalar that corresponds to the total population
#' @param progress \code{TRUE} or \code{FALSE} for whether or not function progress
#' should be displayed in console.
#'
#' @return A data.frame with WTP thresholds and corresponding EVPPIs
#' for the selected parameters
#'
#' @details
#' The expected value of partial pefect information (EVPPI) is the expected
#' value of perfect information from a subset of parameters of interest,
#' \eqn{\theta_I}, of a cost-effectiveness analysis (CEA) of \eqn{D} different
#' strategies with parameters \eqn{\theta = \{ \theta_I, \theta_C\}}, where
#' \eqn{\theta_C} is the set of complimenatry parameters of the CEA. The
#' function \code{calc_evppi} computes the EVPPI of \eqn{\theta_I} from a
#' matrix of net monetary benefits \eqn{B} of the CEA. Each column of \eqn{B}
#' corresponds to the net benefit \eqn{B_d} of strategy \eqn{d}. The function
#' \code{calc_evppi} computes the EVPPI using a linear regression metamodel
#' approach following these steps:
#' \enumerate{
#' \item Determine the optimal strategy \eqn{d^*} from the expected net
#' benefits \eqn{\bar{B}}
#' \deqn{d^* = argmax_{d} \{\bar{B}\}}
#' \item Compute the opportunity loss for each \eqn{d} strategy, \eqn{L_d}
#' \deqn{L_d = B_d - B_{d^*}}
#' \item Estimate a linear metamodel for the opportunity loss of each \eqn{d}
#' strategy, \eqn{L_d}, by regressing them on the spline basis functions of
#' \eqn{\theta_I}, \eqn{f(\theta_I)}
#' \deqn{L_d = \beta_0 + f(\theta_I) + \epsilon,}
#' where \eqn{\epsilon} is the residual term that captures the complementary
#' parameters \eqn{\theta_C} and the difference between the original simulation
#' model and the metamodel.
#' \item Compute the EVPPI of \eqn{\theta_I} using the estimated losses for
#' each \eqn{d} strategy, \eqn{\hat{L}_d} from the linear regression metamodel
#' and applying the following equation:
#' \deqn{EVPPI_{\theta_I} = \frac{1}{K}\sum_{i=1}^{K}\max_d(\hat{L}_d)}
#' The spline model in step 3 is fitted using the `mgcv` package.
#' }

#' @references
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A General Gaussian Approximation Approach
#' for Value of Information Analysis. Med Decis Making. 2018;38(2):174-188.
#' \item Strong M, Oakley JE, Brennan A. Estimating Multiparameter Partial
#' Expected Value of Perfect Information from a Probabilistic Sensitivity
#' Analysis Sample: A Nonparametric Regression Approach. Med Decis Making.
#' 2014;34(3):311–26.
#' }
#'
#' @importFrom dplyr bind_cols
#'
#' @export
calc_evppi <- function(psa,
                       wtp,
                       params = NULL,
                       outcome = c("nmb", "nhb"),
                       type = c("gam", "poly"),
                       poly.order = 2,
                       k = -1,
                       pop = 1,
                       progress = TRUE) {
  # define parameter values and make sure they correspond to a valid option
  type <- match.arg(type)
  outcome <- match.arg(outcome)

  # adjust outcome type
  outcome <- paste0(outcome, "_loss_voi")

  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evppi
  evppi <- rep(0, n_wtps)

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

    # get the fitted loss values from the regression models
    # there is one for each strategy
    fitted_loss_list <- lapply(mms$mods, function(m) m$fitted.values)

    # bind the columns to get a dataframe
    fitted_loss_df <- bind_cols(fitted_loss_list)

    # calculate the evppi as the average of the row maxima
    row_maxes <- apply(fitted_loss_df, 1, max)
    evppi[l] <- mean(row_maxes) * pop

    if (progress == TRUE) {
      if (l / (n_wtps / 10) == round(l / (n_wtps / 10), 0)) { # display progress every 10%
        cat("\r", paste(l / n_wtps * 100, "% done", sep = " "))
      }
    }
  }

  # data.frame to store EVPPI for each WTP threshold
  df_evppi <- data.frame("WTP" = wtp, "EVPPI" = evppi)
  class(df_evppi) <- c("evppi", "data.frame")
  return(df_evppi)
}

#' Plot of Expected Value of Partial Perfect Information (EVPPI)
#'
#' @description
#' Plots the \code{evppi} object created by \code{\link{calc_evppi}}.
#'
#' @param x object of class \code{evppi}, produced by function
#'  \code{\link{calc_evppi}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @inheritParams add_common_aes
#' @keywords expected value of perfect information
#' @return A \code{ggplot2} plot with the EVPPI
#' @seealso \code{\link{calc_evppi}}
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evppi <- function(x,
                       txtsize = 12,
                       currency = "$",
                       effect_units = "QALY",
                       n_y_ticks = 8,
                       n_x_ticks = 20,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = c(0, NA),
                       ylim = NULL,
                       ...) {
  x$WTP_thou <- x$WTP / 1000
  g <- ggplot(data = x,
              aes_(x = as.name("WTP_thou"), y = as.name("EVPPI"))) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPPI (", currency, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}
