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
#' @return A list containing 1) a data.frame with WTP thresholds and corresponding EVPPIs
#' for the selected parameters and 2) a list of metamodels used to estimate EVPPI for each
#' strategy at each willingness to pay threshold.
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
#' @seealso \code{\link{plot.evppi}}, \code{\link{calc_evpi}}, \code{\link{calc_evsi}}
#'
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#'
#' # EVPPI of the two treatment-failure probabilities, jointly, at three WTP thresholds
#' l_evppi <- calc_evppi(l_psa,
#'                       wtp = c(5e4, 1e5, 2e5),
#'                       params = c("pFailChemo", "pFailSurg"),
#'                       outcome = "nmb",
#'                       k = 3,
#'                       progress = FALSE)
#' l_evppi$df_evppi
#' plot(l_evppi)
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
  outcome_input <- outcome

  # adjust outcome type
  outcome <- paste0(outcome, "_loss_voi")

  # number of wtp thresholds
  n_wtps <- length(wtp)
  # vector to store evppi
  v_evppi <- rep(0, n_wtps)

  l_mms <- vector(mode = "list", length = n_wtps)

  # calculate evppi at each wtp
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

    l_mms[[l]] <- mms

    # get the fitted loss values from the regression models
    # there is one for each strategy
    l_fitted_loss <- lapply(mms$mods, function(m) m$fitted.values)

    # bind the columns to get a dataframe
    df_fitted_loss <- bind_cols(l_fitted_loss)

    # calculate the evppi as the average of the row maxima
    v_row_maxes <- apply(df_fitted_loss, 1, max)
    v_evppi[l] <- mean(v_row_maxes) * pop

    if (progress == TRUE) {
      if (l / (n_wtps / 10) == round(l / (n_wtps / 10), 0)) { # display progress every 10%
        cat("\r", paste(l / n_wtps * 100, "% done", sep = " "))
      }
    }
  }

  # data.frame to store EVPPI for each WTP threshold
  df_evppi <- data.frame("WTP" = wtp, "EVPPI" = v_evppi)

  evppi_ls <- list(df_evppi = df_evppi, metamodel_ls = l_mms)
  class(evppi_ls) <- c("evppi")
  attr(evppi_ls, "currency") <- psa$currency
  attr(evppi_ls, "outcome") <- outcome_input
  evppi_ls
}

#' Plot of Expected Value of Partial Perfect Information (EVPPI)
#'
#' @description
#' Plots the \code{evppi} object created by \code{\link{calc_evppi}}.
#'
#' @param x object of class \code{evppi}, produced by function
#'  \code{\link{calc_evppi}}
#' @param currency string with currency used in the cost-effectiveness analysis (CEA).
#'  Default: the currency of the PSA the results were computed from (or $), but it could be any
#'  currency symbol or word (e.g., £, €, peso)
#' @param effect_units units of effectiveness. Default: QALY
#' @inheritParams add_common_aes
#' @concept expected value of partial perfect information
#' @return A \code{ggplot2} plot with the EVPPI
#' @seealso \code{\link{calc_evppi}}
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' l_evppi <- calc_evppi(l_psa, wtp = c(5e4, 1e5, 2e5), params = "pFailChemo",
#'                       progress = FALSE)
#' plot(l_evppi)
#'
#' # ggplot2 layers can be added to the plot
#' plot(l_evppi, n_x_ticks = 5) + ggtitle("EVPPI of pFailChemo")
#' @import ggplot2
#' @importFrom scales comma
#' @export
plot.evppi <- function(x,
                       txtsize = 12,
                       currency = NULL,
                       effect_units = "QALY",
                       n_y_ticks = 8,
                       n_x_ticks = 20,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = c(0, NA),
                       ylim = NULL,
                       ...) {
  WTP_thou <- EVPPI <- NULL
  currency <- resolve_currency(currency, x)
  v_units <- voi_units(x, currency, effect_units)
  x <- x[[1]]
  x$WTP_thou <- x$WTP / 1000
  g <- ggplot(data = x,
              aes(x = WTP_thou, y = EVPPI)) +
    geom_line() +
    xlab(paste("Willingness to Pay (Thousand ", currency, "/", effect_units, ")", sep = "")) +
    ylab(paste("EVPPI (", v_units, ")", sep = ""))
  add_common_aes(g, txtsize, continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}
