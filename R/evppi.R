#' Estimation of the Expected Value of Partial Perfect Information (EVPPI)
#' using a linear regression metamodel approach
#'
#' \code{evppi} is used to estimate the Expected Value of Partial Perfect
#' Information (EVPPI) using a linear regression metamodel approach from a
#' probabilistic sensitivity analysis (PSA) dataset.
#'
#' @param psa object of class psa, produced by \code{\link{make_psa_obj}}
#' @param wtp willingness-to-pay threshold
#' @param params A vector of parameters for which to analyze the EVPPI.
#' @param outcome either net monetary benefit (\code{"nmb_loss"})
#' or net health benefit (\code{"nhb_loss"})
#' @param type either generalized additive models (\code{"gam"}) or
#' polynomial models (\code{"poly"})
#' @param poly.order order of the polynomial, if \code{type == "poly"}
#' @param k basis dimension, if \code{type == "gam"}
#'
#' @return evppi A numeric vector of size one with the EVPPI of the selected
#' parameters
#'
#' @details
#' The expected value of partial pefect information (EVPPI) is the expected
#' value of perfect information from a subset of parameters of interest,
#' \eqn{\theta_I} of a cost-effectiveness analysis (CEA) of \eqn{D} different
#' strategies with parameters \eqn{\theta = \{ \theta_I, \theta_C\}}, where
#' \eqn{\theta_C} is the set of complimenatry parameters of the CEA. The
#' function \code{evppi_lrmm} computes the EVPPI of \eqn{\theta_I} from a
#' matrix of net monetary benefits \eqn{B} of the CEA. Each column of \eqn{B}
#' corresponds to the net benefit \eqn{B_d} of strategy \eqn{d}. The function
#' \code{evppi_lrmm} computes the EVPPI using a linear regression metamodel
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
                       outcome = c("nmb_loss", "nhb_loss"),
                       type = c("gam", "poly"),
                       poly.order = 2,
                       k = -1) {
  # define parameter values and make sure they correspond to a valid option
  type <- match.arg(type)
  outcome <- match.arg(outcome)

  # run the metamodels
  mms <- metamodel(analysis = "multiway",
                   psa = psa,
                   params = params,
                   outcome = outcome,
                   wtp = wtp,
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
  evppi <- mean(row_maxes)

  return(evppi)
}
