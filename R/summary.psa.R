#' summarize a psa object across all simulations
#'
#' @param object the psa object
#' @param ... further arguments to summary (not used)
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#'
#' @importFrom stats sd
#' @export
summary.psa <- function(object, ..., calc_sds = FALSE) {
  mean_cost <- colMeans(object$cost)
  mean_effect <- colMeans(object$effectiveness)
  strat <- object$strategies
  sum_psa <- data.frame("Strategy" = strat,
                        "meanCost" = mean_cost,
                        "meanEffect" = mean_effect,
                        stringsAsFactors = FALSE)
  if (calc_sds) {
    sd_cost <- apply(object$cost, 2, sd)
    sd_effect <- apply(object$effectiveness, 2, sd)
    sum_psa[, "sdCost"] <- sd_cost
    sum_psa[, "sdEffect"] <- sd_effect
  }
  rownames(sum_psa) <- 1:nrow(sum_psa)
  sum_psa
}
