
#' @importFrom dplyr bind_cols
#'
#' @export
calc_evppi <- function(psa,
                       wtp,
                       parms = NULL,
                       outcome = c("nhb_loss", "nmb_loss"),
                       type = c("gam", "poly"),
                       poly.order = 2,
                       k = NA) {
  # define parameter values and make sure they correspond to a valid option
  type <- match.arg(type)
  outcome <- match.arg(outcome)

  # run the metamodels
  mms <- metamodel(analysis = "multiway",
                   psa = psa,
                   parms = parms,
                   outcome = outcome,
                   wtp = wtp,
                   type = type,
                   poly.order = poly.order,
                   k = k)

  # get the fitted loss values from the regression models
  # there is one for each strategy
  fitted_loss_list <- lapply(mms$mods, function(m) m$fitted.values)

  # bind the columns to get a dataframe, and multiply by -1 to get losses positive
  fitted_loss_df <- -1 * bind_cols(fitted_loss_list)

  # calculate the evppi as the average of the row maxima
  row_maxes <- apply(fitted_loss_df, 1, max)
  evppi <- mean(row_maxes)

  return(evppi)
}
