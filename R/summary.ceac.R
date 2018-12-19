
#' summary function for the ceac object
#'
#' @param object object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @export
summary.ceac <- function(object, ...){
  front <- object[object$On_Frontier == TRUE, ]
  wtp <- front$WTP
  wtp_range <- range(wtp)
  n.wtps <- length(wtp)

  # get the indices where the CE strategy isn't the same as the following CE strategy
  strat_on_front <- front$Strategy
  lagged_strat <- c(strat_on_front[-1], strat_on_front[n.wtps])
  switches <- which(strat_on_front != lagged_strat) + 1
  n_switches <- length(switches)
  # strat_on_front[switches] are the optimal strategies at wtp[switches]
  if (n_switches == 0) {
    wtp_min <- wtp_range[1]
    wtp_max <- wtp_range[2]
    one_strat <- unique(front$Strategy)
    sum_df <- data.frame(wtp_min,
                         wtp_max,
                         one_strat)
  } else {
    # build up summary data frame
    sum_df <- NULL
    for (i in 1:n_switches) {
      if (i == 1) {
        sum_df_row_first <- data.frame(wtp_range[1],
                                       wtp[switches],
                                       strat_on_front[switches - 1],
                                       fix.empty.names = FALSE,
                                       stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_first)
      }
      if (i == n_switches) {
        sum_df_row_last <- data.frame(wtp[switches],
                                      wtp_range[2],
                                      strat_on_front[switches],
                                      fix.empty.names = FALSE,
                                      stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_last)
      }
      if (i > 1) {
        sum_df_row_middle <- data.frame(wtp[switches[i]],
                                        wtp[switches[i + 1]],
                                        strat_on_front[switches[i]],
                                        fix.empty.names = FALSE,
                                        stringsAsFactors = FALSE)
        sum_df <- rbind(sum_df, sum_df_row_middle)
      }
    }
  }
  names(sum_df) <- c("range_min", "range_max", "cost_eff_strat")
  sum_df
}
