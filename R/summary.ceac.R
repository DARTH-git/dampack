
#' summary function for the ceac object
#'
#' @param ceac_obj object returned from the \code{ceac} function
#' @param ... further arguments (not used)
#' @export
summary.ceac <- function(ceac_obj, ...){
  front <- ceac_obj[ceac_obj$On_Frontier == TRUE, ]
  n.wtps <- nrow(front)
  strat_on_front <- front$Strategy
  lagged_strat <- c(strat_on_front[-1], strat_on_front[n.wtps])
  switches <- which(strat_on_front != lagged_strat) + 1
  n_switches <- length(switches)
  # first, declare the number of switches in the optimal strategy
  if (n_switches == 0) {
    cat("There were no switches in the optimal strategy.",
        "The optimal strategy is",
        unique(front$Strategy),
        "at all willingness to pay thresholds.")
  }
  if (n_switches == 1) {
    cat("There was one switch in the optimal strategy.\n")
    print_switch(1, switches, front)
  }
  if (n_switches >= 2) {
    cat(c("There were", n_switches, "switches in the optimal strategy"))
    for (i in 1:n_switches) {
      print_switch(i, switches, front)
    }
  }
}

print_switch <- function(i, switches, front){
  cat("At WTP =", front$WTP[switches[i] + 1],
      "the optimal strategy changed from",
      front$Strategy[switches[i] - 1], "to",
      front$Strategy[switches[i]])
}
