#' print a psa object
#'
#' @param x the psa object
#' @param ... further arguments to print (not used)
#' @param all_strat whether or not to print the full list of strategies. defaults to FALSE, which truncates
#' the strategy list to 5
#'
#' @export
print.psa <- function(x, ..., all_strat = FALSE) {
  cat("\n")
  cat("PSA object", "\n")
  cat("-------------------------------------------------", "\n")

  # cost
  cat("number of strategies (n_strategies):", x$n_strategies, "\n")
  n_trunc <- 5
  if (all_strat | (x$n_strategies <= n_trunc)) {
    s2print <- x$strategies
    msg <- ""
  } else {
    s2print <- c(x$strategies[1:n_trunc], "...")
    msg <- paste("(truncated at", n_trunc, ")")
  }
  s_collapsed <- paste(s2print, collapse = ", ")
  cat("strategies:", s_collapsed, msg, "\n")
  cat("number of simulations (n_sim):", x$n_sim, "\n")
  cat("cost: a data frame with", nrow(x$cost), "rows and", ncol(x$cost), "columns.", "\n")
  cat("effectiveness: a data frame with",
      nrow(x$effectiveness), "rows and",
      ncol(x$effectiveness), "columns.", "\n")
  cat("currency:", x$currency, "\n")
}
