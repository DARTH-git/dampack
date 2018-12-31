# todo: clean up, make compatible with PSA

#' function to calculate ICERS
#'
#' @param cost vector of cost for each strategy
#' @param effect vector of effect for each strategy
#' @param strategies character vector of strategy names
#'
#' @export
calculate_icers <- function(cost, effect, strategies) {
  # todo: check data is in correct format

  df <- data.frame("Strategy" = strategies,
                   "Cost" = cost,
                   "Effect" = effect,
                   "Status" = "ND",
                   stringsAsFactors = FALSE)
  coln <- colnames(df)
  nstrat <- nrow(df)
  # remove dominated strategies
  # dominated strategies have a higher cost and lower effect
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df[j, "Effect"]
      if (jth_effect <= ith_effect) {
        df[j, "Status"] <- "D"
      }
    }
  }

  # detect weakly dominated strategies (extended dominance)
  non_d <- df[df$Status != "D", ] %>%
    compute_icers()
  n_non_d <- nrow(non_d)
  for (i in 2:(n_non_d - 1)) {
    if (non_d[i, "ICER"] > non_d[i + 1, "ICER"]) {
      non_d[i, "Status"] <- "ED"
      non_d[i, "ICER"] <- NA
    }
  }
  # recompute icers without weakly dominated strategies
  non_ed <- non_d[non_d$Status != "ED", ] %>%
    compute_icers()

  # recombine all results to produce final output
  d <- df[df$Status == "D", ]
  ed <- non_d[non_d$Status == "ED", ]

  # when combining, sort so we have ref,ND,ED,D
  results <- bind_rows(d, ed, non_ed) %>%
    arrange(desc(.data$Status), .data$Cost, desc(.data$Effect))

  # declare status for first entry to be 'ref'
  results[1, "Status"] <- "ref"

  # declare class of results
  class(results) <- c("icers", "data.frame")
  return(results)
}


# Source: https://miqdad.freeasinspeech.org.uk/icer_calculator/
compute_icers <- function(non_d) {
  if (nrow(non_d) > 1) {
    non_d[1, "ICER"] <- NA
    for (i in 2:nrow(non_d)) {
      non_d[i, "ICER"] <- (non_d[i, "Cost"] - non_d[i - 1, "Cost"]) /
        (non_d[i, "Effect"] - non_d[i - 1, "Effect"])
    }
  }
  return(non_d)
}
