#' function to calculate ICERS
#'
#' Adapted from https://miqdad.freeasinspeech.org.uk/icer_calculator/
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
                   stringsAsFactors = FALSE)
  nstrat <- nrow(df)

  # three statuses: dominated, extended dominated, and non-dominated
  d <- NULL

  # detect dominated strategies
  # dominated strategies have a higher cost and lower effect
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df[j, "Effect"]
      if (jth_effect <= ith_effect) {
        # append dominated strategies to vector
        d <- c(d, df[j, "Strategy"])
      }
    }
  }

  # detect weakly dominated strategies (extended dominance)
  # this needs to be repeated until there are no more ED strategies
  ed <- vector()
  continue <- TRUE
  while (continue) {
    # vector of all dominated strategies (strong or weak)
    dom <- union(d, ed)

    # strategies declared to be non-dominated at this point
    nd <- setdiff(strategies, dom)

    # compute icers for nd strategies
    nd_df <- df[df$Strategy %in% nd, ] %>%
      compute_icers()

    # number non-d
    n_non_d <- nrow(nd_df)

    # strategy identifiers for non-d
    nd_strat <- nd_df$Strategy

    # now, go through non-d strategies and detect any
    # with higher ICER than following strategy
    ## keep track of whether any ED strategies are picked up
    # if not, we're done - exit the loop
    new_ed <- 0
    for (i in 2:(n_non_d - 1)) {
      if (nd_df[i, "ICER"] > nd_df[i + 1, "ICER"]) {
        ed <- c(ed, nd_strat[i])
        new_ed <- new_ed + 1
      }
    }
    if (new_ed == 0) {
      continue <- FALSE
    }
  }

  # recompute icers without weakly dominated strategies
  nd_df_icers <- nd_df[!(nd_df$Strategy %in% dom), ] %>%
    compute_icers() %>%
    mutate(Status = "ND")

  # dominated and weakly dominated
  d_df <- df[df$Strategy %in% d, ] %>%
    mutate(Status = "D", ICER = NA)

  ed_df <- df[df$Strategy %in% ed, ] %>%
    mutate(Status = "ED", ICER = NA)

  # when combining, sort so we have ref,ND,ED,D
  results <- bind_rows(d_df, ed_df, nd_df_icers) %>%
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
