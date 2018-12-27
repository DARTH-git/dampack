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

  df <- data.frame("Strategy" = strategies, "Cost" = cost, "Effect" = effect)
  coln <- colnames(df)
  nstrat <- nrow(df)
  # remove dominated strategies
  df <- df %>%
    arrange(.data$Cost, desc(.data$Effect))
  dominated <- df[FALSE, ]
  for (i in 1:(nstrat - 1)) {
    for (j in (i + 1):nstrat) {
      if (df[j, "Effect"] <= df[i, "Effect"]) {
        dominated <- union(dominated, df[j, ])
      }
    }
  }
  non_dominated <- setdiff(df, dominated)

  # remove extendedly dominated strategies
  # extended dominance means ICER > that of both previous and next strategies in order of cost
  ext_dominated <- non_dominated[FALSE, ]
  if (nrow(non_dominated) > 3) {
    ext_dominated_count <- nrow(ext_dominated)
    prev_ext_dominated_count <- -1 # run atleast once
    while (ext_dominated_count > prev_ext_dominated_count &
           ( (nrow(non_dominated) - ext_dominated_count) > 3) ) {
      non_dominated_set <- compute_icers(setdiff(non_dominated, ext_dominated))
      prev_ext_dominated_count <- ext_dominated_count
      for (i in 3:(nrow(non_dominated_set) - 1)) {
        if (non_dominated_set[i, "ICER"] > non_dominated_set[i - 1, "ICER"] &
            non_dominated_set[i, "ICER"] > non_dominated_set[i + 1, "ICER"]){
          ext_dominated <- union(ext_dominated, non_dominated_set[i, coln])
        }
      }
      ext_dominated_count <- nrow(ext_dominated)
    }
  }

  # calculate ICERs for those strategies not dominated
  # nor etendedly dominated
  non_ext_dominated <- setdiff(non_dominated, ext_dominated) %>%
    compute_icers() %>%
    mutate(Status = "ND")

  dominated <- dominated %>%
    mutate(ICER = NA, Status = "D")

  ext_dominated <- ext_dominated %>%
    mutate(ICER = NA, Status = "ED")

  # recombine all results to produce final output
  results <- bind_rows(non_ext_dominated, dominated, ext_dominated)

  # declare class of results
  class(results) <- c("icers", "data.frame")
  return(results)
}


# Source: https://miqdad.freeasinspeech.org.uk/icer_calculator/
compute_icers <- function(non_dominated) {
  icers <- non_dominated %>%
    arrange(.data$Cost, desc(.data$Effect))

  if (nrow(non_dominated) > 1) {
    icers[1, "ICER"] <- NA
    for (i in 2:nrow(non_dominated)) {
      icers[i, "ICER"] <- (icers[i, "Cost"] - icers[i - 1, "Cost"]) /
        (icers[i, "Effect"] - icers[i - 1, "Effect"])
    }
  }
  return(icers)
}
