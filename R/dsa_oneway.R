#' create deterministic sensitivity analysis object
#'
#' @param params parameter values associated with
#' @export
create_dsa_oneway <- function(params, effectiveness, strategies,
                              cost = NULL, currency = "$") {
  # todo: checks on input

  if (inherits(params, "list")) {
    n_comps_p <- length(params)
    if (is.null(names(params))) {
      stop("if a list is provided, it must have names")
    }
  } else {
    n_comps_p <- 1
    params <- list("param" = params)
  }
  pnames <- names(params)

  if (inherits(effectiveness, "list")) {
    n_comps_e <- length(effectiveness)
    names(effectiveness) <- pnames
  } else {
    n_comps_e <- 1
    effectiveness <- list("p" = effectiveness)
    names(effectiveness) <- pnames
  }

  if (inherits(cost, "list")) {
    n_comps_c <- length(cost)
  } else {
    n_comps_c <- 1
    cost <- list("p" = cost)
    names(cost) <- pnames
  }

  # change column names for effect and cost
  for (i in 1:n_comps_p) {
    effectiveness[[i]] <- check_df_and_coerce(effectiveness[[i]])
    names(effectiveness[[i]]) <- strategies
    cost[[i]] <- check_df_and_coerce(cost[[i]])
    names(cost[[i]]) <- strategies
  }

  dsa <- list(
    params = params,
    effectiveness = effectiveness,
    strategies = strategies,
    cost = cost,
    currency = currency,
    n_dsa = n_comps_p
  )
  class(dsa) <- "dsa_oneway"
  dsa
}
