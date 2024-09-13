#' Two-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a two-way sensitivity analysis (TWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#'
#' @param sa_obj sensitivity analysis object;
#' either a probabilistic sensitivity analysis (\code{\link{make_psa_obj}}) or
#' a deterministic sensitivity analysis object (\code{\link{run_owsa_det}})
#'
#' @param param1 String with the name of the first parameter of interest
#' @param param2 String with the name of the second parameter of interest

#' @inheritParams metamodel
#' @inheritParams predict.metamodel
#'
#' @return twsa A \code{ggplot2} object with the TWSA graph of \code{param1} and
#' \code{param2} on the outcome of interest.
#'
#' @export
twsa <- function(sa_obj, param1 = NULL, param2 = NULL, ranges = NULL,
                 nsamp = 100,
                 outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2) {
  if (inherits(sa_obj, "psa")) {
    if (is.null(param1) || is.null(param2)) {
      stop("if using psa object, both param1 and param2 must be provided")
    }

    params <- c(param1, param2)

    outcome <- match.arg(outcome)

    if (!is.null(sa_obj$other_outcome)) {
      sa_obj$effectiveness <- sa_obj$other_outcome
    }

    # run metamodel
    mm <- metamodel("twoway", sa_obj, params, strategies, outcome, wtp, "poly", poly.order)
    # predict outcomes
    tw <- predict(mm, ranges, nsamp)
  } else if (inherits(sa_obj, "dsa_twoway")) {
    params <- sa_obj$parameters
    if (!is.null(sa_obj$other_outcome)) {
      eff <- sa_obj$other_outcome
    } else {
      eff <- sa_obj$effectiveness
    }
    cost <- sa_obj$cost
    strategies <- sa_obj$strategies
    parnames <- sa_obj$parnames

    # calculate outcomes
    # calculate outcome of interest
    y <- calculate_outcome(outcome, cost, eff, wtp)
    names(y) <- strategies

    # loop over dsa's and create ow
    tw <- NULL
    for (s in strategies) {
      # maybe extract this out later - shared with predict.metamodel
      new_df <- data.frame("p1" = params[, parnames[1]], "p2" = params[, parnames[2]],
                           "strategy" = s, "outcome_val" = y[, s])
      tw <- rbind(tw, new_df, stringsAsFactors = FALSE)
    }
    names(tw)[1:2] <- parnames
    # make strategies in twsa object into ordered factors
    tw$strategy <- factor(tw$strategy, levels = strategies, ordered = TRUE)
  } else {
    stop("either a psa or dsa_twoway object must be provided")
  }

  # define classes
  class(tw) <- c("twsa", "data.frame")
  return(tw)
}

#' Two-way sensitivity analysis plot
#'
#' @param x a twsa object
#' @inheritParams add_common_aes
#' @param maximize If \code{TRUE}, plot of strategy with maximum expected outcome
#' (default); if \code{FALSE}, plot of strategy with minimum expected outcome
#' @param basecase named list of specific combination of param1 and param2 values to highlight
#' on the returned plot. Each list element must have the same name as the corresponding
#' parameter in the \code{owsa} object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang !!
#' @importFrom rlang sym
#' @return A \code{ggplot2} plot of the two-way sensitivity analysis.
#' @export
plot.twsa <- function(x, maximize = TRUE,
                      col = c("full", "bw"),
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      txtsize = 12,
                      basecase = NULL,
                      ...) {

  outcome_val <- strategy <- NULL
  # parameter names
  params <- names(x)[c(1, 2)]
  param1 <- params[1]
  param2 <- params[2]

  # get optimal strategy
  # thanks to
  # https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
  if (maximize) {
    obj_fn <- which.max
  } else {
    obj_fn <- which.min
  }
  opt_df <- x %>%
    group_by(.data[[param1]], .data[[param2]]) %>%
    slice(obj_fn(outcome_val))
  g <- ggplot(opt_df, aes(x = !!sym(param1), y = !!sym(param2))) +
    geom_tile(aes(fill = strategy)) +
    theme_bw() +
    xlab(param1) +
    ylab(param2)

  if (!is.null(basecase)) {
    if (!all(names(basecase) %in% names(x)[1:2])) {
      stop("Some parameter names in the basecase argument do not match param1 or param2 of twsa")
    }
    # create data.frame for "basecase" values
    basecase_df <- as.data.frame(basecase)

    g <- g +
      geom_point(mapping = aes(x = !!sym(param1), y = !!sym(param2)),
                 data = basecase_df,
                 shape = 8)
  }

  col <- match.arg(col)
  add_common_aes(g, txtsize, col = col, col_aes = "fill",
                 scale_name = "Strategy",
                 continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks,
                 n_y_ticks = n_y_ticks,
                 xexpand = c(0, 0),
                 yexpand = c(0, 0))
}
