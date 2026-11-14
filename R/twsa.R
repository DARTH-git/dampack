#' Two-way sensitivity analysis using linear regression metamodeling
#'
#' This function displays a two-way sensitivity analysis (TWSA) graph
#' by estimating a linear regression metamodel of a PSA for a given
#' decision-analytic model
#'
#' @param sa_obj sensitivity analysis object;
#' either a probabilistic sensitivity analysis (\code{\link{make_psa_obj}}) or
#' a two-way deterministic sensitivity analysis object (\code{\link{create_dsa_twoway}})
#'
#' @param param1 String with the name of the first parameter of interest
#' @param param2 String with the name of the second parameter of interest

#' @inheritParams metamodel
#' @inheritParams predict.metamodel
#'
#' @return A \code{data.frame} of class \code{twsa} with the outcome of each strategy over a grid of
#' values of \code{param1} and \code{param2}. It can be visualized with \code{\link{plot.twsa}}.
#'
#' @seealso \code{\link{plot.twsa}}, \code{\link{run_twsa_det}}
#'
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#'
#' # strategy with the highest predicted net monetary benefit as two parameters vary
#' df_twsa <- twsa(l_psa, param1 = "pFailChemo", param2 = "muDieCancer",
#'                 outcome = "nmb", wtp = 1e5)
#' head(df_twsa)
#' plot(df_twsa)
#'
#' @export
twsa <- function(sa_obj, param1 = NULL, param2 = NULL, ranges = NULL,
                 nsamp = 100,
                 outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2) {
  outcome <- match.arg(outcome)
  if (inherits(sa_obj, "psa")) {
    if (is.null(param1) || is.null(param2)) {
      stop("if using psa object, both param1 and param2 must be provided")
    }

    params <- c(param1, param2)

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
      df_eff <- sa_obj$other_outcome
    } else {
      df_eff <- sa_obj$effectiveness
    }
    cost <- sa_obj$cost
    strategies <- sa_obj$strategies
    v_parnames <- sa_obj$parnames

    # calculate outcomes
    # calculate outcome of interest
    df_y <- calculate_outcome(outcome, cost, df_eff, wtp)
    names(df_y) <- strategies

    # loop over dsa's and create ow
    tw <- NULL
    for (s in strategies) {
      # maybe extract this out later - shared with predict.metamodel
      df_new <- data.frame("p1" = params[, v_parnames[1]], "p2" = params[, v_parnames[2]],
                           "strategy" = s, "outcome_val" = df_y[, s])
      tw <- rbind(tw, df_new, stringsAsFactors = FALSE)
    }
    names(tw)[1:2] <- v_parnames
    # make strategies in twsa object into ordered factors
    tw$strategy <- factor(tw$strategy, levels = strategies, ordered = TRUE)
  } else {
    stop("either a psa or dsa_twoway object must be provided")
  }

  # define classes
  class(tw) <- c("twsa", "data.frame")
  tw
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
#' @examples
#' data("cancer_psa_data")
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' df_twsa <- twsa(l_psa, param1 = "pFailChemo", param2 = "muDieCancer",
#'                 outcome = "nmb", wtp = 1e5)
#' plot(df_twsa)
#'
#' # mark a base case (here, close to the PSA means), in black and white
#' plot(df_twsa, basecase = list(pFailChemo = 0.45, muDieCancer = 0.2), col = "bw")
#'
#' # the strategy with the lowest expected cost
#' df_twsa_cost <- twsa(l_psa, param1 = "pFailChemo", param2 = "muDieCancer", outcome = "cost")
#' plot(df_twsa_cost, maximize = FALSE)
#' @export
plot.twsa <- function(x, maximize = TRUE,
                      col = c("access", "full", "bw"),
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      txtsize = 12,
                      basecase = NULL,
                      ...) {

  outcome_val <- strategy <- NULL
  # parameter names
  v_params <- names(x)[c(1, 2)]
  param1 <- v_params[1]
  param2 <- v_params[2]

  # get optimal strategy
  # thanks to
  # https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
  if (maximize) {
    obj_fn <- which.max
  } else {
    obj_fn <- which.min
  }
  df_opt <- x %>%
    group_by(.data[[param1]], .data[[param2]]) %>%
    slice(obj_fn(outcome_val))
  g <- ggplot(df_opt, aes(x = !!sym(param1), y = !!sym(param2))) +
    geom_tile(aes(fill = strategy)) +
    theme_bw() +
    xlab(param1) +
    ylab(param2)

  if (!is.null(basecase)) {
    if (!all(names(basecase) %in% names(x)[1:2])) {
      stop("Some parameter names in the basecase argument do not match param1 or param2 of twsa")
    }
    # create data.frame for "basecase" values
    df_basecase <- as.data.frame(basecase)

    g <- g +
      geom_point(mapping = aes(x = !!sym(param1), y = !!sym(param2)),
                 data = df_basecase,
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
