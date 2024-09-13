#' One-way sensitivity analysis
#'
#' When used on a PSA object, this function uses a polynomial regression metamodel to predict the
#' average outcome of a decision-analytic model as a function of a single input parameter.
#' When used on a DSA object, this function uses the DSA results directly to show how the selected outcome varies
#' as a function of the input parameter of interest. In the DSA context, this function is called
#' internally by \code{\link{run_owsa_det}} and should not be called by the user. In the PSA context,
#' the user must use this function to produce an \code{owsa} object.
#'
#' @param sa_obj sensitivity analysis object;
#' either a probabilistic sensitivity analysis (\code{\link{make_psa_obj}}) or
#' a deterministic sensitivity analysis object (\code{\link{run_owsa_det}})
#' @param nsamp number of samples to take from the ranges
#' @inheritParams metamodel
#' @inheritParams predict.metamodel
#' @return An object of class \code{data.frame} and \code{owsa} with the results of the sensitivity analysis.
#' Can be visualized with \code{\link{plot.owsa}, \link{owsa_tornado}, and \link{owsa_opt_strat}}
#'
#' @export
owsa <- function(sa_obj, params = NULL, ranges = NULL, nsamp = 100,
                 outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2) {
  outcome <- match.arg(outcome)
  if (inherits(sa_obj, "psa")) {
    # Use other_outcome if available
    if (!is.null(sa_obj$other_outcome)) {
      sa_obj$effectiveness <- sa_obj$other_outcome
    }
    # create metamodel
    mm <- metamodel("oneway", sa_obj, params,
                    strategies, outcome, wtp, "poly", poly.order)

    # predict outcomes using predict.metamodel
    ow <- predict(mm, ranges, nsamp)
  } else if (inherits(sa_obj, "dsa_oneway")) {
    params <- sa_obj$parameters
    if (!is.null(sa_obj$other_outcome)) {
      eff <- sa_obj$other_outcome
    } else {
      eff <- sa_obj$effectiveness
    }
    cost <- sa_obj$cost
    strategies <- sa_obj$strategies
    param_names <- sa_obj$parnames

    # calculate outcome of interest
    y <- calculate_outcome(outcome, cost, eff, wtp)
    names(y) <- strategies

    # loop over dsa's and create ow
    ow <- NULL
    for (p in param_names) {
      for (s in strategies) {
        # maybe extract this out later - shared with predict.metamodel
        param_rows <- params$parameter == p
        param_val <- params[param_rows, "paramval"]
        outcome_val <- y[param_rows, s]

        new_df <- data.frame("parameter" = p,
                             "strategy" = s,
                             "param_val" = param_val,
                             "outcome_val" = outcome_val)
        ow <- rbind(ow, new_df, stringsAsFactors = FALSE)
        # make strategies in owsa object into ordered factors
        ow$strategy <- factor(ow$strategy, levels = strategies, ordered = TRUE)
      }
    }

  } else {
    stop("sa_obj must have class 'psa' or 'dsa_oneway'")
  }

  # define classes
  class(ow) <- c("owsa", "data.frame")
  return(ow)
}

#' Plot a sensitivity analysis
#'
#' @param x an owsa object
#' @param txtsize base text size in the plot
#' @param col either full-color ("full") or black and white ("bw")
#' @param size either point size (ptype = "point") and/or line size (ptype = "line")
#' @param facet_scales whether the x or y axes should be fixed.
#' See \code{\link[ggplot2]{facet_grid}} in the \code{ggplo2} package for
#' more details.
#' @param facet_ncol number of columns in plot facet.
#' The default (NULL) is passed to \code{\link[ggplot2]{facet_wrap}},
#' which determines the number of rows and columns automatically.
#' @param facet_nrow number of rows in plot facet.
#' @param basecase named list of specific values for each parameter to highlight
#' on the returned plot. Each list element must have the same name as the corresponding
#' parameter in the \code{owsa} object.
#' @inheritParams add_common_aes
#'
#' @return A \code{ggplot2} plot of the \code{owsa} object.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @export
plot.owsa <- function(x, txtsize = 12,
                      col = c("full", "bw"),
                      facet_scales = c("free_x", "free_y", "free", "fixed"),
                      facet_nrow = NULL,
                      facet_ncol = NULL,
                      size = 1,
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      basecase = NULL,
                      ...) {
  param_val <- outcome_val <- strategy <- NULL
  scales <- match.arg(facet_scales)
  owsa <- ggplot(data = x,
                 aes(x = param_val, y = outcome_val)) +
    facet_wrap(facets = "parameter", scales = scales, nrow = facet_nrow, ncol = facet_ncol) +
    ylab("E[Outcome]") +
    xlab("Parameter Values")

  col <- match.arg(col)
  if (col == "full") {
    owsa <- owsa +
      geom_line(aes(color = strategy),
                linewidth = size)
  }
  if (col == "bw") {
    owsa <- owsa +
      geom_line(aes_(linetype = strategy),
                size = size) +
      scale_linetype_discrete(name = "Strategy")
  }

  if (!is.null(basecase)) {
    # create data.frame for "basecase" values
    basecase_df <- as.data.frame(basecase) %>%
      pivot_longer(cols = everything(),
                   names_to = "parameter",
                   values_to = "param_val")

    if (!all(basecase_df$parameter %in% unique(x$parameter))) {
      stop("Some parameter names in the basecase argument of plot.owsa are not present in owsa object.")
    }

    owsa <- owsa +
      geom_vline(mapping = aes(xintercept = param_val),
                 data = basecase_df)
  }

  add_common_aes(owsa, txtsize, col = col, col_aes = "color",
                 scale_name = "Strategy",
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 continuous = c("x", "y"))
}

#' Tornado plot of a one-way sensitivity analysis
#'
#' @param owsa an owsa object
#' @param min_rel_diff this function only plots
#' parameters that lead to a relative change in the outcome greater than or equal
#' to \code{min_rel_diff}, which must be between 0 and 1. The default (0) is that
#' no strategies are filtered.
#' @inheritParams add_common_aes
#' @inheritParams owsa_opt_strat
#' @return If \code{return == "plot"}, a \code{ggplot2} tornado plot derived from the \code{owsa}
#' object, or if \code{return == "data"}, a \code{data.frame} containing all data contained in the plot.
#' A tornado plot is a visual aid used to identify which parameters are driving most of the variation
#' in a specified model outcome.
#' @importFrom stats median reorder
#' @import ggplot2
#' @export
owsa_tornado <- function(owsa, return = c("plot", "data"),
                         txtsize = 12, min_rel_diff = 0,
                         col = c("full", "bw"),
                         n_y_ticks = 8, ylim = NULL, ybreaks = NULL) {
  parameter <- param_val <- outcome_val <- strategy <- outcome_val.low <-
    outcome_val.high <- abs_diff <- rel_diff <- NULL
  # check that is owsa object
  if (!is_owsa(owsa)) {
    stop("must provide an owsa object created with owsa()")
  }

  # range of min_rel_diff
  if (min_rel_diff < 0 || min_rel_diff > 1) {
    stop("min_rel_diff must be between 0 and 1")
  }

  owsa_filt <- owsa %>%
    group_by(parameter, param_val) %>%
    arrange(outcome_val) %>%
    slice(n()) %>%
    select(-strategy) %>%
    ungroup()

  # group by parameter and strategy
  mins <- owsa_filt %>%
    group_by(parameter) %>%
    filter(param_val == min(param_val))

  maxes <- owsa_filt %>%
    group_by(parameter) %>%
    filter(param_val == max(param_val))

  avg <- median(owsa_filt$outcome_val)

  min_max <- inner_join(mins, maxes, by = c("parameter"),
                        suffix = c(".low", ".high")) %>%
    mutate(abs_diff = abs(outcome_val.high - outcome_val.low),
           rel_diff = abs_diff / outcome_val.low) %>%
    filter(rel_diff >= min_rel_diff) %>%
    arrange(-abs_diff)

  # return either plot or data
  ret <- match.arg(return)
  if (ret == "plot") {
    g <- ggplot(min_max, aes(x = reorder(min_max$parameter, min_max$abs_diff))) +
      geom_bar(aes(y = outcome_val.low, fill = "Low"),
               stat = "identity") +
      geom_bar(aes(y = outcome_val.high, fill = "High"),
               stat = "identity") +
      labs(x = "Parameter", y = "Outcome") +
      coord_flip()

    col <- match.arg(col)
    g <- add_common_aes(g, txtsize, col = col, col_aes = "fill",
                        scale_name = "Parameter\nLevel",
                        continuous = "y",
                        ytrans = offset_trans(offset = avg),
                        n_y_ticks = n_y_ticks,
                        ybreaks = ybreaks,
                        ylim = ylim) +
      geom_hline(yintercept = avg, linetype = 3)

    return(g)
  } else {
    return(min_max)
  }
}

#' transformation for owsa_tornado
#' @param offset the offset for the transformation (kind of the new 0)
#' @return offset value supplied to ytrans in \code{add_common_aes}
#' @keywords internal
#' @importFrom scales trans_new
offset_trans <- function(offset = 0) {
  trans_new(paste0("offset-", format(offset)),
            function(x) x - offset,
            function(x) x + offset)
}

#' plot the optimal strategy as the parameter values change
#'
#' @param owsa An owsa object
#' @param params vector of parameters to plot
#' @param maximize whether to maximize (TRUE) or minimize the outcome
#' @param return either return a ggplot object \code{plot} or a data frame with
#' ranges of parameters for which each strategy is optimal.
#' @param plot_const whether to plot parameters that don't lead to
#' changes in optimal strategy as they vary.
#' @inheritParams add_common_aes
#' @inheritParams plot.owsa
#' @param facet_ncol Number of columns in plot facet.
#' @return If \code{return == "plot"}, a \code{ggplot2} optimal strategy plot derived from the \code{owsa}
#' object, or if \code{return == "data"}, a \code{data.frame} containing all data contained in the plot.
#' The plot allows us to see how the strategy that maximizes the expectation of the outcome of interest
#' changes as a function of each parameter of interest.
#' @import ggplot2
#' @export
owsa_opt_strat <- function(owsa, params = NULL, maximize = TRUE,
                           return = c("plot", "data"),
                           plot_const = TRUE,
                           col = c("full", "bw"),
                           greystart = 0.2,
                           greyend = 0.8,
                           txtsize = 12,
                           facet_ncol = 1,
                           facet_nrow = NULL,
                           facet_lab_txtsize = NULL,
                           n_x_ticks = 10) {
  # check that is owsa object
  if (!is_owsa(owsa)) {
    stop("must provide an owsa object created with owsa()")
  }

  parameter <- param_val <- outcome_val <- strategy <- NULL
  # get optimal strategy
  ## either minimum or maximum
  if (maximize) {
    obj_fun <- max
  } else {
    obj_fun <- min
  }

  ## filter to those parameter values
  ## that maximize the outcome for each strategy
  opt_strat <- owsa %>%
    group_by(parameter, param_val) %>%
    filter(outcome_val == obj_fun(outcome_val)) %>%
    ungroup() %>%
    group_by(parameter, strategy) %>%
    summarize(pmin = min(param_val), pmax = max(param_val)) %>%
    ungroup()
  if (!plot_const) {
    opt_strat <- opt_strat %>%
      group_by(parameter) %>%
      filter(n() > 1) %>%
      ungroup()
  }
  opt_strat$strategy <- as.factor(opt_strat$strategy)

  # filter to parameters input by user and maintain their ordering
  # if no parameters are supplied, initial ordering from owsa will be used
  if (!is.null(params)) {
    #check that params supplied are a subset of parameters from owsa
    if (!all(params %in% opt_strat$parameter)) {
      stop("must provide valid parameters")
    }
    opt_strat <- opt_strat[opt_strat$parameter %in% params, ]
    opt_strat$parameter <- factor(opt_strat$parameter, levels = params)
  } else {
    opt_strat$parameter <- factor(opt_strat$parameter, levels = unique(owsa$parameter))
  }

  g <- ggplot(opt_strat) +
    facet_wrap("parameter", scales = "free_x", ncol = facet_ncol, nrow = facet_nrow) +
    # a little bit hacky: rectangles with height 1
    geom_rect(aes(xmin = pmin, xmax = pmax,
                  ymin = 0, ymax = 1,
                  fill = strategy),
              position = "identity") +
    facet_wrap(facets = "parameter", scales = "free_x", nrow = facet_nrow, ncol = facet_ncol)

  col <- match.arg(col)
  g <- add_common_aes(g, txtsize, scale_name = "Optimal Strategy: ",
                      col, col_aes = "fill", continuous = "x",
                      facet_lab_txtsize = facet_lab_txtsize,
                      n_x_ticks = n_x_ticks) +
    # these remove the meaningless y axis labels and text
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank())

  # return either plot or data
  ret <- match.arg(return)
  if (ret == "plot") {
    return(g)
  } else {
    return(opt_strat)
  }
}


#' check that object is owsa object
#' @param obj the object to be checked for the owsa class
#' @return returns TRUE if object is "owsa" object and FALSE if not.
#' @keywords internal
is_owsa <- function(obj) {
  if (inherits(obj, "owsa")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
