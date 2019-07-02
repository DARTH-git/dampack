#' One-way sensitivity analysis using linear regression metamodeling
#'
#' This function uses a linear regression metamodel of a PSA for a given
#' decision-analytic model to predict the desired outcome.
#'
#' @param sens sensitivity analysis object;
#' either a probabilistic sensitivity analysis (\code{\link{make_psa_obj}}) or
#' a deterministic sensitivity analysis object (\code{\link{create_dsa_twoway}})
#' @param nsamps number of samples to take from the ranges
#' @inheritParams metamodel
#' @inheritParams predict.metamodel
#' @return A dataframe with the results of the sensitivity analysis.
#' Can be visualized with \code{\link{plot.owsa}, \link{owsa_tornado}, and \link{owsa_opt_strat}}
#'
#' @export
owsa <- function(sens, parms = NULL, ranges = NULL, nsamps = 100,
                 outcome = c("eff", "cost", "nhb", "nmb", "nhb_loss", "nmb_loss"),
                 wtp = NULL,
                 strategies = NULL,
                 poly.order = 2){
  outcome <- match.arg(outcome)
  if (inherits(sens, "psa")) {
    # create metamodel
    mm <- metamodel("oneway", sens, parms,
                    strategies, outcome, wtp, "poly", poly.order)

    # predict outcomes using predict.metamodel
    ow <- predict(mm, ranges, nsamps)
  } else if (inherits(sens, "dsa_oneway")) {
    params <- sens$parameters
    eff <- sens$effectiveness
    cost <- sens$cost
    strategies <- sens$strategies
    param_names <- sens$parnames

    # calculate outcome of interest
    y <- calculate_outcome(outcome, cost, eff, wtp)
    names(y) <- strategies

    # loop over dsa's and create ow
    ow <- NULL
    for (p in param_names) {
      for (s in strategies) {
        # maybe extract this out later - shared with predict.metamodel
        param_rows <- params$parameter == p
        param_val <- params[param_rows, "parmval"]
        outcome_val <- y[param_rows, s]

        new_df <- data.frame("parameter" = p,
                             "strategy" = s,
                             "param_val" = param_val,
                             "outcome_val" = outcome_val)
        ow <- rbind(ow, new_df, stringsAsFactors = FALSE)
      }
    }

  } else {
    stop("sens must have class 'psa' or 'dsa_oneway'")
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
#' @param facet_scales whether the x or y axes should be fixed. See \code{\link[ggplot2]{facet_grid}} in the \code{ggplo2} package for
#' more details.
#' @param facet_ncol number of columns in plot facet.
#' The default (NULL) is passed to \code{\link[ggplot2]{facet_wrap}},
#' which determines the number of rows and columns automatically.
#' @param facet_nrow number of rows in plot facet.
#' @inheritParams add_common_aes
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot.owsa <- function(x, txtsize = 12,
                      col = c("full", "bw"),
                      facet_scales = c("free_x", "free_y", "free", "fixed"),
                      facet_nrow = NULL,
                      facet_ncol = NULL,
                      size = 1,
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      ...) {
  scales <- match.arg(facet_scales)
  owsa <- ggplot(data = x,
                 aes_(x = as.name("param_val"), y = as.name("outcome_val"))) +
    facet_wrap(facets = "parameter", scales = scales, nrow = facet_nrow, ncol = facet_ncol) +
    ylab("E[Outcome]") +
    xlab("Parameter Values")

  col <- match.arg(col)
  if (col == "full") {
    owsa <- owsa +
      geom_line(aes_(color = as.name("strategy")),
                size = size)
  }
  if (col == "bw") {
    owsa <- owsa +
      geom_line(aes_(linetype = as.name("strategy")),
                size = size) +
      scale_linetype_discrete(name = "Strategy")
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
#' @param strategy the desired strategy
#' @inheritParams add_common_aes
#' @inheritParams owsa_opt_strat
#' @importFrom stats median reorder
#' @import ggplot2
#' @export
owsa_tornado <- function(owsa, strategy, return = c("plot", "data"),
                         txtsize = 12, min_rel_diff = 0,
                         col = c("full", "bw"),
                         n_y_ticks = 8, ylim = NULL, ybreaks = NULL){
  # check that is owsa object
  if (!is_owsa(owsa)) {
    stop("must provide an owsa object created with owsa()")
  }

  # range of min_rel_diff
  if (min_rel_diff < 0 | min_rel_diff > 1) {
    stop("min_rel_diff must be between 0 and 1")
  }
  # filter to strategy
  owsa_filt <- owsa[owsa$strategy == strategy, ]

  # group by parameter and strategy
  mins <- owsa_filt %>%
    group_by(.data$parameter) %>%
    filter(.data$param_val == min(.data$param_val))

  maxes <- owsa_filt %>%
    group_by(.data$parameter) %>%
    filter(.data$param_val == max(.data$param_val))

  avg <- median(owsa_filt$outcome_val)

  min_max <- inner_join(mins, maxes, by = c("parameter", "strategy"),
                        suffix = c(".low", ".high")) %>%
    mutate(abs_diff = abs(.data$outcome_val.high - .data$outcome_val.low),
           rel_diff = .data$abs_diff / .data$outcome_val.low) %>%
    filter(.data$rel_diff >= min_rel_diff) %>%
    arrange(-.data$abs_diff)

  # return either plot or data
  ret <- match.arg(return)
  if (ret == "plot") {
    g <- ggplot(min_max, aes_(x = reorder(min_max$parameter, min_max$abs_diff))) +
      geom_bar(aes_(y = as.name("outcome_val.low"), fill = "Low"),
               stat = "identity") +
      geom_bar(aes_(y = as.name("outcome_val.high"), fill = "High"),
               stat = "identity") +
      labs(x = "Parameter", y = "Outcome",
           title = paste0("Strategy: ", strategy)) +
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
    group_by(.data$parameter, .data$param_val) %>%
    filter(.data$outcome_val == obj_fun(.data$outcome_val)) %>%
    ungroup() %>%
    group_by(.data$parameter, .data$strategy) %>%
    summarize(pmin = min(.data$param_val), pmax = max(.data$param_val)) %>%
    ungroup()
  if (!plot_const) {
    opt_strat <- opt_strat %>%
      group_by(.data$parameter) %>%
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
    geom_rect(aes_(xmin = as.name("pmin"), xmax = as.name("pmax"),
                   ymin = 0, ymax = 1,
                   fill = as.name("strategy")),
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

# check that is owsa object
is_owsa <- function(obj) {
  if (inherits(obj, "owsa")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
