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
      df_eff <- sa_obj$other_outcome
    } else {
      df_eff <- sa_obj$effectiveness
    }
    df_cost <- sa_obj$cost
    strategies <- sa_obj$strategies
    v_param_names <- sa_obj$parnames

    # calculate outcome of interest
    df_y <- calculate_outcome(outcome, df_cost, df_eff, wtp)
    names(df_y) <- strategies

    # loop over dsa's and create ow
    ow <- NULL
    for (p in v_param_names) {
      for (s in strategies) {
        # maybe extract this out later - shared with predict.metamodel
        v_param_rows <- params$parameter == p
        v_param_val <- params[v_param_rows, "paramval"]
        v_outcome_val <- df_y[v_param_rows, s]

        df_new <- data.frame("parameter" = p,
                             "strategy" = s,
                             "param_val" = v_param_val,
                             "outcome_val" = v_outcome_val)
        ow <- rbind(ow, df_new, stringsAsFactors = FALSE)
        # make strategies in owsa object into ordered factors
        ow$strategy <- factor(ow$strategy, levels = strategies, ordered = TRUE)
      }
    }

  } else {
    stop("sa_obj must have class 'psa' or 'dsa_oneway'")
  }

  # define classes
  class(ow) <- c("owsa", "data.frame")
  ow
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
                      col = c("access", "full", "bw"),
                      facet_scales = c("free_x", "free_y", "free", "fixed"),
                      facet_nrow = NULL,
                      facet_ncol = NULL,
                      size = 1,
                      n_x_ticks = 6,
                      n_y_ticks = 6,
                      basecase = NULL,
                      ...) {
  param_val <- outcome_val <- strategy <- NULL
  v_strategies <- unique(x$strategy)
  scales <- match.arg(facet_scales)
  owsa <- ggplot(data = x,
                 aes(x = param_val, y = outcome_val)) +
    facet_wrap(facets = "parameter", scales = scales, nrow = facet_nrow, ncol = facet_ncol) +
    ylab("E[Outcome]") +
    xlab("Parameter Values")

  col <- match.arg(col)
  if (col == "full" || col == "access") {
    owsa <- owsa +
      geom_line(aes(color = strategy),
                linewidth = size)
  }
  if (col == "bw") {
    owsa <- owsa +
      geom_line(aes(linetype = strategy),
                linewidth = size) +
      scale_linetype_discrete(name = "Strategy")
  }

  if (!is.null(basecase)) {
    # create data.frame for "basecase" values
    df_basecase <- as.data.frame(basecase) %>%
      pivot_longer(cols = everything(),
                   names_to = "parameter",
                   values_to = "param_val")

    if (!all(df_basecase$parameter %in% unique(x$parameter))) {
      stop("Some parameter names in the basecase argument of plot.owsa are not present in owsa object.")
    }

    owsa <- owsa +
      geom_vline(mapping = aes(xintercept = param_val),
                 data = df_basecase)
  }

  # in black and white, strategies are told apart by line type, so no color scale is added
  add_common_aes(owsa, txtsize, col = if (col == "bw") "none" else col, col_aes = "color",
                 scale_name = "Strategy",
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 continuous = c("x", "y"),
                 v_str = v_strategies)
}

#' Tornado plot of a one-way sensitivity analysis
#'
#' @param owsa an owsa object
#' @param return choose whether to return the plot, underlying data, or both
#' @param txtsize text size used for the plot; input for add_common_aes function
#' @param min_rel_diff this function only plots
#' parameters that lead to a relative change in the outcome greater than or equal
#' to \code{min_rel_diff}, which must be between 0 and 1. The default (0) is that
#' no strategies are filtered.
#' @param col color used for the plot; input for add_common_aes function
#' @param n_y_ticks number of y-axis ticks; input for add_common_aes function
#' @param ylim vector of y-axis limits, or NULL, which sets limits
#' automatically; input for add_common_aes function
#' @param ybreaks vector of y-axis breaks, will override \code{n_y_ticks} if
#' provided; input for add_common_aes function
#' @param select_str vector of strategy names to examine (max of 2). If none are
#' input, it will default to choose only the first of the strategies in the owsa
#' object.
#' @param outcome_name The name of the outcome to show by strategy, either IMB,
#' NMB, or ICER
#' @return If \code{return == "plot"}, a \code{ggplot2} tornado plot derived from the \code{owsa}
#' object, or if \code{return == "data"}, a \code{data.frame} containing all data contained in the plot.
#' A tornado plot is a visual aid used to identify which parameters are driving most of the variation
#' in a specified model outcome.
#' @importFrom stats median reorder
#' @import ggplot2
#' @export
owsa_tornado <- function(owsa, return = c("plot", "data"),
                         txtsize = 12, min_rel_diff = 0,
                         col = c("access", "full", "bw"),
                         n_y_ticks = 8, ylim = NULL, ybreaks = NULL,
                         select_str = NULL, outcome_name = NULL) {

  if (length(select_str) > 2) {
    stop("Please select a max of 2 strategies")
  }

  if (is.null(outcome_name)) {
    outcome_name <- "Outcome"
  }

  if (is.null(select_str)) {
    select_str <- owsa$strategy[1]
    owsa       <- owsa %>%
      dplyr::filter(strategy == select_str[1])
    y_label    <- paste0(outcome_name, " (", select_str[1], ")")
    avg        <- median(owsa$outcome_val)  # MK: Replacement for res_out

  } else {
    if (length(select_str) == 2) {
      df_owsa_base <- owsa %>%
        dplyr::filter(strategy == select_str[1])
      df_owsa_comp <- owsa %>%
        dplyr::filter(strategy == select_str[2])
      df_owsa_join <- dplyr::inner_join(df_owsa_base, df_owsa_comp,
                                        by = c("parameter", "param_val"))
      df_owsa_join$outcome_val <- df_owsa_join$outcome_val.y - df_owsa_join$outcome_val.x
      df_owsa_join$strategy    <- df_owsa_join$strategy.x
      owsa                  <- df_owsa_join %>%
        dplyr::select(parameter, strategy, param_val, outcome_val)
      y_label <- paste0(outcome_name, " (", select_str[2],
                        " - ", select_str[1], ")")
      # MK: Replacement for res_out
      avg <- median(df_owsa_comp$outcome_val) - median(df_owsa_base$outcome_val)

    } else {
      owsa       <- owsa %>%
        dplyr::filter(strategy == select_str)
      y_label    <- paste0(outcome_name, " (", select_str, ")")
      avg        <- median(owsa$outcome_val)  # MK: Replacement for res_out
    }
  }

  # MK: Removed rel_diff
  parameter <- param_val <- outcome_val <- strategy <- outcome_val.low <-
    outcome_val.high <- abs_diff <- NULL

  # check that is owsa object
  if (!is_owsa(owsa)) {
    stop("must provide an owsa object created with owsa()")
  }

  # range of min_rel_diff
  if (min_rel_diff < 0 || min_rel_diff > 1) {
    stop("min_rel_diff must be between 0 and 1")
  }

  df_owsa_filt <- owsa %>%
    dplyr::group_by(parameter, param_val) %>%
    dplyr::arrange(outcome_val) %>%
    dplyr::slice(n()) %>%
    dplyr::select(-strategy) %>%
    dplyr::ungroup()

  # group by parameter and strategy
  df_mins <- df_owsa_filt %>%
    dplyr::group_by(parameter) %>%
    dplyr::filter(param_val == min(param_val))

  df_maxes <- df_owsa_filt %>%
    dplyr::group_by(parameter) %>%
    dplyr::filter(param_val == max(param_val))

  df_min_max <- dplyr::inner_join(df_mins, df_maxes, by = c("parameter"),
                                  suffix = c(".low", ".high")) %>%
    dplyr::mutate(abs_diff = abs(outcome_val.high - outcome_val.low),
                  rel_diff = abs_diff / outcome_val.low) %>%
    dplyr::filter(abs(.data$rel_diff) >= min_rel_diff) %>%
    dplyr::arrange(-abs_diff)

  # return either plot or data
  ret <- match.arg(return)
  if (ret == "plot") {
    g <- ggplot2::ggplot(df_min_max, aes(x = reorder(df_min_max$parameter,
                                                     df_min_max$abs_diff))) +
      ggplot2::geom_bar(aes(y = outcome_val.low, fill = "Low"),
                        stat = "identity") +
      ggplot2::geom_bar(aes(y = outcome_val.high, fill = "High"),
                        stat = "identity") +
      ggplot2::labs(x = "Parameter", y = y_label) +
      ggplot2::coord_flip()

    col <- match.arg(col)
    g <- add_common_aes(g, txtsize, col = col, col_aes = "fill",
                        scale_name = "Parameter\nLevel",
                        continuous = "y",
                        ytrans = offset_trans(offset = avg),
                        n_y_ticks = n_y_ticks,
                        ybreaks = ybreaks,
                        ylim = ylim) +
      ggplot2::geom_hline(yintercept = avg, linetype = 3)

    g
  } else {
    df_min_max
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
                           col = c("access", "full", "bw"),
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

  parameter <- param_val <- outcome_val <- strategy <- xmin <- xmax <- NULL
  # get optimal strategy
  ## either minimum or maximum
  if (maximize) {
    obj_fun <- max
  } else {
    obj_fun <- min
  }

  ## filter to those parameter values
  ## that maximize the outcome for each strategy
  df_opt_strat <- owsa %>%
    group_by(parameter, param_val, .groups = "drop_last") %>%
    filter(outcome_val == obj_fun(outcome_val)) %>%
    ungroup() %>%
    group_by(parameter, strategy, .groups = "drop_last") %>%
    summarize(pmin = min(param_val), pmax = max(param_val)) %>%
    ungroup()
  if (!plot_const) {
    df_opt_strat <- df_opt_strat %>%
      group_by(parameter, .groups = "drop_last") %>%
      filter(n() > 1) %>%
      ungroup()
  }
  df_opt_strat$strategy <- as.factor(df_opt_strat$strategy)

  # filter to parameters input by user and maintain their ordering
  # if no parameters are supplied, initial ordering from owsa will be used
  if (!is.null(params)) {
    #check that params supplied are a subset of parameters from owsa
    if (!all(params %in% df_opt_strat$parameter)) {
      stop("must provide valid parameters")
    }
    df_opt_strat <- df_opt_strat[df_opt_strat$parameter %in% params, ]
    df_opt_strat$parameter <- factor(df_opt_strat$parameter, levels = params)
  } else {
    df_opt_strat$parameter <- factor(df_opt_strat$parameter, levels = unique(owsa$parameter))
  }

  # bars for the plot: one per run of the same optimal strategy along each parameter's values,
  # meeting halfway between the last value of one run and the first value of the next
  df_bars <- opt_strat_runs(owsa, obj_fun, levels(droplevels(df_opt_strat$parameter)),
                            levels(df_opt_strat$strategy))

  g <- ggplot(df_bars) +
    # rectangles with height 1; the y axis carries no information
    geom_rect(aes(xmin = xmin, xmax = xmax,
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
    g
  } else {
    df_opt_strat
  }
}


#' check that object is owsa object
#' @param obj the object to be checked for the owsa class
#' @return returns TRUE if object is "owsa" object and FALSE if not.
#' @keywords internal
is_owsa <- function(obj) {
  if (inherits(obj, "owsa")) {
    TRUE
  } else {
    FALSE
  }
}

#' Runs of the optimal strategy along each parameter in a one-way sensitivity analysis
#'
#' For each parameter, finds the optimal strategy at every parameter value (ties go to the
#' first strategy) and collapses consecutive values with the same optimal strategy into one run.
#' Runs meet halfway between the last value of one run and the first value of the next, so that
#' the bars drawn by \code{\link{owsa_opt_strat}} cover the whole range without gaps or overlaps.
#'
#' @param owsa an \code{owsa} object.
#' @param obj_fun \code{max} or \code{min}.
#' @param v_params parameters to include, in plotting order.
#' @param v_strategies strategies, in legend order.
#' @return a data frame with columns \code{parameter}, \code{strategy}, \code{xmin}, \code{xmax}.
#' @keywords internal
opt_strat_runs <- function(owsa, obj_fun, v_params, v_strategies) {
  l_runs <- lapply(v_params, function(p) {
    df_p <- owsa[owsa$parameter == p, ]
    v_x <- sort(unique(df_p$param_val))
    v_best <- vapply(v_x, function(x) {
      df_x <- df_p[df_p$param_val == x, ]
      as.character(df_x$strategy)[which(df_x$outcome_val == obj_fun(df_x$outcome_val))[1]]
    }, character(1))
    l_rle <- rle(v_best)
    v_end <- cumsum(l_rle$lengths)
    v_start <- c(1, v_end[-length(v_end)] + 1)
    v_mid <- (v_x[-length(v_x)] + v_x[-1]) / 2
    data.frame(parameter = p,
               strategy = l_rle$values,
               xmin = ifelse(v_start == 1, v_x[1], v_mid[pmax(v_start - 1, 1)]),
               xmax = ifelse(v_end == length(v_x), v_x[length(v_x)], v_mid[pmin(v_end, length(v_mid))]),
               stringsAsFactors = FALSE)
  })
  df_runs <- do.call(rbind, l_runs)
  df_runs$parameter <- factor(df_runs$parameter, levels = v_params)
  df_runs$strategy <- factor(df_runs$strategy, levels = v_strategies)
  df_runs
}
