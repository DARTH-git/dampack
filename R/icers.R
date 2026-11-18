#' Calculate incremental cost-effectiveness ratios (ICERs)
#'
#' @description
#' This function takes in strategies and their associated cost and effect, assigns them
#' one of three statuses (non-dominated, extended dominated, or dominated), and
#' calculates the incremental cost-effectiveness ratios for the non-dominated strategies
#'
#' The cost-effectiveness frontier can be visualized with \code{plot}, which calls \code{\link{plot.icers}}.
#'
#' An efficient way to get from a probabilistic sensitivity analysis to an ICER table
#' is by using \code{summary} on the PSA object and then using its columns as
#' inputs to \code{calculate_icers}.
#'
#' @param cost vector of cost for each strategy
#' @param effect vector of effect for each strategy
#' @param strategies string vector of strategy names
#' @param wtp a single willingness-to-pay threshold (currency per unit of effect). If given,
#' the net monetary benefit (\code{NMB}) of each strategy is added. The default (\code{NULL})
#' adds no net-benefit columns.
#' @param nhb if \code{TRUE}, also add the net health benefit (\code{NHB}). Requires \code{wtp}.
#' @param currency currency symbol used when printing and plotting the results, for example
#' \code{"€"}. The default (\code{NULL}) uses \code{"$"}.
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, incremental cost, incremental effect, and ICER, computed sequentially along the
#' cost-effectiveness frontier.
#'
#' If \code{wtp} is given, \code{NMB} (\code{wtp * Effect - Cost}) is added. With
#' \code{nhb = TRUE}, \code{NHB} (\code{Effect - Cost / wtp}) is added too.
#'
#' The ICERs are always sequential: each strategy on the frontier is compared with the next less
#' costly one, after removing dominated and extendedly dominated strategies. This is the fully
#' incremental analysis that health technology assessment guidelines require when more than two
#' strategies are compared. To compare every strategy with a single reference strategy instead,
#' use \code{\link{calculate_cers_ref}}.
#'
#' @seealso \code{\link{plot.icers}}, \code{\link{calculate_cers_ref}}
#'
#' @examples
#' ## Base Case
#' # if you have a base case analysis, can use calculate_icers on that
#' data(hund_strat)
#' hund_icers <- calculate_icers(hund_strat$Cost,
#'                               hund_strat$QALYs,
#'                               hund_strat$Strategy)
#'
#' plot(hund_icers)
#' # we have so many strategies that we may just want to plot the frontier
#' plot(hund_icers, plot_frontier_only = TRUE)
#' # see ?plot.icers for more options
#'
#' ## Using a PSA object
#' data(psa_cdiff)
#'
#' # summary() gives mean cost and effect for each strategy
#' sum_cdiff <- summary(psa_cdiff)
#'
#' # calculate icers
#' icers <- calculate_icers(sum_cdiff$meanCost,
#'                          sum_cdiff$meanEffect,
#'                          sum_cdiff$Strategy)
#' icers
#'
#' # visualize
#' plot(icers)
#'
#' # by default, only the frontier is labeled
#' # if using a small number of strategies, you can label all the points
#' # note that longer strategy names will get truncated
#' plot(icers, label = "all")
#'
#' # the incremental cost-effectiveness plane, with the least costly strategy on the
#' # frontier at the origin
#' plot(icers, incremental = TRUE)
#'
#' ## Net monetary benefit at a willingness-to-pay threshold
#' calculate_icers(sum_cdiff$meanCost, sum_cdiff$meanEffect, sum_cdiff$Strategy,
#'                 wtp = 50000)
#' @export
calculate_icers <- function(cost, effect, strategies, wtp = NULL,
                            nhb = FALSE, currency = NULL) {
  # checks on input
  n_cost <- length(cost)
  n_eff <- length(effect)
  n_strat <- length(strategies)
  if (n_cost != n_eff || n_eff != n_strat) {
    stop("cost, effect, and strategies must all be vectors of the same length", call. = FALSE)
  }
  if (n_strat == 0) {
    stop("cost, effect, and strategies must contain at least one strategy", call. = FALSE)
  }
  check_wtp_nhb(wtp, nhb)

  # column names used unquoted in the dplyr code below; declaring them avoids the
  # R CMD check note "no visible binding for global variable"
  Cost <- Effect <- Status <- Strategy <- Inc_Cost <- Inc_Effect <- ICER <- NULL

  # coerce to character, in case they are provided as numeric
  v_char_strat <- as.character(strategies)

  # create data frame to hold data
  df_cea <- data.frame("Strategy" = v_char_strat,
                       "Cost" = cost,
                       "Effect" = effect,
                       stringsAsFactors = FALSE)
  nstrat <- nrow(df_cea)

  # if only one strategy was provided, it is on the frontier by definition and
  # has no incremental values; return the same layout and class as below
  if (nstrat == 1) {
    df_cea$Inc_Cost <- NA_real_
    df_cea$Inc_Effect <- NA_real_
    df_cea$ICER <- NA_real_
    df_cea$Status <- "ND"
    class(df_cea) <- c("icers", "data.frame")
    return(add_nmb(df_cea, wtp, nhb, currency))
  }

  # three statuses: dominated, extended dominated, and non-dominated
  v_d <- NULL

  # detect dominated strategies
  # dominated strategies have a higher cost and lower effect
  df_cea <- df_cea %>%
    arrange(Cost, desc(Effect))

  # iterate over strategies and detect (strongly) dominated strategies
  # those with higher cost and equal or lower effect
  for (i in 1:(nstrat - 1)) {
    ith_effect <- df_cea[i, "Effect"]
    for (j in (i + 1):nstrat) {
      jth_effect <- df_cea[j, "Effect"]
      if (jth_effect <= ith_effect) {
        # append dominated strategies to vector
        v_d <- c(v_d, df_cea[j, "Strategy"])
      }
    }
  }

  # detect weakly dominated strategies (extended dominance)
  # this needs to be repeated until there are no more ED strategies
  v_ed <- vector()
  continue <- TRUE  # ensure that the loop is run at least once
  while (continue) {
    # vector of all dominated strategies (strong or weak)
    v_dom <- union(v_d, v_ed)

    # strategies declared to be non-dominated at this point
    v_nd <- setdiff(strategies, v_dom)

    # compute icers for nd strategies
    df_nd <- df_cea[df_cea$Strategy %in% v_nd, ] %>%
      compute_icers()

    # number non-d
    n_non_d <- nrow(df_nd)

    # if only two strategies left, we're done
    if (n_non_d <= 2) {
      break
    }

    # strategy identifiers for non-d
    v_nd_strat <- df_nd$Strategy

    # now, go through non-d strategies and detect any
    # with higher ICER than following strategy
    ## keep track of whether any ED strategies are picked up
    # if not, we're done - exit the loop
    new_ed <- 0
    for (i in 2:(n_non_d - 1)) {
      if (df_nd[i, "ICER"] > df_nd[i + 1, "ICER"]) {
        v_ed <- c(v_ed, v_nd_strat[i])
        new_ed <- new_ed + 1
      }
    }
    if (new_ed == 0) {
      continue <- FALSE
    }
  }

  # recompute icers without weakly dominated strategies
  df_nd_icers <- df_nd[!(df_nd$Strategy %in% v_dom), ] %>%
    mutate(Status = "ND") %>%
    compute_icers()

  # dominated and weakly dominated
  df_d <- df_cea[df_cea$Strategy %in% v_d, ] %>%
    mutate(ICER = NA, Status = "D")

  df_ed <- df_cea[df_cea$Strategy %in% v_ed, ] %>%
    mutate(ICER = NA, Status = "ED")

  # when combining, sort so we have ref,ND,ED,D
  df_results <- bind_rows(df_d, df_ed, df_nd_icers) %>%
    arrange(desc(Status), Cost, desc(Effect))

  # re-arrange columns
  df_results <- df_results %>%
    select(Strategy, Cost, Effect,
           Inc_Cost, Inc_Effect, ICER, Status)

  # declare class of results
  class(df_results) <- c("icers", "data.frame")
  add_nmb(df_results, wtp, nhb, currency)
}

#' Caption text for a willingness-to-pay line
#'
#' @param wtp willingness-to-pay threshold.
#' @param currency currency symbol.
#' @param effect_units units of effectiveness, e.g. \code{"QALYs"}.
#' @return a string such as \code{"Dotted line: WTP = $50,000/QALY"}.
#' @keywords internal
wtp_caption <- function(wtp, currency, effect_units = "QALYs") {
  unit <- per_unit(effect_units)
  paste0("Dotted line: WTP = ", currency, format(wtp, big.mark = ",", scientific = FALSE, trim = TRUE),
         "/", unit)
}

#' Check the willingness-to-pay and NHB arguments of calculate_icers()
#'
#' @inheritParams calculate_icers
#' @return \code{NULL}, invisibly; called for its errors.
#' @keywords internal
check_wtp_nhb <- function(wtp, nhb) {
  if (!is.null(wtp) && (!is.numeric(wtp) || length(wtp) != 1 || !is.finite(wtp) || wtp < 0)) {
    stop("`wtp` must be a single non-negative number.", call. = FALSE)
  }
  if (!isTRUE(nhb) && !isFALSE(nhb)) {
    stop("`nhb` must be TRUE or FALSE.", call. = FALSE)
  }
  if (nhb && (is.null(wtp) || wtp == 0)) {
    stop("`nhb = TRUE` requires `wtp` greater than zero.", call. = FALSE)
  }
  invisible(NULL)
}

#' Add net-benefit columns and stored attributes to an icers object
#'
#' The frontier-based columns are not modified. Columns and attributes are only added
#' for the arguments that are given, so with all of them \code{NULL} the object is unchanged.
#'
#' @param df_icers an \code{icers} object.
#' @inheritParams calculate_icers
#' @return \code{df_icers} with the requested columns and attributes added.
#' @keywords internal
add_nmb <- function(df_icers, wtp, nhb, currency) {
  if (!is.null(wtp)) {
    # net monetary benefit of each strategy at the given threshold
    df_icers$NMB <- wtp * df_icers$Effect - df_icers$Cost
    if (nhb) {
      # net health benefit: the same comparison expressed in units of effect
      df_icers$NHB <- df_icers$Effect - df_icers$Cost / wtp
    }
    attr(df_icers, "wtp") <- wtp
  }
  if (!is.null(currency)) {
    attr(df_icers, "currency") <- currency
  }
  df_icers
}

#' Calculate incremental cost-effectiveness ratios from a \code{psa} object.
#'
#' @description The mean costs and QALYs for each strategy in a PSA are used
#' to conduct an incremental cost-effectiveness analysis. \code{\link{calculate_icers}} should be used
#' if costs and QALYs for each strategy need to be specified manually, whereas \code{calculate_icers_psa}
#' can be used if mean costs and mean QALYs from the PSA are assumed to represent a base case scenario for
#' calculation of ICERS.
#'
#' Optionally, the \code{uncertainty} argument can be used to provide the 2.5th and 97.5th
#' quantiles for each strategy's cost and QALY outcomes based on the variation present in the PSA.
#' Because the dominated vs. non-dominated status and the ordering of strategies in the ICER table are
#' liable to change across different samples of the PSA, confidence intervals are not provided for the
#' incremental costs and QALYs along the cost-effectiveness acceptability frontier.
#' \code{link{plot.psa}} does not show the confidence intervals in the resulting plot
#' even if present in the ICER table.
#'
#' @param psa \code{psa} object from \code{link{make_psa_object}}
#' @param uncertainty whether or not 95\% quantiles for the cost and QALY outcomes should be included
#' in the resulting ICER table. Defaults to \code{FALSE}.
#' @inheritParams calculate_icers
#'
#' @return A data frame and \code{icers} object of strategies and their associated
#' status, cost, effect, incremental cost, incremental effect, and ICER. If \code{uncertainty} is
#' set to \code{TRUE}, four additional columns are provided for the 2.5th and 97.5th quantiles for
#' each strategy's cost and effect.
#'
#' With \code{wtp} or \code{nhb}, the net-benefit columns described in
#' \code{\link{calculate_icers}} are added.
#' @seealso \code{\link{plot.icers}}
#' @seealso \code{\link{calculate_icers}}
#' @seealso \code{\link{calculate_cers_ref_psa}} to compare every strategy with a reference strategy
#' @importFrom tidyr pivot_longer
#' @export
calculate_icers_psa <- function(psa, uncertainty = FALSE, wtp = NULL, nhb = FALSE) {

  # check that psa has class 'psa'
  check_psa_object(psa)

  # Calculate mean outcome values
  df_psa_sum <- summary(psa)

  # column names used unquoted in the dplyr code below; declaring them avoids the
  # R CMD check note "no visible binding for global variable"
  Cost <- Effect <- Status <- Strategy <- Inc_Cost <- Inc_Effect <- ICER <- value <-
    Lower_95_Cost <- Upper_95_Cost <- Lower_95_Effect <- Upper_95_Effect <- NULL

  # Supply mean outcome values to calculate_icers
  df_icers <- calculate_icers(cost = df_psa_sum$meanCost,
                              effect = df_psa_sum$meanEffect,
                              strategies = df_psa_sum$Strategy,
                              wtp = wtp, nhb = nhb,
                              currency = psa$currency)

  if (uncertainty == TRUE) {
    # the joins and column selection below drop the icers class and attributes; keep them
    l_attr <- attributes(df_icers)[c("class", "wtp", "currency")]
    l_attr <- l_attr[!vapply(l_attr, is.null, logical(1))]

    # extract cost and effect data.frames from psa object
    df_cost <- psa$cost
    df_effect <- psa$effectiveness

    # Calculate quantiles across costs and effects
    df_cost_bounds <- df_cost %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(Strategy) %>%
      summarize(Lower_95_Cost = quantile(value, probs = 0.025, names = FALSE),
                Upper_95_Cost = quantile(value, probs = 0.975, names = FALSE))

    df_effect_bounds <- df_effect %>%
      pivot_longer(cols = everything(), names_to = "Strategy") %>%
      group_by(Strategy) %>%
      summarize(Lower_95_Effect = quantile(value, probs = 0.025, names = FALSE),
                Upper_95_Effect = quantile(value, probs = 0.975, names = FALSE))

    # merge bound data.frames into icers data.frame
    df_icers <- df_icers %>%
      left_join(df_cost_bounds, by = "Strategy") %>%
      left_join(df_effect_bounds, by = "Strategy") %>%
      # put each interval next to its estimate; everything() keeps the remaining columns
      # (the net-benefit columns), which select() would otherwise drop
      select(Strategy, Cost, Lower_95_Cost, Upper_95_Cost,
             Effect, Lower_95_Effect, Upper_95_Effect,
             Inc_Cost, Inc_Effect, ICER, Status, everything())

    df_icers <- as.data.frame(df_icers)
    for (a in names(l_attr)) {
      attr(df_icers, a) <- l_attr[[a]]
    }
  }

  df_icers
}

#' compute icers for non-dominated strategies
#'
#' @param non_d a data frame of non-dominated strategies, with columns
#' "Strategy", "Cost", and "Effect"
#'
#' @return the input dataframe with columns "Inc_Cost",
#' "Inc_Effect", and "ICER" appended
#'
#' @keywords internal
compute_icers <- function(non_d) {
  if (nrow(non_d) > 1) {
    non_d[1, "ICER"] <- NA
    for (i in 2:nrow(non_d)) {
      inc_cost <- (non_d[i, "Cost"] - non_d[i - 1, "Cost"])
      inc_effect <- (non_d[i, "Effect"] - non_d[i - 1, "Effect"])
      non_d[i, "Inc_Cost"] <- inc_cost
      non_d[i, "Inc_Effect"] <- inc_effect
      non_d[i, "ICER"] <- inc_cost / inc_effect
    }
  } else {
    # don't calculate ICER if only one strategy
    non_d[1, c("ICER", "Inc_Cost", "Inc_Effect")] <- NA
  }
  non_d
}

#' Plot of ICERs
#'
#' Plots the cost-effectiveness plane for a ICER object, calculated with \code{\link{calculate_icers}}
#' @param x Object of class \code{icers}.
#' @inheritParams add_common_aes
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' @param effect_units string. unit of effectiveness
#' @param label whether to label strategies on the efficient frontier, all strategies, or none.
#' defaults to frontier.
#' @param label_max_char max number of characters to label the strategies - if not NULL (the default)
#' longer strategies are truncated to save space.
#' @param plot_frontier_only only plot the efficient frontier
#' @param alpha opacity of points
#' @param incremental if \code{TRUE}, plot the incremental cost-effectiveness plane: the costs
#' and effects of every strategy are shown relative to those of the first non-dominated strategy
#' (the least costly strategy on the frontier), which is placed at the origin (0, 0). The frontier
#' and the status of each strategy do not change. Defaults to \code{FALSE} (absolute costs and
#' effects). To compare every strategy with a reference strategy of your choice, use
#' \code{\link{calculate_cers_ref}} and its \code{plot} method.
#' @param wtp a willingness-to-pay threshold. If given, a dotted line with slope \code{wtp} is
#' drawn through the least costly strategy on the frontier. Strategies below the line are
#' cost-effective compared with that strategy at this threshold.
#' @param ref_axis with \code{incremental = TRUE}, draw dashed axes through the origin.
#' Defaults to \code{TRUE}.
#' @param ref_caption add a caption naming the strategy at the origin (with
#' \code{incremental = TRUE}) and the WTP threshold (with \code{wtp}). Defaults to \code{TRUE}.
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return a ggplot2 object which can be modified by adding additional geoms
#'
#' @importFrom stringr str_sub
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang !!
#' @importFrom rlang sym
#' @export
plot.icers <- function(x,
                       txtsize = 12,
                       currency = NULL,
                       effect_units = "QALYs",
                       label = c("frontier", "all", "none"),
                       label_max_char = NULL,
                       plot_frontier_only = FALSE,
                       alpha = 1,
                       n_x_ticks = 6,
                       n_y_ticks = 6,
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xexpand = expansion(0.1),
                       yexpand = expansion(0.1),
                       max.iter = 20000,
                       incremental = FALSE,
                       wtp = NULL,
                       ref_axis = TRUE,
                       ref_caption = TRUE,
                       ...) {
  # column name used unquoted in arrange() below; avoids an R CMD check note
  Status <- NULL
  if (is.null(currency)) {
    currency <- if (is.null(attr(x, "currency"))) "$" else attr(x, "currency")
  }
  if (!isTRUE(incremental) && !isFALSE(incremental)) {
    stop("`incremental` must be TRUE or FALSE.", call. = FALSE)
  }
  check_wtp_nhb(wtp, nhb = FALSE)

  # keep only the columns the plot uses, as a plain data frame, so the plot data are the same
  # whether or not x has uncertainty bounds, net-benefit columns, or stored attributes
  v_plot_cols <- c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "ICER", "Status")
  x <- data.frame(unclass(x)[v_plot_cols], check.names = FALSE, stringsAsFactors = FALSE)

  # type checking
  label <- match.arg(label)

  # this is so non-dominated strategies are plotted last (on top)
  x <- arrange(x, Status)

  # change status text in data frame for plotting
  d_name <- "Dominated"
  ed_name <- "Weakly Dominated"
  nd_name <- "Efficient Frontier"

  v_status_expand <- c("D" = d_name, "ED" = ed_name,
                       "ND" = nd_name, "ref" = nd_name)
  x$Status <- factor(v_status_expand[x$Status], ordered = FALSE,
                     levels = c(d_name, ed_name, nd_name))

  # linetype
  v_plot_lines <- c("Dominated" = "blank",
                    "Weakly Dominated" = "blank",
                    "Efficient Frontier" = "solid")

  # names to refer to in aes
  stat_name <- "Status"
  strat_name <- "Strategy"
  eff_name <- "Effect"
  cost_name <- "Cost"

  # first non-dominated strategy: the least costly strategy on the frontier. It is the origin of
  # the incremental plane and the point the WTP line goes through
  df_front <- x[x$Status == nd_name, ]
  df_anchor <- df_front[which.min(df_front$Cost), ]

  x_lab <- paste0("Effect (", effect_units, ")")
  y_lab <- paste0("Cost (", currency, ")")
  if (incremental) {
    x$Cost <- x$Cost - df_anchor$Cost
    x$Effect <- x$Effect - df_anchor$Effect
    x_lab <- paste0("Incremental Effect (", effect_units, ")")
    y_lab <- paste0("Incremental Cost (", currency, ")")
  }

  # frontier only
  if (plot_frontier_only) {
    df_plt_data <- x[x$Status == nd_name, ]
  } else {
    df_plt_data <- x
  }

  # make plot
  icer_plot <- ggplot(df_plt_data, aes(x = !!sym(eff_name), y = !!sym(cost_name),
                                       shape = !!sym(stat_name))) +
    geom_point(alpha = alpha, size = 2) +
    geom_line(aes(linetype = !!sym(stat_name), group = !!sym(stat_name))) +
    scale_linetype_manual(name = NULL, values = v_plot_lines) +
    scale_shape_discrete(name = NULL) +
    labs(x = x_lab, y = y_lab)

  if (incremental && ref_axis) {
    icer_plot <- icer_plot +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
  }
  if (!is.null(wtp)) {
    # through the anchor strategy, which is at the origin on the incremental plane
    intercept <- if (incremental) 0 else df_anchor$Cost - wtp * df_anchor$Effect
    icer_plot <- icer_plot +
      geom_abline(intercept = intercept, slope = wtp, linetype = "dotted", color = "grey30")
  }
  v_caption <- c(if (incremental) paste0("Origin: ", df_anchor$Strategy,
                                         " (least costly strategy on the frontier)"),
                 if (!is.null(wtp)) wtp_caption(wtp, currency, effect_units))
  if (ref_caption && length(v_caption)) {
    icer_plot <- icer_plot + labs(caption = paste(v_caption, collapse = "; "))
  }

  icer_plot <- add_common_aes(icer_plot, txtsize, col = "none",
                              continuous = c("x", "y"),
                              n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                              xbreaks = xbreaks, ybreaks = ybreaks,
                              xlim = xlim, ylim = ylim,
                              xexpand = xexpand, yexpand = yexpand)

  # labeling
  if (label != "none") {
    if (!is.null(label_max_char)) {
      df_plt_data[, strat_name] <- str_sub(df_plt_data[, strat_name],
                                           start = 1L, end = label_max_char)
    }
    if (label == "all") {
      df_lab_data <- df_plt_data
    }
    if (label == "frontier") {
      df_lab_data <- df_plt_data[df_plt_data$Status == nd_name, ]
    }

    icer_plot <- icer_plot +
      geom_label_repel(data = df_lab_data,
                       aes(label = !!sym(strat_name)),
                       size = 3,
                       show.legend = FALSE,
                       max.iter = max.iter,
                       direction = "both")
  }
  icer_plot
}


#' Print the formatted CEA table
#'
#' Prints the sequential cost-effectiveness table: each strategy on the frontier is compared with
#' the previous one.
#'
#' @param x Object of class \code{icers}
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' Defaults to the currency stored in \code{x}, or \code{"$"}.
#' @param dig_cost integer. number of digits to round costs to
#' @param dig_eff integer. number of digits to round effects to
#' @param dig_icer integer. number of digits to round ICER to
#' @param ... Additional arguments passed to other methods.
#' @return a dataframe object - formatted CEA table
#' @export
print.icers <- function(x,
                        currency = NULL,
                        dig_cost = 0,
                        dig_eff  = 2,
                        dig_icer = 0,
                        ...) {
  # check if icers object
  if (!inherits(x, "icers")) {
    stop("x must be an object of class icers")
  }
  if (is.null(currency)) {
    currency <- if (is.null(attr(x, "currency"))) "$" else attr(x, "currency")
  }

  df_table_cea <- x
  class(df_table_cea) <- "data.frame"

  # display digits and header for each known column
  l_format <- list(
    Cost       = list(dig_cost, paste0("Costs (", currency, ")")),
    Effect     = list(dig_eff, "QALYs"),
    Inc_Cost   = list(dig_cost, paste0("Incremental Costs (", currency, ")")),
    Inc_Effect = list(dig_eff, "Incremental QALYs"),
    ICER       = list(dig_icer, paste0("ICER (", currency, "/QALY)")),
    NMB        = list(dig_cost, paste0("NMB (", currency, ")")),
    NHB        = list(dig_eff, "NHB (QALYs)")
  )
  df_table_cea <- format_cea_table(df_table_cea, l_format)

  print(df_table_cea)
  invisible(df_table_cea)
}

#' Format the numeric columns of a CEA table for printing
#'
#' @param df_table data frame to format.
#' @param l_format named list, one element per column to format: a list of the number of digits
#' (\code{NA} to leave the column as it is) and the header to print.
#' @return \code{df_table} with the formatted columns, as character, and renamed.
#' @keywords internal
format_cea_table <- function(df_table, l_format) {
  format_numbers <- function(col, n_digits) {
    v_new_col <- rep("-", length(col))
    v_non_na <- !is.na(col)
    if (n_digits > 0) {
      v_new_col[v_non_na] <- format(col[v_non_na], big.mark = ",", nsmall = n_digits,
                                    digits = n_digits, scientific = FALSE)
    } else {
      v_new_col[v_non_na] <- format(round(col[v_non_na], n_digits), big.mark = ",",
                                    nsmall = n_digits, scientific = FALSE)
    }
    v_new_col
  }
  for (col in intersect(names(l_format), names(df_table))) {
    if (!is.na(l_format[[col]][[1]])) {
      df_table[[col]] <- format_numbers(df_table[[col]], n_digits = l_format[[col]][[1]])
    }
    names(df_table)[names(df_table) == col] <- l_format[[col]][[2]]
  }
  df_table
}
