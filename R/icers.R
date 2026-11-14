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
#' @param ref_str name of a reference strategy (one of \code{strategies}) to compare every
#' strategy against, for example \code{"Standard of care"}. The default (\code{NULL}) adds no
#' reference columns. The frontier-based results (\code{Status}, \code{Inc_Cost},
#' \code{Inc_Effect}, \code{ICER}) never depend on the reference.
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
#' If \code{ref_str} is given, four columns compare each strategy with the reference:
#' \code{Inc_Cost_ref} and \code{Inc_Effect_ref} (cost and effect minus those of the
#' reference), \code{ICER_ref}, and \code{Status_ref}, one of \code{"Reference"},
#' \code{"Dominant"} (less costly and at least as effective), \code{"Dominated"} (more costly
#' and no more effective), \code{"More costly, more effective"},
#' \code{"Less costly, less effective"}, or \code{"Equivalent"} (same cost and effect).
#' \code{ICER_ref} is \code{NA} where a ratio is not meaningful: for the reference itself and
#' for dominant, dominated, and equivalent strategies.
#'
#' If \code{wtp} is given, \code{NMB} (\code{wtp * Effect - Cost}) is added, and with
#' \code{ref_str} also \code{Inc_NMB_ref}, the NMB minus that of the reference. With
#' \code{nhb = TRUE}, \code{NHB} (\code{Effect - Cost / wtp}) and \code{Inc_NHB_ref} are added too.
#'
#' @seealso \code{\link{plot.icers}}
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
#' ## Comparing every strategy with a reference strategy
#' hund_ref <- calculate_icers(hund_strat$Cost,
#'                             hund_strat$QALYs,
#'                             hund_strat$Strategy,
#'                             ref_str = hund_strat$Strategy[1],
#'                             wtp = 50000)
#' print(hund_ref, ref = "reference")
#' @export
calculate_icers <- function(cost, effect, strategies, ref_str = NULL, wtp = NULL,
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
  ref_str <- check_ref_str(ref_str, as.character(strategies))
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
    return(add_reference_and_nmb(df_cea, ref_str, wtp, nhb, currency))
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
  add_reference_and_nmb(df_results, ref_str, wtp, nhb, currency)
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

#' Check a reference strategy name
#'
#' @param ref_str reference strategy name, or \code{NULL}.
#' @param strategies names of the strategies in the analysis.
#' @return \code{ref_str} as a character string, or \code{NULL}.
#' @keywords internal
check_ref_str <- function(ref_str, strategies) {
  if (is.null(ref_str)) {
    return(NULL)
  }
  if (length(ref_str) != 1 || is.na(ref_str)) {
    stop("`ref_str` must be a single strategy name.", call. = FALSE)
  }
  ref_str <- as.character(ref_str)
  if (!ref_str %in% strategies) {
    stop("`ref_str` must be one of the strategies: ",
         paste0("\"", strategies, "\"", collapse = ", "), ".", call. = FALSE)
  }
  ref_str
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

#' Status of each strategy relative to a reference strategy
#'
#' @param v_inc_cost,v_inc_effect cost and effect of each strategy minus those of the reference.
#' @return character vector of statuses (the reference itself is labeled by the caller).
#' @keywords internal
status_vs_reference <- function(v_inc_cost, v_inc_effect) {
  ifelse(v_inc_cost == 0 & v_inc_effect == 0, "Equivalent",
         ifelse(v_inc_cost <= 0 & v_inc_effect >= 0, "Dominant",
                ifelse(v_inc_cost >= 0 & v_inc_effect <= 0, "Dominated",
                       ifelse(v_inc_cost > 0, "More costly, more effective",
                              "Less costly, less effective"))))
}

#' Add reference-strategy and net-benefit columns to an icers object
#'
#' The frontier-based columns are not modified. Columns and attributes are only added
#' for the arguments that are given, so with all of them \code{NULL} the object is unchanged.
#'
#' @param df_icers an \code{icers} object.
#' @inheritParams calculate_icers
#' @return \code{df_icers} with the requested columns and attributes added.
#' @keywords internal
add_reference_and_nmb <- function(df_icers, ref_str, wtp, nhb, currency) {
  if (!is.null(ref_str)) {
    # row of the reference strategy; check_ref_str() has already confirmed it exists
    i_ref <- match(ref_str, df_icers$Strategy)
    # differences of every strategy from the reference (the reference's own are 0)
    v_inc_cost <- df_icers$Cost - df_icers$Cost[i_ref]
    v_inc_effect <- df_icers$Effect - df_icers$Effect[i_ref]
    # classify each strategy by its quadrant of the incremental CE plane
    v_status <- status_vs_reference(v_inc_cost, v_inc_effect)
    # the reference has no comparison with itself: label it and blank its differences
    v_status[i_ref] <- "Reference"
    v_inc_cost[i_ref] <- NA_real_
    v_inc_effect[i_ref] <- NA_real_
    # a ratio is only meaningful when cost and effect move in the same direction (NE and SW
    # quadrants); for dominant, dominated, and equivalent strategies ICER_ref stays NA
    v_has_ratio <- v_status %in% c("More costly, more effective", "Less costly, less effective")
    df_icers$Inc_Cost_ref <- v_inc_cost
    df_icers$Inc_Effect_ref <- v_inc_effect
    df_icers$ICER_ref <- ifelse(v_has_ratio, v_inc_cost / v_inc_effect, NA_real_)
    df_icers$Status_ref <- v_status
    # print.icers() and plot.icers() read the reference from this attribute
    attr(df_icers, "ref_str") <- ref_str
  }
  if (!is.null(wtp)) {
    # net monetary benefit of each strategy at the given threshold
    df_icers$NMB <- wtp * df_icers$Effect - df_icers$Cost
    if (!is.null(ref_str)) {
      # incremental NMB versus the reference; positive means cost-effective versus the reference
      df_icers$Inc_NMB_ref <- df_icers$NMB - df_icers$NMB[i_ref]
      df_icers$Inc_NMB_ref[i_ref] <- NA_real_
    }
    if (nhb) {
      # net health benefit: the same comparison expressed in units of effect
      df_icers$NHB <- df_icers$Effect - df_icers$Cost / wtp
      if (!is.null(ref_str)) {
        df_icers$Inc_NHB_ref <- df_icers$NHB - df_icers$NHB[i_ref]
        df_icers$Inc_NHB_ref[i_ref] <- NA_real_
      }
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
#' With \code{ref_str}, \code{wtp}, or \code{nhb}, the reference-strategy and net-benefit columns
#' described in \code{\link{calculate_icers}} are added. With \code{uncertainty = TRUE} and
#' \code{ref_str}, the 2.5th and 97.5th quantiles of the sample-by-sample differences from the reference
#' are also given for \code{Inc_Cost_ref}, \code{Inc_Effect_ref}, and (with \code{wtp})
#' \code{Inc_NMB_ref}, in columns such as \code{Lower_95_Inc_Cost_ref} and \code{Upper_95_Inc_Cost_ref}.
#' @seealso \code{\link{plot.icers}}
#' @seealso \code{\link{calculate_icers}}
#' @importFrom tidyr pivot_longer
#' @export
calculate_icers_psa <- function(psa, uncertainty = FALSE, ref_str = NULL, wtp = NULL, nhb = FALSE) {

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
                              ref_str = ref_str, wtp = wtp, nhb = nhb,
                              currency = psa$currency)

  if (uncertainty == TRUE) {
    # the joins and column selection below drop the icers class and attributes; keep them
    l_attr <- attributes(df_icers)[c("class", "ref_str", "wtp", "currency")]
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
      # (the reference and net-benefit columns), which select() would otherwise drop
      select(Strategy, Cost, Lower_95_Cost, Upper_95_Cost,
             Effect, Lower_95_Effect, Upper_95_Effect,
             Inc_Cost, Inc_Effect, ICER, Status, everything())

    if (!is.null(ref_str)) {
      df_icers <- add_reference_intervals(df_icers, psa, ref_str, wtp)
    }
    df_icers <- as.data.frame(df_icers)
    for (a in names(l_attr)) {
      attr(df_icers, a) <- l_attr[[a]]
    }
  }

  df_icers
}

#' Add 95\% intervals for the differences from a reference strategy
#'
#' Quantiles of the sample-by-sample differences between each strategy and the reference, placed
#' after the corresponding point estimate. \code{NA} for the reference itself.
#'
#' @param df_icers an \code{icers} object with reference columns.
#' @param psa the \code{psa} object it was computed from.
#' @inheritParams calculate_icers
#' @return \code{df_icers} with the interval columns added.
#' @keywords internal
add_reference_intervals <- function(df_icers, psa, ref_str, wtp) {
  # sample-by-sample differences from the reference: one row per PSA sample, one column per
  # strategy (subtracting a vector from a matrix recycles it down each column)
  m_inc_cost <- as.matrix(psa$cost) - psa$cost[[ref_str]]
  m_inc_effect <- as.matrix(psa$effectiveness) - psa$effectiveness[[ref_str]]
  # one matrix of differences per point-estimate column that gets an interval
  l_inc <- list(Inc_Cost_ref = m_inc_cost, Inc_Effect_ref = m_inc_effect)
  if (!is.null(wtp)) {
    l_inc$Inc_NMB_ref <- wtp * m_inc_effect - m_inc_cost
  }
  v_strat <- df_icers$Strategy
  # running column order, so each interval lands right after its point estimate
  v_cols <- names(df_icers)
  for (col in names(l_inc)) {
    # reorder the matrix columns to match the rows of df_icers (frontier order)
    m_inc <- l_inc[[col]][, v_strat, drop = FALSE]
    v_lower <- unname(apply(m_inc, 2, quantile, probs = 0.025, names = FALSE))
    v_upper <- unname(apply(m_inc, 2, quantile, probs = 0.975, names = FALSE))
    # the reference's differences are all 0, so its interval is reported as NA
    v_lower[v_strat == ref_str] <- NA_real_
    v_upper[v_strat == ref_str] <- NA_real_
    df_icers[[paste0("Lower_95_", col)]] <- v_lower
    df_icers[[paste0("Upper_95_", col)]] <- v_upper
    i <- match(col, v_cols)
    v_cols <- append(v_cols, paste0(c("Lower_95_", "Upper_95_"), col), after = i)
  }
  df_icers[, v_cols]
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
#' @param ref_str name of a reference strategy. If given, the plot shows the incremental
#' cost-effectiveness plane, with the reference strategy at the origin. The frontier and the
#' status of each strategy do not change. Defaults to \code{NULL} (the cost-effectiveness plane).
#' @param wtp a willingness-to-pay threshold. If given, a dotted line with slope \code{wtp} is
#' drawn through the reference strategy (or, without \code{ref_str}, through the least costly
#' strategy on the frontier). Strategies below the line are cost-effective compared with that
#' strategy at this threshold.
#' @param ref_axis with \code{ref_str}, draw dashed axes through the origin. Defaults to \code{TRUE}.
#' @param ref_caption add a caption naming the reference strategy and the WTP threshold.
#' Defaults to \code{TRUE}.
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
                       ref_str = NULL,
                       wtp = NULL,
                       ref_axis = TRUE,
                       ref_caption = TRUE,
                       ...) {
  # column name used unquoted in arrange() below; avoids an R CMD check note
  Status <- NULL
  if (is.null(currency)) {
    currency <- if (is.null(attr(x, "currency"))) "$" else attr(x, "currency")
  }
  ref_str <- check_ref_str(ref_str, x$Strategy)
  check_wtp_nhb(wtp, nhb = FALSE)

  # keep only the columns the plot uses, as a plain data frame, so the plot data are the same
  # whether or not x has uncertainty bounds, reference columns, or stored attributes
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

  # point the WTP line goes through: the reference, or the least costly frontier strategy
  if (!is.null(wtp)) {
    df_front <- x[x$Status == nd_name, ]
    df_anchor <- if (is.null(ref_str)) df_front[which.min(df_front$Cost), ] else x[x$Strategy == ref_str, ]
  }

  # incremental plane: put the reference strategy at the origin
  x_lab <- paste0("Effect (", effect_units, ")")
  y_lab <- paste0("Cost (", currency, ")")
  if (!is.null(ref_str)) {
    i_ref <- which(x$Strategy == ref_str)
    x$Cost <- x$Cost - x$Cost[i_ref]
    x$Effect <- x$Effect - x$Effect[i_ref]
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

  if (!is.null(ref_str) && ref_axis) {
    icer_plot <- icer_plot +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
  }
  if (!is.null(wtp)) {
    intercept <- if (is.null(ref_str)) df_anchor$Cost - wtp * df_anchor$Effect else 0
    icer_plot <- icer_plot +
      geom_abline(intercept = intercept, slope = wtp, linetype = "dotted", color = "grey30")
  }
  v_caption <- c(if (!is.null(ref_str)) paste0("Reference Strategy: ", ref_str),
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
#' By default, prints the sequential cost-effectiveness table: each strategy on the frontier is
#' compared with the previous one. If the \code{icers} object was created with a reference
#' strategy (\code{ref_str} in \code{\link{calculate_icers}}), \code{ref = "reference"} shows the
#' comparison of every strategy with the reference instead, listing the reference first and the
#' other strategies by increasing cost. \code{ref = "both"} shows both comparisons, in the
#' frontier order of the sequential table.
#'
#' @param x Object of class \code{icers}
#' @param currency string. with currency used in the cost-effectiveness analysis (CEA).
#' Defaults to the currency stored in \code{x}, or \code{"$"}.
#' @param dig_cost integer. number of digits to round costs to
#' @param dig_eff integer. number of digits to round effects to
#' @param dig_icer integer. number of digits to round ICER to
#' @param ref which comparison to show: \code{"sequential"} (the default), \code{"reference"},
#' or \code{"both"}.
#' @param ... Additional arguments passed to other methods.
#' @return a dataframe object - formatted CEA table
#' @export
print.icers <- function(x,
                        currency = NULL,
                        dig_cost = 0,
                        dig_eff  = 2,
                        dig_icer = 0,
                        ref = c("sequential", "reference", "both"),
                        ...) {
  # check if icers object
  if (!inherits(x, "icers")) {
    stop("x must be an object of class icers")
  }
  ref <- match.arg(ref)
  if (is.null(currency)) {
    currency <- if (is.null(attr(x, "currency"))) "$" else attr(x, "currency")
  }
  ref_str <- attr(x, "ref_str")
  if (ref != "sequential" && is.null(ref_str)) {
    stop("x has no reference strategy; create it with calculate_icers(..., ref_str = ) ",
         "to use ref = \"", ref, "\".", call. = FALSE)
  }

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

  # columns that belong to each view
  v_seq_cols <- c("Inc_Cost", "Inc_Effect", "ICER", "Status")
  v_ref_cols <- grep("_ref$", names(x), value = TRUE)
  v_drop <- switch(ref, sequential = v_ref_cols, reference = v_seq_cols, both = character(0))
  df_table_cea <- x
  class(df_table_cea) <- "data.frame"
  df_table_cea <- df_table_cea[, setdiff(names(df_table_cea), v_drop), drop = FALSE]
  # the reference view lists the reference first, then the other strategies by cost; the
  # sequential and combined views keep the frontier order, in which the sequential ICERs are read
  if (ref == "reference") {
    v_order <- order(df_table_cea$Strategy != ref_str, df_table_cea$Cost)
    df_table_cea <- df_table_cea[v_order, , drop = FALSE]
    rownames(df_table_cea) <- NULL
  }

  # display digits and header for each known column
  vs <- paste0(" vs ", ref_str)
  l_format <- list(
    Cost           = list(dig_cost, paste0("Costs (", currency, ")")),
    Effect         = list(dig_eff, "QALYs"),
    Inc_Cost       = list(dig_cost, paste0("Incremental Costs (", currency, ")")),
    Inc_Effect     = list(dig_eff, "Incremental QALYs"),
    ICER           = list(dig_icer, paste0("ICER (", currency, "/QALY)")),
    Inc_Cost_ref   = list(dig_cost, paste0("Incremental Costs", vs, " (", currency, ")")),
    Inc_Effect_ref = list(dig_eff, paste0("Incremental QALYs", vs)),
    ICER_ref       = list(dig_icer, paste0("ICER", vs, " (", currency, "/QALY)")),
    Status_ref     = list(NA, paste0("Status", vs)),
    NMB            = list(dig_cost, paste0("NMB (", currency, ")")),
    Inc_NMB_ref    = list(dig_cost, paste0("Incremental NMB", vs, " (", currency, ")")),
    NHB            = list(dig_eff, "NHB (QALYs)"),
    Inc_NHB_ref    = list(dig_eff, paste0("Incremental NHB", vs, " (QALYs)"))
  )
  for (col in intersect(names(l_format), names(df_table_cea))) {
    if (!is.na(l_format[[col]][[1]])) {
      df_table_cea[[col]] <- format_numbers(df_table_cea[[col]], n_digits = l_format[[col]][[1]])
    }
    names(df_table_cea)[names(df_table_cea) == col] <- l_format[[col]][[2]]
  }

  print(df_table_cea)
  invisible(df_table_cea)
}
