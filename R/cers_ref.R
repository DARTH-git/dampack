# nolint start: line_length_linter. (guideline URL in the references)
#' Compare every strategy with a reference strategy
#'
#' @description
#' Compares each strategy with a single reference strategy chosen by the user, such as the
#' current standard of care: the difference in cost and in effect from the reference, the
#' cost-effectiveness ratio (CER) of that difference, and where each strategy lies on the
#' incremental cost-effectiveness plane centered at the reference.
#'
#' These comparisons are \emph{not} the incremental cost-effectiveness ratios (ICERs) of a
#' fully incremental analysis. When more than two strategies are compared, health technology
#' assessment guidelines require a fully incremental (sequential) analysis, in which dominated
#' and extendedly dominated strategies are removed and each strategy on the cost-effectiveness
#' frontier is compared with the next less costly one: use \code{\link{calculate_icers}} for that.
#' Comparisons with a single reference are a complement for settings where they are relevant
#' and justified. The NICE manual (2022), for example, states that "pairwise comparisons may be
#' presented when relevant and justified (for example, when the technology is expected to
#' specifically displace individual comparators)", after requiring a fully incremental
#' analysis. The CDA-AMC (2017) and Zorginstituut Nederland (2024) guidelines also require a
#' sequential analysis when more than two interventions are compared.
#'
#' @param cost vector of cost for each strategy.
#' @param effect vector of effect for each strategy.
#' @param strategies string vector of strategy names.
#' @param ref_str name of the reference strategy, one of \code{strategies}, for example
#' \code{"Standard of care"}.
#' @param wtp a single willingness-to-pay threshold (currency per unit of effect). If given, the
#' net monetary benefit of each strategy and its difference from the reference are added.
#' The default (\code{NULL}) adds no net-benefit columns.
#' @param nhb if \code{TRUE}, also add the net health benefit and its difference from the
#' reference. Requires \code{wtp}.
#' @param currency currency symbol used when printing and plotting the results, for example
#' \code{"€"}. The default (\code{NULL}) uses \code{"$"}.
#'
#' @return A data frame of class \code{cers_ref}, with the reference strategy in the first row
#' and the other strategies by increasing cost, and these columns:
#' \describe{
#'   \item{Strategy, Cost, Effect}{as given.}
#'   \item{Inc_Cost, Inc_Effect}{cost and effect minus those of the reference.}
#'   \item{CER}{\code{Inc_Cost / Inc_Effect}, the cost-effectiveness ratio compared with the
#'   reference. It is \code{NA} where a ratio is not meaningful: for the reference itself and for
#'   dominant, dominated and equivalent strategies.}
#'   \item{Status}{one of \code{"Reference"}, \code{"Dominant"} (not more costly and at least as
#'   effective), \code{"Dominated"} (not less costly and no more effective),
#'   \code{"More costly, more effective"}, \code{"Less costly, less effective"}, or
#'   \code{"Equivalent"} (same cost and effect).}
#'   \item{NMB, Inc_NMB}{with \code{wtp}: the net monetary benefit, \code{wtp * Effect - Cost},
#'   and its difference from the reference. A strategy with a positive \code{Inc_NMB} is
#'   cost-effective compared with the reference at this threshold.}
#'   \item{NHB, Inc_NHB}{with \code{nhb = TRUE}: the net health benefit,
#'   \code{Effect - Cost / wtp}, and its difference from the reference.}
#' }
#' The reference strategy, threshold and currency are stored as attributes, which the
#' \code{print} and \code{plot} methods use.
#'
#' For a strategy that is more costly and more effective than the reference, a \code{CER} below
#' the willingness-to-pay threshold means it is cost-effective compared with the reference. For a
#' strategy that is less costly and less effective, the \code{CER} is the cost saved per unit of
#' effect lost, and the strategy is cost-effective compared with the reference when its
#' \code{CER} is \emph{above} the threshold.
#'
#' @references
#' National Institute for Health and Care Excellence (NICE). NICE health technology evaluations:
#' the manual (PMG36), section 4.6, "Economic evaluation". London: NICE; 2022.
#' \url{https://www.nice.org.uk/process/pmg36/chapter/economic-evaluation-2}
#'
#' CDA-AMC (formerly CADTH). Guidelines for the Economic Evaluation of Health Technologies:
#' Canada. 4th ed. Ottawa: CADTH; 2017.
#' \url{https://www.cda-amc.ca/sites/default/files/pdf/guidelines_for_the_economic_evaluation_of_health_technologies_canada_4th_ed.pdf}
#'
#' Zorginstituut Nederland. Guideline for economic evaluations in healthcare (2024 version).
#' Diemen: Zorginstituut Nederland; 2024.
#' \url{https://english.zorginstituutnederland.nl/documents/2024/01/16/guideline-for-economic-evaluations-in-healthcare}
#'
#' @seealso \code{\link{plot.cers_ref}}, \code{\link{calculate_cers_ref_psa}} for the same
#' comparison from a PSA, and \code{\link{calculate_icers}} for the fully incremental analysis.
#'
#' @examples
#' # three strategies; "Standard of care" is neither the cheapest nor the least effective
#' df_ref <- calculate_cers_ref(cost = c(12000, 9000, 20000),
#'                              effect = c(8.0, 7.6, 8.5),
#'                              strategies = c("Standard of care", "Generic drug", "New drug"),
#'                              ref_str = "Standard of care",
#'                              wtp = 50000)
#' df_ref
#' plot(df_ref)
#'
#' # the fully incremental analysis of the same strategies
#' calculate_icers(cost = c(12000, 9000, 20000),
#'                 effect = c(8.0, 7.6, 8.5),
#'                 strategies = c("Standard of care", "Generic drug", "New drug"))
#' @export
calculate_cers_ref <- function(cost, effect, strategies, ref_str, wtp = NULL, nhb = FALSE,
                               currency = NULL) {
  if (length(cost) != length(effect) || length(effect) != length(strategies)) {
    stop("cost, effect, and strategies must all be vectors of the same length", call. = FALSE)
  }
  if (length(strategies) == 0) {
    stop("cost, effect, and strategies must contain at least one strategy", call. = FALSE)
  }
  if (missing(ref_str) || is.null(ref_str)) {
    stop("`ref_str` must name the reference strategy.", call. = FALSE)
  }
  v_strat <- as.character(strategies)
  if (anyDuplicated(v_strat)) {
    stop("Strategy names must be unique.", call. = FALSE)
  }
  ref_str <- check_ref_str(ref_str, v_strat)
  check_wtp_nhb(wtp, nhb)

  # reference first, then the other strategies by increasing cost (and decreasing effect)
  v_order <- order(v_strat != ref_str, cost, -effect)
  df_ref <- data.frame(Strategy = v_strat[v_order], Cost = cost[v_order],
                       Effect = effect[v_order], stringsAsFactors = FALSE)
  is_ref <- df_ref$Strategy == ref_str

  # differences from the reference; its own are reported as NA
  df_ref$Inc_Cost <- df_ref$Cost - df_ref$Cost[is_ref]
  df_ref$Inc_Effect <- df_ref$Effect - df_ref$Effect[is_ref]
  v_status <- status_vs_reference(df_ref$Inc_Cost, df_ref$Inc_Effect)
  v_status[is_ref] <- "Reference"
  df_ref$Inc_Cost[is_ref] <- NA_real_
  df_ref$Inc_Effect[is_ref] <- NA_real_
  # a ratio is only meaningful when cost and effect move in the same direction (NE and SW
  # quadrants); for dominant, dominated, and equivalent strategies the CER stays NA
  v_has_ratio <- v_status %in% c("More costly, more effective", "Less costly, less effective")
  df_ref$CER <- ifelse(v_has_ratio, df_ref$Inc_Cost / df_ref$Inc_Effect, NA_real_)
  df_ref$Status <- v_status

  if (!is.null(wtp)) {
    df_ref$NMB <- wtp * df_ref$Effect - df_ref$Cost
    df_ref$Inc_NMB <- df_ref$NMB - df_ref$NMB[is_ref]
    df_ref$Inc_NMB[is_ref] <- NA_real_
    if (nhb) {
      df_ref$NHB <- df_ref$Effect - df_ref$Cost / wtp
      df_ref$Inc_NHB <- df_ref$NHB - df_ref$NHB[is_ref]
      df_ref$Inc_NHB[is_ref] <- NA_real_
    }
  }

  structure(df_ref, class = c("cers_ref", "data.frame"), ref_str = ref_str, wtp = wtp,
            currency = currency)
}

# nolint end

#' Compare every strategy with a reference strategy, from a \code{psa} object
#'
#' @description
#' Compares each strategy with a reference strategy using the mean costs and effects of a
#' probabilistic sensitivity analysis (PSA), as \code{\link{calculate_cers_ref}} does for a base
#' case. With \code{uncertainty = TRUE}, it also summarizes the uncertainty in each comparison
#' across the PSA samples. See \code{\link{calculate_cers_ref}} for when a comparison with a
#' single reference is appropriate, and \code{\link{calculate_icers_psa}} for the fully
#' incremental analysis.
#'
#' @param psa \code{psa} object from \code{\link{make_psa_obj}}.
#' @param uncertainty if \code{TRUE}, add the 95\% intervals and quadrant proportions described
#' in Value. Defaults to \code{FALSE}.
#' @inheritParams calculate_cers_ref
#'
#' @return A \code{cers_ref} object with the columns described in
#' \code{\link{calculate_cers_ref}}, computed from the mean cost and effect of each strategy. The
#' currency of \code{psa} is kept.
#'
#' With \code{uncertainty = TRUE}, the differences from the reference are computed in every PSA
#' sample, and these columns are added (\code{NA} for the reference itself):
#' \itemize{
#'   \item \code{Lower_95_Inc_Cost} and \code{Upper_95_Inc_Cost}, the 2.5th and 97.5th
#'   percentiles of the difference in cost, placed after \code{Inc_Cost}; likewise for
#'   \code{Inc_Effect} and, with \code{wtp}, \code{Inc_NMB} (and \code{Inc_NHB}).
#'   \item \code{Prop_NE}, \code{Prop_SE}, \code{Prop_SW}, and \code{Prop_NW}, the proportion of
#'   samples in each quadrant of the incremental cost-effectiveness plane: NE more effective and
#'   more costly; SE more effective and not more costly; SW not more effective and not more
#'   costly; NW not more effective and more costly. The four proportions sum to 1.
#'   \item With \code{wtp}, \code{Prop_CE}, the proportion of samples in which the strategy is
#'   cost-effective compared with the reference (it has a higher net monetary benefit).
#' }
#'
#' @inherit calculate_cers_ref references
#' @seealso \code{\link{calculate_cers_ref}}, \code{\link{plot.cers_ref}}, and
#' \code{\link{plot.psa}} with \code{ref_str} for the PSA samples on the incremental plane.
#'
#' @examples
#' data(cancer_psa_data)
#' l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                       effectiveness = cancer_psa_data$effectiveness,
#'                       parameters = cancer_psa_data$parameters,
#'                       strategies = cancer_psa_data$strategies)
#' df_ref <- calculate_cers_ref_psa(l_psa, ref_str = "Radio", wtp = 50000,
#'                                  uncertainty = TRUE)
#' df_ref
#' plot(df_ref)
#' @export
calculate_cers_ref_psa <- function(psa, ref_str, wtp = NULL, nhb = FALSE, uncertainty = FALSE) {
  check_psa_object(psa)
  if (!isTRUE(uncertainty) && !isFALSE(uncertainty)) {
    stop("`uncertainty` must be TRUE or FALSE.", call. = FALSE)
  }
  df_sum <- summary(psa)
  df_ref <- calculate_cers_ref(cost = df_sum$meanCost, effect = df_sum$meanEffect,
                               strategies = df_sum$Strategy, ref_str = ref_str, wtp = wtp,
                               nhb = nhb, currency = psa$currency)
  if (uncertainty) {
    df_ref <- add_cers_ref_uncertainty(df_ref, psa, wtp, nhb)
  }
  df_ref
}

#' Add PSA intervals and quadrant proportions to a \code{cers_ref} object
#'
#' @param df_ref a \code{cers_ref} object computed from the means of \code{psa}.
#' @param psa the \code{psa} object.
#' @inheritParams calculate_cers_ref
#' @return \code{df_ref} with the columns described in \code{\link{calculate_cers_ref_psa}}.
#' @keywords internal
add_cers_ref_uncertainty <- function(df_ref, psa, wtp, nhb) {
  ref_str <- attr(df_ref, "ref_str")
  v_strat <- df_ref$Strategy
  is_ref <- v_strat == ref_str
  # sample-by-sample differences from the reference: one row per PSA sample, one column per
  # strategy in the row order of df_ref (subtracting a vector recycles it down each column)
  m_inc_cost <- as.matrix(psa$cost)[, v_strat, drop = FALSE] - psa$cost[[ref_str]]
  m_inc_effect <- as.matrix(psa$effectiveness)[, v_strat, drop = FALSE] -
    psa$effectiveness[[ref_str]]
  l_inc <- list(Inc_Cost = m_inc_cost, Inc_Effect = m_inc_effect)
  if (!is.null(wtp)) {
    m_inc_nmb <- wtp * m_inc_effect - m_inc_cost
    l_inc$Inc_NMB <- m_inc_nmb
    if (nhb) {
      l_inc$Inc_NHB <- m_inc_effect - m_inc_cost / wtp
    }
  }
  na_ref <- function(v) {
    v <- unname(v)
    v[is_ref] <- NA_real_
    v
  }

  # 95% intervals, each placed right after its point estimate
  v_cols <- names(df_ref)
  for (col in names(l_inc)) {
    df_ref[[paste0("Lower_95_", col)]] <- na_ref(apply(l_inc[[col]], 2, quantile, probs = 0.025,
                                                       names = FALSE))
    df_ref[[paste0("Upper_95_", col)]] <- na_ref(apply(l_inc[[col]], 2, quantile, probs = 0.975,
                                                       names = FALSE))
    v_cols <- append(v_cols, paste0(c("Lower_95_", "Upper_95_"), col), after = match(col, v_cols))
  }

  # share of samples in each quadrant of the incremental plane, and cost-effective ones
  df_ref$Prop_NE <- na_ref(colMeans(m_inc_effect > 0 & m_inc_cost > 0))
  df_ref$Prop_SE <- na_ref(colMeans(m_inc_effect > 0 & m_inc_cost <= 0))
  df_ref$Prop_SW <- na_ref(colMeans(m_inc_effect <= 0 & m_inc_cost <= 0))
  df_ref$Prop_NW <- na_ref(colMeans(m_inc_effect <= 0 & m_inc_cost > 0))
  v_cols <- c(v_cols, "Prop_NE", "Prop_SE", "Prop_SW", "Prop_NW")
  if (!is.null(wtp)) {
    df_ref$Prop_CE <- na_ref(colMeans(m_inc_nmb > 0))
    v_cols <- c(v_cols, "Prop_CE")
  }

  l_attr <- attributes(df_ref)[c("class", "ref_str", "wtp", "currency")]
  df_ref <- as.data.frame(unclass(df_ref)[v_cols], check.names = FALSE, stringsAsFactors = FALSE)
  for (a in names(l_attr)) {
    attr(df_ref, a) <- l_attr[[a]]
  }
  df_ref
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

#' Print a comparison with a reference strategy
#'
#' Prints the table of a \code{\link{calculate_cers_ref}} object with formatted numbers and
#' headers that name the reference strategy.
#'
#' @param x a \code{cers_ref} object.
#' @param currency currency symbol. Defaults to the currency stored in \code{x}, or \code{"$"}.
#' @param dig_cost number of digits to round costs and net monetary benefits to.
#' @param dig_eff number of digits to round effects and net health benefits to.
#' @param dig_cer number of digits to round the cost-effectiveness ratios to.
#' @param dig_prop number of digits to round the proportions of PSA samples to.
#' @param ... further arguments (not used).
#' @return the formatted table, invisibly.
#' @export
print.cers_ref <- function(x, currency = NULL, dig_cost = 0, dig_eff = 2, dig_cer = 0,
                           dig_prop = 3, ...) {
  currency <- resolve_currency(currency, x)
  vs <- paste0(" vs ", attr(x, "ref_str"))
  cat("Comparison of each strategy with the reference strategy, ", attr(x, "ref_str"),
      "\n(not a fully incremental analysis; see calculate_icers())\n\n", sep = "")
  df_table <- as.data.frame(unclass(x), check.names = FALSE, stringsAsFactors = FALSE)
  l_format <- list(
    Cost       = list(dig_cost, paste0("Costs (", currency, ")")),
    Effect     = list(dig_eff, "QALYs"),
    Inc_Cost   = list(dig_cost, paste0("Incremental Costs", vs, " (", currency, ")")),
    Inc_Effect = list(dig_eff, paste0("Incremental QALYs", vs)),
    CER        = list(dig_cer, paste0("CER", vs, " (", currency, "/QALY)")),
    Status     = list(NA, paste0("Status", vs)),
    NMB        = list(dig_cost, paste0("NMB (", currency, ")")),
    Inc_NMB    = list(dig_cost, paste0("Incremental NMB", vs, " (", currency, ")")),
    NHB        = list(dig_eff, "NHB (QALYs)"),
    Inc_NHB    = list(dig_eff, paste0("Incremental NHB", vs, " (QALYs)"))
  )
  # interval columns use the digits of their point estimate; proportions their own
  for (col in grep("^(Lower|Upper)_95_", names(df_table), value = TRUE)) {
    base <- sub("^(Lower|Upper)_95_", "", col)
    l_format[[col]] <- list(l_format[[base]][[1]], col)
  }
  # proportions with a fixed number of decimals, so they line up
  for (col in grep("^Prop_", names(df_table), value = TRUE)) {
    v <- df_table[[col]]
    df_table[[col]] <- ifelse(is.na(v), "-", formatC(v, format = "f", digits = dig_prop))
  }
  df_table <- format_cea_table(df_table, l_format)
  print(df_table)
  invisible(df_table)
}

#' Plot a comparison with a reference strategy
#'
#' Draws the incremental cost-effectiveness plane centered at the reference strategy: each
#' strategy is placed at its difference in effect (x) and cost (y) from the reference, which sits
#' at the origin. Optional segments join the reference to each strategy, forming a star, and an
#' optional willingness-to-pay line goes through the origin: strategies below the line are
#' cost-effective compared with the reference at that threshold.
#'
#' @param x a \code{cers_ref} object from \code{\link{calculate_cers_ref}} or
#' \code{\link{calculate_cers_ref_psa}}.
#' @param wtp a willingness-to-pay threshold for the dotted line through the origin. Defaults to
#' the threshold stored in \code{x}, if any; \code{NULL} draws no line.
#' @param lines whether to draw a segment from the reference to each strategy. The default
#' (\code{NULL}) draws them when there are at most 20 strategies besides the reference, since
#' with many strategies they make the plot hard to read.
#' @param label whether to label all strategies (the default) or none.
#' @param label_max_char maximum number of characters of the strategy labels; longer names are
#' truncated. The default (\code{NULL}) does not truncate.
#' @param ref_axis draw dashed axes through the origin. Defaults to \code{TRUE}.
#' @param ref_caption add a caption naming the reference strategy and the WTP threshold.
#' Defaults to \code{TRUE}.
#' @param currency currency symbol. Defaults to the currency stored in \code{x}, or \code{"$"}.
#' @param effect_units units of effectiveness. Defaults to \code{"QALYs"}.
#' @param col color palette for the status of each strategy: \code{"access"} (the default, an
#' accessible palette), \code{"full"}, or \code{"bw"}.
#' @param alpha opacity of the points.
#' @inheritParams add_common_aes
#' @inheritParams ggrepel::geom_label_repel
#' @param ... further arguments (not used).
#'
#' @return a \code{ggplot2} object, which can be modified by adding further layers.
#' @seealso \code{\link{calculate_cers_ref}}
#' @examples
#' df_ref <- calculate_cers_ref(cost = c(12000, 9000, 20000),
#'                              effect = c(8.0, 7.6, 8.5),
#'                              strategies = c("Standard of care", "Generic drug", "New drug"),
#'                              ref_str = "Standard of care")
#' plot(df_ref, wtp = 50000)
#'
#' # many strategies: no segments, no labels
#' data(hund_strat)
#' df_hund <- calculate_cers_ref(hund_strat$Cost, hund_strat$QALYs, hund_strat$Strategy,
#'                               ref_str = "1")
#' plot(df_hund, label = "none")
#' @importFrom ggrepel geom_label_repel
#' @export
plot.cers_ref <- function(x,
                          wtp = attr(x, "wtp"),
                          lines = NULL,
                          label = c("all", "none"),
                          label_max_char = NULL,
                          ref_axis = TRUE,
                          ref_caption = TRUE,
                          currency = NULL,
                          effect_units = "QALYs",
                          col = c("access", "full", "bw"),
                          alpha = 1,
                          txtsize = 12,
                          n_x_ticks = 6,
                          n_y_ticks = 6,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          xexpand = expansion(0.1),
                          yexpand = expansion(0.1),
                          max.iter = 20000,
                          ...) {
  Inc_Effect <- Inc_Cost <- Status <- Strategy <- NULL
  label <- match.arg(label)
  col <- match.arg(col)
  currency <- resolve_currency(currency, x)
  check_wtp_nhb(wtp, nhb = FALSE)
  ref_str <- attr(x, "ref_str")
  is_ref <- x$Strategy == ref_str
  n_comp <- sum(!is_ref)
  if (is.null(lines)) {
    lines <- n_comp <= 20
  }

  # the reference sits at the origin of the incremental plane
  df_plot <- data.frame(Strategy = x$Strategy, Status = x$Status,
                        Inc_Cost = ifelse(is_ref, 0, x$Inc_Cost),
                        Inc_Effect = ifelse(is_ref, 0, x$Inc_Effect),
                        stringsAsFactors = FALSE)
  if (!is.null(label_max_char)) {
    df_plot$Strategy <- substr(df_plot$Strategy, 1, label_max_char)
  }
  v_levels <- c("Dominant", "More costly, more effective", "Less costly, less effective",
                "Dominated", "Equivalent")
  df_comp <- df_plot[!is_ref, , drop = FALSE]
  # all levels, so each status keeps its color whichever statuses are present
  df_comp$Status <- factor(df_comp$Status, levels = v_levels)
  df_ref <- df_plot[is_ref, , drop = FALSE]

  p <- ggplot(df_comp, aes(x = Inc_Effect, y = Inc_Cost))
  if (ref_axis) {
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
  }
  if (lines && n_comp > 0) {
    p <- p + geom_segment(aes(x = 0, y = 0, xend = Inc_Effect, yend = Inc_Cost),
                          color = "grey60", linewidth = 0.4)
  }
  if (!is.null(wtp)) {
    p <- p + geom_abline(intercept = 0, slope = wtp, linetype = "dotted", color = "grey30")
  }
  p <- p +
    geom_point(aes(color = Status), size = 2.5, alpha = alpha) +
    geom_point(data = df_ref, shape = 23, size = 3.5, fill = "black", color = "black") +
    labs(x = paste0("Incremental Effect (", effect_units, ")"),
         y = paste0("Incremental Cost (", currency, ")"))
  v_caption <- c(paste0("Reference Strategy (diamond): ", ref_str),
                 if (!is.null(wtp)) wtp_caption(wtp, currency, effect_units))
  if (ref_caption) {
    p <- p + labs(caption = paste(v_caption, collapse = "\n"))
  }
  p <- add_common_aes(p, txtsize, col = col, col_aes = "color", scale_name = NULL,
                      continuous = c("x", "y"),
                      n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                      xbreaks = xbreaks, ybreaks = ybreaks,
                      xlim = xlim, ylim = ylim,
                      xexpand = xexpand, yexpand = yexpand,
                      v_str = v_levels)
  if (label == "all") {
    p <- p + geom_label_repel(data = df_plot, aes(label = Strategy), size = 3,
                              show.legend = FALSE, max.iter = max.iter, direction = "both")
  }
  p
}
