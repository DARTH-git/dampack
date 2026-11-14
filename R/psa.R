#' Create a PSA object
#'
#' @description
#' Creates an object to hold probabilistic sensitivity analysis data,
#' while checking the data for validity. The object can then be
#' used for many standard cost-effectiveness analyses (see Details below).
#'
#' @param parameters Data frame with values for each simulation (rows) and parameter (columns).
#' The column names should be the parameter names.
#' @param cost For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param effectiveness For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param other_outcome data.frame containing values for another user-defined outcome.
#' Each simulation should be a row of the data frame, and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param strategies vector with the names of the strategies, in the same order as
#' the columns of the outcome data frames. Names are kept exactly as provided, so
#' they can contain spaces and other characters (for example "Standard of care"),
#' and are used as labels in tables and plots. They must be unique and not missing
#' or empty. If \code{NULL}, strategies are named \code{Strategy_1},
#' \code{Strategy_2}, and so on.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
#' @details
#' The PSA object forms the backbone of one part of the \code{dampack} package.
#'
#' A scatterplot of the cost-effectiveness plane may be shown by running \code{plot}
#' on the output of \code{make_psa_obj}.
#'
#' Using this object, you may calculate:
#' \itemize{
#'   \item Cost-effectiveness acceptability curves (\code{\link{ceac}})
#'   \item Expected value of perfect information (\code{\link{calc_evpi}})
#'   \item Expected loss (\code{\link{calc_exp_loss}})
#'   \item One-way sensitivity analysis (\code{\link{owsa}})
#'   \item Two-way sensitivity analysis (\code{\link{twsa}})
#'   \item Metamodels (\code{\link{metamodel}})
#' }
#'
#' In addition, the PSA may be converted to a base-case analysis by using \code{summary}
#' on the PSA object. The output of \code{summary} can be used in \code{\link{calculate_icers}}.
#'
#'
#' @return An object of class \code{psa}
#'
#' @seealso \code{\link{summary.psa}}, \code{\link{plot.psa}}
#'
#' @examples
#' # psa input provided with package
#' data("cancer_psa_data")
#' psa <- make_psa_obj(cost = cancer_psa_data$cost,
#'                     effectiveness = cancer_psa_data$effectiveness,
#'                     parameters = cancer_psa_data$parameters,
#'                     strategies = cancer_psa_data$strategies)
#'
#' # custom print and summary methods
#' print(psa)
#' summary(psa)
#'
#' # custom plot method; see ?plot.psa for options
#' plot(psa)
#'
#' @importFrom stringr str_replace
#' @export
make_psa_obj <- function(cost, effectiveness, parameters = NULL,
                         strategies = NULL, currency = "$", other_outcome = NULL) {

  # parameter names
  v_parnames <- names(parameters)

  # define psa as a named list
  psa_obj <- create_sa(parameters, v_parnames, effectiveness, strategies,
                       cost, currency, other_outcome)

  # give classes "psa" and "sa"
  class(psa_obj) <- c("psa", class(psa_obj))
  psa_obj
}

check_psa_object <- function(psa) {
  if (!inherits(psa, "psa")) {
    stop(paste0("The psa results parameter must be an object of class `psa`.\n",
                "Please run the make_psa() function to create this object."))
  }
}

check_df_and_coerce <- function(obj) {
  obj_name <- deparse(substitute(obj))
  if (!inherits(obj, "data.frame")) {
    warning(paste0("\'", obj_name, "\'", " is not a data frame. coercing to data frame"))
    df_obj <- as.data.frame(obj)
  } else {
    df_obj <- as.data.frame(obj)
  }
  df_obj
}

#' summarize a psa object across all simulations
#'
#' @param object the psa object
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#' @param ref_str name of a reference strategy. If given, each strategy is compared with it
#' sample by sample (see Value). Defaults to \code{NULL}.
#' @param wtp a single willingness-to-pay threshold. If given, the mean net monetary benefit is
#' added (see Value). Defaults to \code{NULL}.
#' @param ... further arguments to summary (not used)
#'
#' @importFrom stats sd
#' @return a \code{data.frame} containing the mean cost and effectiveness for each strategy and, if requested,
#' the standard deviations of the cost and effectiveness for each strategy.
#'
#' With \code{ref_str}, the differences between each strategy and the reference are computed in
#' every PSA sample, and these columns are added (\code{NA} for the reference itself):
#' \code{meanIncCost} and \code{meanIncEffect}, the mean differences in cost and effect; and
#' \code{propNE}, \code{propSE}, \code{propSW}, and \code{propNW}, the proportion of samples in each
#' quadrant of the incremental cost-effectiveness plane: NE more effective and more costly, SE more
#' effective and not more costly (dominant), SW not more effective and not more costly, and NW not more
#' effective and more costly (dominated). The four proportions sum to 1.
#'
#' With \code{wtp}, \code{meanNMB} (\code{wtp * effect - cost}, averaged over samples) is added; with
#' \code{ref_str} too, also \code{meanIncNMB} and \code{propCostEffective}, the proportion of samples in
#' which the strategy has a higher NMB than the reference (it lies below the willingness-to-pay line).
#' @export
summary.psa <- function(object, calc_sds = FALSE, ref_str = NULL, wtp = NULL, ...) {
  ref_str <- check_ref_str(ref_str, object$strategies)
  check_wtp_nhb(wtp, nhb = FALSE)

  v_mean_cost <- colMeans(object$cost)
  v_mean_effect <- colMeans(object$effectiveness)
  v_strat <- object$strategies
  df_sum_psa <- data.frame("Strategy" = v_strat,
                           "meanCost" = v_mean_cost,
                           "meanEffect" = v_mean_effect,
                           stringsAsFactors = FALSE)
  if (calc_sds) {
    v_sd_cost <- apply(object$cost, 2, sd)
    v_sd_effect <- apply(object$effectiveness, 2, sd)
    df_sum_psa[, "sdCost"] <- v_sd_cost
    df_sum_psa[, "sdEffect"] <- v_sd_effect
  }
  if (!is.null(wtp)) {
    df_sum_psa[, "meanNMB"] <- unname(colMeans(wtp * object$effectiveness - object$cost))
  }
  if (!is.null(ref_str)) {
    # sample-by-sample differences from the reference strategy
    m_inc_cost <- as.matrix(object$cost) - object$cost[[ref_str]]
    m_inc_effect <- as.matrix(object$effectiveness) - object$effectiveness[[ref_str]]
    v_is_ref <- v_strat == ref_str
    na_ref <- function(v) {
      v[v_is_ref] <- NA_real_
      unname(v)
    }
    df_sum_psa[, "meanIncCost"] <- na_ref(colMeans(m_inc_cost))
    df_sum_psa[, "meanIncEffect"] <- na_ref(colMeans(m_inc_effect))
    df_sum_psa[, "propNE"] <- na_ref(colMeans(m_inc_effect > 0 & m_inc_cost > 0))
    df_sum_psa[, "propSE"] <- na_ref(colMeans(m_inc_effect > 0 & m_inc_cost <= 0))
    df_sum_psa[, "propSW"] <- na_ref(colMeans(m_inc_effect <= 0 & m_inc_cost <= 0))
    df_sum_psa[, "propNW"] <- na_ref(colMeans(m_inc_effect <= 0 & m_inc_cost > 0))
    if (!is.null(wtp)) {
      m_inc_nmb <- wtp * m_inc_effect - m_inc_cost
      df_sum_psa[, "meanIncNMB"] <- na_ref(colMeans(m_inc_nmb))
      df_sum_psa[, "propCostEffective"] <- na_ref(colMeans(m_inc_nmb > 0))
    }
  }
  rownames(df_sum_psa) <- seq_len(nrow(df_sum_psa))
  df_sum_psa
}

#' Plot the psa object
#'
#' @param x the psa object
#' @param ref_str select a reference strategy. If the user selects a reference
#' strategy, the cost-effectiveness plane becomes an incremental cost-effectiveness plane
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#' @param ref_axis Add dotted axes lines at (0,0). Defaults to TRUE
#' @param ref_caption Add the reference strategy (and the WTP threshold, if given) as a caption.
#' Defaults to TRUE
#' @param wtp a willingness-to-pay threshold. If given, a dotted line with slope \code{wtp} is
#' drawn through the reference strategy, which with \code{ref_str} is the origin of the incremental
#' cost-effectiveness plane. Without \code{ref_str}, the line goes through the mean cost and effect
#' of the least costly strategy, as in \code{\link{plot.icers}}. Samples below the line are
#' cost-effective compared with that strategy at this threshold.
#' @param xtrans,ytrans transformation of the effectiveness (x) and cost (y) axes, such as
#' \code{"log10"}, passed to \code{\link[ggplot2]{scale_continuous}}. Defaults to
#' \code{"identity"}. The ellipses are computed on the transformed scale, so they keep their
#' shape on a log axis; the center points stay at the arithmetic means. A log axis needs every
#' value on it to be positive, which on the incremental plane (\code{ref_str}) is rarely the case.
#' @inheritParams add_common_aes
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import ggplot2
#' @importFrom scales dollar_format
#' @return A \code{ggplot2} plot of the PSA, showing the distribution of each PSA sample and strategy
#' on the cost-effectiveness plane.
#' @importFrom tidyr pivot_longer
#' @importFrom stats cor
#' @export
plot.psa <- function(x,
                     ref_str = NULL,
                     center = TRUE, ellipse = TRUE,
                     alpha = 0.2, txtsize = 12, col = c("access", "full", "bw"),
                     n_x_ticks = 6, n_y_ticks = 6,
                     xbreaks = NULL,
                     ybreaks = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     ref_axis = TRUE,
                     ref_caption = TRUE,
                     wtp = NULL,
                     xtrans = "identity",
                     ytrans = "identity",
                     ...) {

  Effectiveness <- Cost <- Strategy <- Eff.mean <- Cost.mean <- y <- group <- NULL
  df_effectiveness_wide <- x$effectiveness
  df_cost_wide <- x$cost
  v_strategies <- x$strategies
  currency <- x$currency
  ref_str <- check_ref_str(ref_str, v_strategies)
  check_wtp_nhb(wtp, nhb = FALSE)

  if (is.null(ref_str)) {
    # expect that effectiveness and costs have strategy column names
    # removes confusing 'No id variables; using all as measure variables'
    df_cost <- suppressMessages(
      pivot_longer(df_cost_wide,
                   everything(),
                   names_to = "Strategy",
                   values_to = "Cost")
    )
    df_effect <- suppressMessages(
      pivot_longer(df_effectiveness_wide,
                   cols = everything(),
                   names_to = "Strategy",
                   values_to = "Effectiveness")
    )
    df_ce <- data.frame("Strategy" = df_cost$Strategy,
                        "Cost" = df_cost$Cost,
                        "Effectiveness" = df_effect$Effectiveness)

    # make strategies in psa object into ordered factors
    df_ce$Strategy <- factor(df_ce$Strategy, levels = v_strategies, ordered = TRUE)

    check_axis_transform(df_ce, xtrans, ytrans)
    psa_plot <- ggplot(df_ce, aes(x = Effectiveness, y = Cost, color = Strategy)) +
      geom_point(size = 0.7, alpha = alpha, shape = 21) +
      ylab(paste("Cost (", currency, ")", sep = ""))

    # define strategy-specific means for the center of the ellipse
    if (center) {
      df_strat_means <- df_ce %>%
        group_by(Strategy) %>%
        summarize(Cost.mean = mean(Cost),
                  Eff.mean = mean(Effectiveness))
      # make strategies in psa object into ordered factors
      df_strat_means$Strategy <- factor(df_strat_means$Strategy, levels = v_strategies, ordered = TRUE)
      psa_plot <- psa_plot +
        geom_point(data = df_strat_means,
                   aes(x = Eff.mean, y = Cost.mean, fill = Strategy),
                   size = 8, shape = 21, color = "black")
    }

    if (ellipse) {
      # make points for ellipse plotting
      df_ell <- psa_ellipses(df_ce, v_strategies, xtrans, ytrans)
      # draw ellipse lines
      psa_plot <- psa_plot + geom_path(data = df_ell,
                                       aes(x = x, y = y, colour = group),
                                       linewidth = 1, linetype = 2, alpha = 1)
    }

    # WTP line through the mean of the least costly strategy, matching plot.icers()
    if (!is.null(wtp)) {
      v_mean_cost <- colMeans(df_cost_wide)
      i_anchor <- which.min(v_mean_cost)
      intercept <- v_mean_cost[[i_anchor]] - wtp * mean(df_effectiveness_wide[[i_anchor]])
      psa_plot <- psa_plot +
        geom_abline(intercept = intercept, slope = wtp, linetype = "dotted", color = "grey30")
      if (ref_caption) {
        psa_plot <- psa_plot + labs(caption = wtp_caption(wtp, currency))
      }
    }

    # add common theme
    col <- match.arg(col)
    # only the center points use fill; a fill scale without them makes ggplot2 warn
    add_common_aes(psa_plot, txtsize, col = col,
                   col_aes = if (center) c("color", "fill") else "color",
                   continuous = c("x", "y"),
                   n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                   xbreaks = xbreaks, ybreaks = ybreaks,
                   xlim = xlim, ylim = ylim,
                   xtrans = xtrans, ytrans = ytrans,
                   v_str = v_strategies)

  } else {
    ## Function to convert the raw outcomes to incremental outcomes
    make_df_inc <- function(df) {
      v_ref    <- df[, ref_str]
      # drop = FALSE keeps a data frame when there is a single comparator
      df_inc   <- df[, !(names(df) %in% ref_str), drop = FALSE]
      df_inc   <- sweep(df_inc, 1, v_ref, "-")
      df_inc
    }

    ## Long data frames
    df_inc_cost <- suppressMessages(
      make_df_inc(df_cost_wide) %>%
        pivot_longer(cols = everything(),
                     names_to = "Strategy",
                     values_to = "Cost")
    )
    df_inc_effect <- suppressMessages(
      make_df_inc(df_effectiveness_wide) %>%
        pivot_longer(cols = everything(),
                     names_to = "Strategy",
                     values_to = "Effectiveness")
    )
    df_ice <- data.frame("Strategy" = df_inc_cost$Strategy,
                         "Cost" = df_inc_cost$Cost,
                         "Effectiveness" = df_inc_effect$Effectiveness)

    # make strategies in psa object into ordered factors
    v_comp_strategies <- v_strategies[v_strategies != ref_str]

    df_ice$Strategy <- factor(df_ice$Strategy, levels = v_comp_strategies, ordered = TRUE)

    check_axis_transform(df_ice, xtrans, ytrans, incremental = TRUE)
    psa_plot <- ggplot(df_ice, aes(x = Effectiveness, y = Cost, color = Strategy)) +
      geom_point(size = 0.7, alpha = alpha, shape = 21) +
      ylab(paste("Incremental Cost (", currency, ")", sep = "")) +
      xlab("Incremental Effectiveness")

    if (ref_axis == TRUE) {
      psa_plot <- psa_plot +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black")
    }

    if (!is.null(wtp)) {
      psa_plot <- psa_plot +
        geom_abline(intercept = 0, slope = wtp, linetype = "dotted", color = "grey30")
    }

    if (ref_caption == TRUE) {
      v_caption <- c(paste0("Reference Strategy: ", ref_str),
                     if (!is.null(wtp)) wtp_caption(wtp, currency))
      psa_plot <- psa_plot +
        labs(caption = paste(v_caption, collapse = "; "))
    }

    # define strategy-specific means for the center of the ellipse
    if (center) {
      df_strat_means <- df_ice %>%
        group_by(Strategy) %>%
        summarize(Cost.mean = mean(Cost),
                  Eff.mean = mean(Effectiveness))
      # make strategies in psa object into ordered factors
      df_strat_means$Strategy <- factor(df_strat_means$Strategy, levels = v_comp_strategies, ordered = TRUE)
      psa_plot <- psa_plot +
        geom_point(data = df_strat_means,
                   aes(x = Eff.mean, y = Cost.mean, fill = Strategy),
                   size = 8, shape = 21, color = "black")
    }

    if (ellipse) {
      # make points for ellipse plotting
      df_ell <- psa_ellipses(df_ice, v_comp_strategies, xtrans, ytrans)
      # draw ellipse lines
      psa_plot <- psa_plot + geom_path(data = df_ell,
                                       aes(x = x, y = y, colour = group),
                                       linewidth = 1, linetype = 2, alpha = 1)
    }

    # add common theme
    col <- match.arg(col)
    # only the center points use fill; a fill scale without them makes ggplot2 warn
    add_common_aes(psa_plot, txtsize, col = col,
                   col_aes = if (center) c("color", "fill") else "color",
                   continuous = c("x", "y"),
                   n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                   xbreaks = xbreaks, ybreaks = ybreaks,
                   xlim = xlim, ylim = ylim,
                   xtrans = xtrans, ytrans = ytrans,
                   v_str = v_strategies)

  }
}

#' Ellipses around each strategy's PSA samples
#'
#' Each ellipse is computed from the mean, standard deviation and correlation of the samples on
#' the transformed scale of the axes and mapped back, so that it keeps its shape when the axes
#' are, for example, on a log scale. With identity transformations this is the usual ellipse.
#'
#' @param df_samples data frame with columns \code{Strategy}, \code{Effectiveness} and \code{Cost}.
#' @param v_str strategies to draw an ellipse for.
#' @inheritParams plot.psa
#' @return a data frame with columns \code{x}, \code{y} and \code{group}.
#' @keywords internal
psa_ellipses <- function(df_samples, v_str, xtrans, ytrans) {
  tr_x <- scales::as.transform(xtrans)
  tr_y <- scales::as.transform(ytrans)
  l_ell <- lapply(v_str, function(s) {
    v_sel <- df_samples$Strategy == s
    # samples on the scale of the axes
    v_x <- tr_x$transform(df_samples$Effectiveness[v_sel])
    v_y <- tr_y$transform(df_samples$Cost[v_sel])
    m_ell <- ellipse(cor(v_x, v_y), scale = c(sd(v_x), sd(v_y)), centre = c(mean(v_x), mean(v_y)))
    # back to the data scale, where ggplot2 applies the axis transformation again
    data.frame(x = tr_x$inverse(m_ell[, 1]), y = tr_y$inverse(m_ell[, 2]),
               group = s, stringsAsFactors = FALSE)
  })
  bind_rows(l_ell)
}

#' Check that the PSA samples can be shown on transformed axes
#'
#' @param df_samples data frame with columns \code{Effectiveness} and \code{Cost}.
#' @param incremental whether the samples are differences from a reference strategy.
#' @inheritParams plot.psa
#' @return \code{NULL}, invisibly; called for the error.
#' @keywords internal
check_axis_transform <- function(df_samples, xtrans, ytrans, incremental = FALSE) {
  l_axes <- list(list(xtrans, df_samples$Effectiveness, "xtrans", "effectiveness"),
                 list(ytrans, df_samples$Cost, "ytrans", "cost"))
  for (axis in l_axes) {
    tr <- scales::as.transform(axis[[1]])
    if (!all(is.finite(suppressWarnings(tr$transform(axis[[2]]))))) {
      tr_name <- if (is.character(axis[[1]])) axis[[1]] else tr$name
      stop("`", axis[[3]], " = \"", tr_name, "\"` cannot show every ",
           if (incremental) "incremental ", axis[[4]], " value: some are outside its domain ",
           "(zero or negative, for a log scale)",
           if (incremental) ". Differences from the reference strategy are often negative",
           ".", call. = FALSE)
    }
  }
  invisible(NULL)
}
