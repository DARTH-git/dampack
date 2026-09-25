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
#' @param strategies vector with the names of the strategies. Due to requirements in
#' certain uses of this vector, this function uses \code{\link{make.names}} to modify
#' strategy names as necessary. It is strongly suggested that you follow the rules
#' in the \code{\link{make.names}} help page, to avoid unexpected errors.
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
#' data("example_psa")
#' psa <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
#'                     example_psa$parameters, example_psa$strategies)
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
  parnames <- names(parameters)

  # define psa as a named list
  psa_obj <- create_sa(parameters, parnames, effectiveness, strategies,
                       cost, currency, other_outcome)

  # give classes "psa" and "sa"
  class(psa_obj) <- c("psa", class(psa_obj))
  return(psa_obj)
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
    df <- as.data.frame(obj)
  } else {
    df <- as.data.frame(obj)
  }
  return(df)
}

#' summarize a psa object across all simulations
#'
#' @param object the psa object
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#' @param ... further arguments to summary (not used)
#'
#' @importFrom stats sd
#' @return a \code{data.frame} containing the mean cost and effectiveness for each strategy and, if requested,
#' the standard deviations of the cost and effectiveness for each strategy.
#' @export
summary.psa <- function(object, calc_sds = FALSE, ...) {

  mean_cost <- colMeans(object$cost)
  mean_effect <- colMeans(object$effectiveness)
  strat <- object$strategies
  sum_psa <- data.frame("Strategy" = strat,
                        "meanCost" = mean_cost,
                        "meanEffect" = mean_effect,
                        stringsAsFactors = FALSE)
  if (calc_sds) {
    sd_cost <- apply(object$cost, 2, sd)
    sd_effect <- apply(object$effectiveness, 2, sd)
    sum_psa[, "sdCost"] <- sd_cost
    sum_psa[, "sdEffect"] <- sd_effect
  }
  rownames(sum_psa) <- seq_len(nrow(sum_psa))
  sum_psa
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
#' @param ref_caption Add the reference strategy as a caption. Defaults to TRUE
#' @inheritParams add_common_aes
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import ggplot2
#' @importFrom scales dollar_format
#' @return A \code{ggplot2} plot of the PSA, showing the distribution of each PSA sample and strategy
#' on the cost-effectiveness plane.
#' @importFrom tidyr pivot_longer
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
                     ...) {

  Effectiveness <- Cost <- Strategy <- Eff.mean <- Cost.mean <- y <- group <- NULL
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency

  if (is.null(ref_str)) {
    # expect that effectiveness and costs have strategy column names
    # removes confusing 'No id variables; using all as measure variables'
    df_cost <- suppressMessages(
      pivot_longer(cost,
                   everything(),
                   names_to = "Strategy",
                   values_to = "Cost")
    )
    df_effect <- suppressMessages(
      pivot_longer(effectiveness,
                   cols = everything(),
                   names_to = "Strategy",
                   values_to = "Effectiveness")
    )
    df_ce <- data.frame("Strategy" = df_cost$Strategy,
                        "Cost" = df_cost$Cost,
                        "Effectiveness" = df_effect$Effectiveness)

    # make strategies in psa object into ordered factors
    df_ce$Strategy <- factor(df_ce$Strategy, levels = strategies, ordered = TRUE)

    psa_plot <- ggplot(df_ce, aes(x = Effectiveness, y = Cost, color = Strategy)) +
      geom_point(size = 0.7, alpha = alpha, shape = 21) +
      ylab(paste("Cost (", currency, ")", sep = ""))

    # define strategy-specific means for the center of the ellipse
    if (center) {
      strat_means <- df_ce %>%
        group_by(Strategy) %>%
        summarize(Cost.mean = mean(Cost),
                  Eff.mean = mean(Effectiveness))
      # make strategies in psa object into ordered factors
      strat_means$Strategy <- factor(strat_means$Strategy, levels = strategies, ordered = TRUE)
      psa_plot <- psa_plot +
        geom_point(data = strat_means,
                   aes(x = Eff.mean, y = Cost.mean, fill = Strategy),
                   size = 8, shape = 21, color = "black")
    }

    if (ellipse) {
      # make points for ellipse plotting
      df_list_ell <- lapply(strategies, function(s) {
        df_strat_specific <- df_ce[df_ce$Strategy == s, ]
        els <-  with(df_strat_specific,
                     ellipse(cor(Effectiveness, Cost),
                             scale = c(sd(Effectiveness), sd(Cost)),
                             centre = c(mean(Effectiveness), mean(Cost))))
        data.frame(els, group = s, stringsAsFactors = FALSE)
      })
      df_ell <- bind_rows(df_list_ell)
      # draw ellipse lines
      psa_plot <- psa_plot + geom_path(data = df_ell,
                                       aes(x = x, y = y, colour = group),
                                       linewidth = 1, linetype = 2, alpha = 1)
    }

    # add common theme
    col <- match.arg(col)
    add_common_aes(psa_plot, txtsize, col = col, col_aes = c("color", "fill"),
                   continuous = c("x", "y"),
                   n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                   xbreaks = xbreaks, ybreaks = ybreaks,
                   xlim = xlim, ylim = ylim)

  } else {
    ### Check that the reference strategy is in the list of strategies
    if (!(ref_str %in% strategies)) {
      stop("ref_str must be in the list of strategies")
    }
    ## Function to convert the raw outcomes to incremental outcomes
    make_df_inc <- function(df) {
      v_ref    <- df[, ref_str]
      df_inc   <- df[, !(names(df) %in% ref_str)]
      df_inc   <- sweep(df_inc, 1, v_ref, "-")
      return(df_inc)
    }

    ## Long data frames
    df_inc_cost <- suppressMessages(
      make_df_inc(cost) %>%
        pivot_longer(cols = everything(),
                     names_to = "Strategy",
                     values_to = "Cost")
    )
    df_inc_effect <- suppressMessages(
      make_df_inc(effectiveness) %>%
        pivot_longer(cols = everything(),
                     names_to = "Strategy",
                     values_to = "Effectiveness")
    )
    df_ice <- data.frame("Strategy" = df_inc_cost$Strategy,
                         "Cost" = df_inc_cost$Cost,
                         "Effectiveness" = df_inc_effect$Effectiveness)

    # make strategies in psa object into ordered factors
    v_comp_strategies <- strategies[strategies != ref_str]

    df_ice$Strategy <- factor(df_ice$Strategy, levels = v_comp_strategies, ordered = TRUE)

    psa_plot <- ggplot(df_ice, aes(x = Effectiveness, y = Cost, color = Strategy)) +
      geom_point(size = 0.7, alpha = alpha, shape = 21) +
      ylab(paste("Incremental Cost (", currency, ")", sep = "")) +
      xlab("Incremental Effectiveness")

    if (ref_axis == TRUE) {
      psa_plot <- psa_plot +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "black")
    }

    if (ref_caption == TRUE) {
      psa_plot <- psa_plot +
        labs(caption = paste("Reference Strategy: ", ref_str))
    }

    # define strategy-specific means for the center of the ellipse
    if (center) {
      strat_means <- df_ice %>%
        group_by(Strategy) %>%
        summarize(Cost.mean = mean(Cost),
                  Eff.mean = mean(Effectiveness))
      # make strategies in psa object into ordered factors
      strat_means$Strategy <- factor(strat_means$Strategy, levels = v_comp_strategies, ordered = TRUE)
      psa_plot <- psa_plot +
        geom_point(data = strat_means,
                   aes(x = Eff.mean, y = Cost.mean, fill = Strategy),
                   size = 8, shape = 21, color = "black")
    }

    if (ellipse) {
      # make points for ellipse plotting
      df_list_ell <- lapply(v_comp_strategies, function(s) {
        df_strat_specific <- df_ice[df_ice$Strategy == s, ]
        els <-  with(df_strat_specific,
                     ellipse(cor(Effectiveness, Cost),
                             scale = c(sd(Effectiveness), sd(Cost)),
                             centre = c(mean(Effectiveness), mean(Cost))))
        data.frame(els, group = s, stringsAsFactors = FALSE)
      })
      df_ell <- bind_rows(df_list_ell)
      # draw ellipse lines
      psa_plot <- psa_plot + geom_path(data = df_ell,
                                       aes(x = x, y = y, colour = group),
                                       linewidth = 1, linetype = 2, alpha = 1)
    }

    # add common theme
    col <- match.arg(col)
    add_common_aes(psa_plot, txtsize, col = col, col_aes = c("color", "fill"),
                   continuous = c("x", "y"),
                   n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                   xbreaks = xbreaks, ybreaks = ybreaks,
                   xlim = xlim, ylim = ylim,
                   v_str = strategies)

  }
}
