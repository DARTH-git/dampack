#' Create a PSA object
#'
#' @description
#' Creates an object to hold probabilistic sensivity analysis data,
#' while checking the data for validity. The object can then be
#' used for many standard cost-effectiveness analyses (see Details below).
#'
#' @param parameters Data frame with values for each simulation (rows) and parameter (columns).
#' The column names should be the parameter names.
#' @inheritParams create_sa
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
make_psa_obj <- function(cost, effectiveness, parameters,
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
    df <- obj
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
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#' @inheritParams add_common_aes
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @importFrom scales dollar_format
#' @export
plot.psa <- function(x,
                     center = TRUE, ellipse = TRUE,
                     alpha = 0.2, txtsize = 12, col = c("full", "bw"),
                     n_x_ticks = 6, n_y_ticks = 6,
                     xbreaks = NULL,
                     ybreaks = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     ...) {
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency

  # expect that effectiveness and costs have strategy column names
  # removes confusing 'No id variables; using all as measure variables'
  df_cost <- suppressMessages(
    melt(cost, variable.name = "Strategy",
         factorsAsStrings = TRUE,
         value.name = "Cost")
  )
  df_effect <- suppressMessages(
    melt(effectiveness, variable.name = "Strategy",
         factorsAsStrings = TRUE,
         value.name = "Effectiveness")
  )
  ce_df <- data.frame("Strategy" = df_cost$Strategy,
                      "Cost" = df_cost$Cost,
                      "Effectiveness" = df_effect$Effectiveness)

  psa_plot <- ggplot(ce_df, aes_string(x = "Effectiveness", y = "Cost", color = "Strategy")) +
    geom_point(size = 0.7, alpha = alpha, shape = 21) +
    ylab(paste("Cost (", currency, ")", sep = ""))

  # define strategy-specific means for the center of the ellipse
  if (center) {
    strat_means <- ce_df %>%
      group_by(.data$Strategy) %>%
      summarize(Cost.mean = mean(.data$Cost),
                Eff.mean = mean(.data$Effectiveness))
    psa_plot <- psa_plot +
      geom_point(data = strat_means,
                 aes_string(x = "Eff.mean", y = "Cost.mean", fill = "Strategy"),
                 size = 8, shape = 21, color = "black")
  }

  if (ellipse) {
    # make points for ellipse plotting
    df_list_ell <- lapply(strategies, function(s) {
      strat_specific_df <- ce_df[ce_df$Strategy == s, ]
      els <-  with(strat_specific_df,
                   ellipse(cor(Effectiveness, Cost),
                           scale = c(sd(Effectiveness), sd(Cost)),
                           centre = c(mean(Effectiveness), mean(Cost))))
      data.frame(els, group = s, stringsAsFactors = FALSE)
    })
    df_ell <- bind_rows(df_list_ell)
    # draw ellipse lines
    psa_plot <- psa_plot + geom_path(data = df_ell,
                                     aes_string(x = "x", y = "y", colour = "group"),
                                     size = 1, linetype = 2, alpha = 1)
  }

  # add common theme
  col <- match.arg(col)
  add_common_aes(psa_plot, txtsize, col = col, col_aes = c("color", "fill"),
                 continuous = c("x", "y"),
                 n_x_ticks = n_x_ticks, n_y_ticks = n_y_ticks,
                 xbreaks = xbreaks, ybreaks = ybreaks,
                 xlim = xlim, ylim = ylim)
}
