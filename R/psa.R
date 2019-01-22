#' An object to hold PSA results.
#'
#' @param cost Matrix with the cost for each simulation (rows) and strategy (columns).
#' @param effectiveness Matrix with the effectiveness for each simulation (rows) and strategy (columns)
#' @param parameters Data frame with values for each simulation (rows) and parameter (columns).
#' The column names should be the parameter names
#' @param strategies String vector with the name of the strategies
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
#' @export
make_psa_obj <- function(cost, effectiveness, parameters, strategies=NULL, currency = "$"){
  # argument checking
  cost <- check_df_and_coerce(cost)
  effectiveness <- check_df_and_coerce(effectiveness)
  parameters <- check_df_and_coerce(parameters)

  # parameter names
  parnames <- colnames(parameters)

  # argument checks and defining other variables
  # costs, effectiveness, and parameters have same number of rows
  n_sim_costs <- nrow(cost)
  n_sim_effectiveness <- nrow(effectiveness)
  n_sim_parameters <- nrow(parameters)
  if ((n_sim_costs != n_sim_effectiveness) | (n_sim_parameters != n_sim_costs)) {
    stop("The cost, effectiveness, and parameter dataframes must all have the same number of rows.")
  }

  # define n_sim (could be any of the three, since they're all equal)
  n_sim <- n_sim_costs

  # costs and effectiveness have same number of columns
  n_strategies_costs <- ncol(cost)
  n_strategies_effectiveness <- ncol(effectiveness)
  if (n_strategies_costs != n_strategies_effectiveness) {
    stop("The number of columns of the cost and benefit matrices is different and must be the same.")
  }
  # define n.strat (could be either n_sim_costs or n_sim_effectiveness)
  n_strategies <- n_strategies_costs

  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)) {
    strategies <- paste(rep("Strategy_", n_strategies), seq(1, n_strategies), sep = "")
  } else {
    # make sure strategies is the same length as the number of columns
    if (n_strategies != length(strategies)) {
      stop(
        paste0("The number of columns in the cost and effectiveness",
               "matrices is different from the number of strategies provided"))
    }
  }
  # define cost and effectiveness column names using strategies
  names(cost) <- names(effectiveness) <- strategies
  # define psa as a named list
  psa_obj <- list("n_strategies" = n_strategies,
                  "strategies" = strategies,
                  "n_sim" = n_sim,
                  "cost" = cost,
                  "effectiveness" = effectiveness,
                  "parameters" = parameters,
                  "parnames" = parnames,
                  "currency" = currency)
  class(psa_obj) <- "psa"
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
#' @param ... further arguments to summary (not used)
#' @param calc_sds whether or not to calculate the standard deviations. Defaults to FALSE
#'
#' @importFrom stats sd
#' @export
summary.psa <- function(object, ..., calc_sds = FALSE) {
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
  rownames(sum_psa) <- 1:nrow(sum_psa)
  sum_psa
}

#' Plot the psa object
#'
#' @param x the psa object
#' @param ... further arguments to plot (not used)
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param title title for the plot
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#' @param txtsize base textsize
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @importFrom scales dollar_format
#' @export
plot.psa <- function(x, ...,
                     center = TRUE, ellipse = TRUE, title="Cost-Effectiveness Scatterplot",
                     alpha = 0.2, txtsize=12) {
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency

  # expect that effectiveness and costs have strategy column names
  df.cost <- suppressMessages( # removes confusing 'No id variables; using all as measure variables'
    melt(cost, variable.name = "Strategy",
         factorsAsStrings = TRUE,
         value.name = "Cost")
  )
  df.effect <- suppressMessages(
    melt(effectiveness, variable.name = "Strategy",
         factorsAsStrings = TRUE,
         value.name = "Effectiveness")
  )
  ce_df <- data.frame("Strategy" = df.cost$Strategy,
                      "Cost" = df.cost$Cost,
                      "Effectiveness" = df.effect$Effectiveness)

  psa_plot <- ggplot(ce_df, aes_string(x = "Effectiveness", y = "Cost", color = "Strategy")) +
    geom_point(size = 0.7, alpha = alpha, shape = 21)

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
    df_list_ell <- lapply(strategies, function (s) {
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

  # these layers are used for all psa plots
  psa_plot + ggtitle(title) +
    scale_colour_discrete(l = 50) +  # Use a slightly darker palette than normal
    scale_fill_discrete(l = 50) +
    scale_y_continuous(labels = dollar_format(prefix = currency)) +
    scale_x_continuous(breaks = number_ticks(6)) + common_theme(txtsize)
}

#' print a psa object
#'
#' @param x the psa object
#' @param ... further arguments to print (not used)
#' @param all_strat whether or not to print the full list of strategies. defaults to FALSE, which truncates
#' the strategy list to 5
#'
#' @export
print.psa <- function(x, ..., all_strat = FALSE) {
  cat("\n")
  cat("PSA object", "\n")
  cat("-------------------------------------------------", "\n")

  # cost
  cat("number of strategies (n_strategies):", x$n_strategies, "\n")
  n_trunc <- 5
  if (all_strat | (x$n_strategies <= n_trunc)) {
    s2print <- x$strategies
    msg <- ""
  } else {
    s2print <- c(x$strategies[1:n_trunc], "...")
    msg <- paste("(truncated at", n_trunc, ")")
  }
  s_collapsed <- paste(s2print, collapse = ", ")
  cat("strategies:", s_collapsed, msg, "\n")
  cat("number of simulations (n_sim):", x$n_sim, "\n")
  cat("cost: a data frame with", nrow(x$cost), "rows and", ncol(x$cost), "columns.", "\n")
  cat("effectiveness: a data frame with",
      nrow(x$effectiveness), "rows and",
      ncol(x$effectiveness), "columns.", "\n")
  cat("parameters: a data frame with",
      nrow(x$parameters), "rows and",
      ncol(x$parameters), "columns", "\n")
  cat("parameter names (parnames): ", paste(x$parnames, collapse = ", "), "\n")
  cat("currency:", x$currency, "\n")
}
