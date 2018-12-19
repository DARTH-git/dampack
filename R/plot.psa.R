#' Plot the psa object
#'
#' @param x the psa object
#' @param ... further arguments to plot (not used)
#' @param center plot the mean cost and effectiveness for each strategy. defaults to TRUE
#' @param ellipse plot an ellipse around each strategy. defaults to TRUE
#' @param title title for the plot
#' @param alpha opacity of the scatterplot points.
#' 0 is completely transparent, 1 is completely opaque
#'
#' @importFrom ellipse ellipse
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#' @importFrom scales dollar_format
#' @export
plot.psa <- function(x, ...,
                     center = TRUE, ellipse = TRUE, title="Cost-Effectiveness Scatterplot",
                     alpha = 0.2) {
  effectiveness <- x$effectiveness
  cost <- x$cost
  strategies <- x$strategies
  currency <- x$currency

  # expect that effectiveness and costs have strategy column names
  df.cost <- suppressMessages( # removes confusing 'No id variables; using all as measure variables'
    melt(cost, variable.name = "Strategy",
         factorsAsStrings=TRUE,
         value.name = "Cost")
  )
  df.effect <- suppressMessages(
    melt(effectiveness, variable.name = "Strategy",
         factorsAsStrings=TRUE,
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
      data.frame(els, group=s, stringsAsFactors = FALSE)
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
    scale_x_continuous(breaks = number_ticks(6)) + theme_bw()
}
