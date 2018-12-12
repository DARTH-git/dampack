#' @importFrom ellipse ellipse
#' @import plyr
#' @export
plot.psa <- function(psa) {
  strategies <- psa$strategies
  n.strategies <- psa$n.strategies
  effectiveness <- psa$effectiveness
  cost <- psa$cost
  n.sim <- psa$n.sim

  # expect that effectiveness and costs have strategy column names
  df.cost  <- melt(cost, variable.name = "Strategy")
  levels(df.cost$Strategy) <- strategies
  df.effect <- melt(effectiveness, variable.name = "Strategy")
  levels(df.effect$Strategy) <- strategies
  CE <- cbind(df.cost, df.effect[, 2])
  colnames(CE) <- c("Strategy", "Cost", "Effectiveness")

  # Ellipses code
  df_ell <- data.frame() #create an empty dataframe
  # for each level in df$groups
  for(g in levels(CE$Strategy)) {
    # create 100 points per variable around the mean of each group
    df_ell <- rbind(df_ell,
                    cbind(as.data.frame(with(CE[CE$Strategy == g,],
                                             ellipse(cor(Effectiveness, Cost),
                                                     scale = c(sd(Effectiveness), sd(Cost)),
                                                     centre = c(mean(Effectiveness), mean(Cost)))
                    )), group = g))
  }
  Means <- ddply(CE,.(Strategy), summarise,
                 N = length(Cost),
                 Cost.mean = mean(Cost),
                 Eff.mean = mean(Effectiveness))
  #Define ggplot object
  ggplot(CE, aes(x = Effectiveness, y = Cost, color = Strategy)) +
    geom_point(size = 0.7, alpha = 0.2, shape = 21) +
    geom_point(data = Means, aes(x = Eff.mean, y = Cost.mean, shape = Strategy),
               size = 8, fill = "white") +
    # geom_text(data = Means,aes(x = Eff.mean, y = Cost.mean, label = 1:length(strategies)), size = 5, colour = "gray", alpha = 1) +
    geom_path(data = df_ell, aes(x = x, y = y, colour = group), size = 1, linetype = 2, alpha = 1) + # draw ellipse lines
    ggtitle("Cost-Effectiveness Scatterplot") +
    scale_colour_discrete(l = 50) +  # Use a slightly darker palette than normal
    # scale_y_continuous(labels = dollar) +
    # scale_x_continuous(breaks =number_ticks(6)) +
    theme_bw() +
    theme(legend.position = "bottom")
}
