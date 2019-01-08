#----------------------------#
#### Expected Loss Curves ####
#----------------------------#
elc <- function(v.wtp, strategies, m.e, m.c) {
  library(reshape2)
  library(ggplot2)
  library(scales)
  n.sim <- nrow(m.e)
  n.str  <- ncol(m.e)
  m.exp.loss <- matrix(0, nrow = length(v.wtp), ncol = n.str)
  for(l in 1:length(v.wtp)) {
    m.nmb <- m.e * v.wtp[l] - m.c # Effectiveness minus Costs, with vector indexing
    max.str <- max.col(m.nmb)
    m.loss <- m.nmb - m.nmb[cbind(1:n.sim, max.str)]
    m.exp.loss[l, ] <- colMeans(m.loss)
  }
  # Optimal strategy based on lowest expected loss
  optimal.str <- max.col(m.exp.loss)
  # Expected loss of optimal strategy
  optimal.el <- m.exp.loss[cbind(1:length(v.wtp), optimal.str)]
  # Format expected loss for plotting
  df.exp.loss <- data.frame(cbind(v.wtp, m.exp.loss, optimal.el))
  colnames(df.exp.loss) <- c("WTP", strategies, "Frontier & EVPI")
  df.exp.loss.plot <- melt(df.exp.loss, 
                           id.vars = "WTP", 
                           variable.name = "Strategy")
  ## Plot expected losses
  # Format to plot frontier
  strats <- 1:(length(unique(df.exp.loss.plot$Strategy)) - 1)
  point.shapes <- c(strats+14, 0) # Shapes: http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  colors <- c(gg_color_hue(length(strats)), "#696969")
  point.size <- c(rep(2, length(strats)), 4) # Trick consists on firts define size as aes then manually change it
  # Plot ELC
  print(
    ggplot(data = df.exp.loss.plot, aes(x = WTP/1000, y = -value)) +
      geom_point(aes(shape = Strategy, color = Strategy, size = Strategy)) +
      geom_line(aes(color = Strategy)) +
      ggtitle("Expected Loss Curves") + 
      #scale_colour_hue(l = 50, values = colors) +
      scale_x_continuous(breaks = number_ticks(20))+
      scale_y_continuous("Log(Expected loss in thousand $)", 
                         trans = "log",
                         labels = function(x) format(round(x/10000, 1), nsmall = 1),
                         breaks = number_ticks(8))  +
      xlab("Willingness to Pay (Thousand $/QALY)") +
      ylab("Expected loss ($)") +
      scale_shape_manual(values = point.shapes) +
      scale_color_manual(values = colors) + 
      scale_size_manual(values = point.size) +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  )
}