#' Plot of Cost-Effectiveness Acceptability Curves (CEAC)
#'
#' Plots the CEAC as a \code{ggplot2} object calculated with \code{\link{ceac}}.
#' @param ceac Object of class \code{ceac}. A melted data frame produced by
#' function \code{ceac} with each strategy's probability of being
#' cost-effective for each willingness-to-pay (WTP) threshold
#' @param title String with graph's title
#' @param txtsize number with text size
#' @param currency String with currency used in the cost-effectiveness analysis (CEA).
#' Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @keywords cost-effectiveness acceptability curves
#' @section Details:
#' \code{ceac} computes the probability of each of the strategies being
#' cost-effective at each \code{wtp} value.
#' @return ceac.gg A \code{ggplot2} object with the CEAC
#'
plot.ceac <- function(ceac,
                      title = "Cost-Effectiveness Acceptability Curves",
                      txtsize = 12,
                      currency = "$"){
  ceac <- data.frame(WTP = ceac$WTP, Strategy = ceac$Strategy, Proportion = ceac$value)
  # Load required packages
  require(ggplot2)
  # Use `scales` package for `alpha` parameter on legend
  require(scales)
  ggplot(data = ceac, aes(x = WTP/1000, y = Proportion,
                          color = Strategy,
                          #linetype = Strategies,
                          shape = Strategy)) +
    geom_point() +
    geom_line() +
    ggtitle(title) +
    scale_colour_hue(l=50) +
    scale_x_continuous(breaks=number_ticks(20))+
    xlab(paste("Willingness to Pay (Thousand ", currency, "/QALY)", sep = "")) +
    ylab("Pr Cost-Effective") +
    theme_bw() +
    theme(legend.title=element_text(size = txtsize), #legend.position="right",
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize-3),
          legend.background = element_rect(fill=alpha(0.4)),
          title = element_text(face="bold", size=14),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize),
          legend.position = c(0.5, 0.5))
}
