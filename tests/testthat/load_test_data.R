library(dampack)

## test data
data(psa)
# Name of strategies
strategies <- c("Chemo", "Radio", "Surgery")
# Vector of WTP thresholds
wtp <- seq(1000, 150000, by = 10000)
# Matrix of costs
costs <- psa[, c(2, 4, 6)]
# Matrix of effectiveness
effectiveness <- psa[, c(3, 5, 7)]
