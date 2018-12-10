library(dampack)

## test data
data(example_psa)
# Name of strategies
strategies <- c("Chemo", "Radio", "Surgery")
# Vector of WTP thresholds
wtp <- seq(1000, 150000, by = 10000)
# Matrix of costs
costs <- example_psa[, c(2, 4, 6)]
# Matrix of effectiveness
effectiveness <- example_psa[, c(3, 5, 7)]
