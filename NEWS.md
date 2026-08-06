## dampack 1.0.1.9000

### New Features
-Added calculate_icers_psa function
-Added argument to plot.owsa and plot.twsa for the plotting of specific points (e.g. base case values) on top of default plot

### Bug Fixes
-summary.ceac no longer returns duplicated, mislabeled, and NA-range rows when the cost-effectiveness frontier switches optimal strategy two or more times. It now returns one row per interval (#173)

## dampack 1.0.1

### Bug Fixes
-Patch fix for compatibility with ggplot2 3.3.1. scale_fill_discrete and scale_color_discrete now differentiated in add_common_aes.R. Previous version of add_common_aes caused error with ggplot2 3.3.1

## dampack 1.0.0

-Initial CRAN submission
