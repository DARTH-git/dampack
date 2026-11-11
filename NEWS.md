## dampack 1.0.1.9000

### New Features
-Added df_example_psa_elc, a de-identified six-strategy PSA sample whose cost-effectiveness frontier switches optimal strategy three times. Useful for exercising CEAC and expected loss curve functions, which behave differently when the frontier switches more than once (#180)
-Added calculate_icers_psa function
-Added argument to plot.owsa and plot.twsa for the plotting of specific points (e.g. base case values) on top of default plot

### Breaking Changes
-summary.ceac renames its third output column from `cost_eff_strat` to `optimal_strategy`. Code that referred to the old column name by `$cost_eff_strat` must be updated (#173)

### Bug Fixes
-summary.ceac no longer returns duplicated, mislabeled, and NA-range rows when the cost-effectiveness frontier switches optimal strategy two or more times. It now returns one row per interval (#173)

## dampack 1.0.1

### Bug Fixes
-Patch fix for compatibility with ggplot2 3.3.1. scale_fill_discrete and scale_color_discrete now differentiated in add_common_aes.R. Previous version of add_common_aes caused error with ggplot2 3.3.1

## dampack 1.0.0

-Initial CRAN submission
