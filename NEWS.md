# dampack (development version)

## New features

- `print.icers()` prints a formatted ICER table, with options for the currency symbol and the number of digits (#184, #171).
- `plot.psa()` gains a `ref_str` argument that plots the PSA on the incremental cost-effectiveness plane relative to a chosen reference strategy (#188).
- Plotting functions now default to an accessible, colorblind-friendly palette (`col = "access"`, based on viridis). The previous palettes remain available as `col = "full"` and `col = "bw"`.
- Added `cancer_psa_data`, PSA output from an illustrative model of three cancer treatments (chemotherapy, radiation, surgery). It is now the example dataset used throughout the examples, tests, and vignettes.
- Added `df_example_psa_elc`, a de-identified six-strategy PSA sample whose cost-effectiveness frontier switches optimal strategy three times. It is useful for exercising CEAC and expected loss curve functions, which behave differently when the frontier switches more than once (#180).
- Added a package overview help page, `?dampack` (#150).

## Superseded

- `example_psa` and `example_psa_obj` are superseded by `cancer_psa_data`, which contains the same PSA samples. Both remain available for backward compatibility and will be removed in dampack 2.0. They are now rebuilt from `cancer_psa_data` when the package is installed, so the samples are stored only once.

## Breaking changes

- Strategy names are now kept exactly as provided in `make_psa_obj()`, `create_dsa_oneway()`, `create_dsa_twoway()`, and the objects built from them, so names such as "Standard of care" appear unchanged in tables and plot legends. Previously they were converted with `make.names()` (to `Standard.of.care`) with a warning. Code that refers to the converted names must use the original ones, for example `psa$cost[["Standard of care"]]` instead of `psa$cost$Standard.of.care`. Strategy names must be unique and not missing or empty, and a wrong number of names now gives an informative error (#208).
- `summary.ceac()` renames its third output column from `cost_eff_strat` to `optimal_strategy`. Code that refers to the old column name with `$cost_eff_strat` must be updated (#173).
- dampack now requires R 4.1.0 or later, the minimum required by current versions of ggplot2.
- dampack now requires ggplot2 3.5.0 or later and scales 1.3.0 or later, for ggplot2's `transform` argument and `scales::new_transform()` (#211).

## Bug fixes

- `plot.owsa(col = "bw")` now draws each strategy with its own line type; all strategies used to share one line type and could not be told apart (#176).
- `metamodel()` now names invalid parameters in its error message, including when called through `calc_evppi()` and `calc_evsi()` (#177).
- `gen_psa_samp()` now accepts the documented `"value, weight"` parameterization for `"bootstrap"` distributions. It used to be dropped silently, which shifted the parameterization of every later parameter by one position. **Samples from calls with a bootstrap parameter before other parameters will differ, because they are now correct.** Unknown distributions or parameterizations, and `params`, `dists`, `parameterization_types`, and `dists_params` of different lengths, now give informative errors instead of being dropped silently (#175).
- `calculate_icers()` with a single strategy now returns a valid `icers` object, with the same columns as for several strategies (`Status` is `"ND"` and the incremental columns are `NA`), so `plot()` and `print()` work on it (#183).
- Plots that choose an accessible palette from the number of groups (`owsa_tornado()`, `owsa_opt_strat()`, `plot.twsa()`, and the value-of-information plots) no longer fail on ggplot2 versions before 4.0 with "no applicable method for `@` applied to an object of class 'ggplot_built'" (#198).
- `calculate_icers()` with no strategies now gives an informative error instead of "missing value where TRUE/FALSE needed", and its documentation no longer describes a reference-strategy option that does not exist (#178).
- `ceac()`, `calc_exp_loss()`, and the expected value of perfect information no longer pick the optimal strategy at random among strategies whose net benefits differ by less than a relative 1e-5 (for example, a few dollars at a scale of $1,000,000). `max.col()`'s default `ties.method = "random"` treated these as ties; comparisons are now exact, and exact ties go to the first strategy. Results for such near-tied strategies change and are now reproducible (#167).
- `metamodel()`, and the functions that use it (`owsa()` and `twsa()` on a PSA, `calc_evppi()`, `calc_evsi()`), now work with strategy names that are not syntactically valid R names (#208).
- `owsa_opt_strat()` plots now draw one bar per range in which a strategy is optimal, and adjacent bars meet halfway between parameter values, so the bars line up with the switch points on the axis. Before, each bar covered only the values where its strategy was observed optimal (leaving gaps), and a strategy optimal in two separate ranges got a single bar spanning both. The data returned with `return = "data"` is unchanged (#67).
- `summary.ceac()` no longer returns duplicated, mislabeled, and `NA`-range rows when the cost-effectiveness frontier switches optimal strategy two or more times. It now returns one row per interval (#173).
- `run_psa()` no longer drops the first parameter of a PSA sample that was not created by `gen_psa_samp()`; it removes only the `nsamp` sample-number column that `gen_psa_samp()` adds. When `FUN` fails on the PSA parameters, `run_psa()` now stops with "FUN is not well defined ..." followed by `FUN`'s own error; that check never ran before (#174).
- The y-axis label of the CEAC plot now correctly describes what is plotted (#165).
- `twsa()` on a `dsa_twoway` object now works without an explicit `outcome`; it used to fail with "'arg' must be of length 1" (#210).
- `make_psa_obj()` and the DSA constructors now name the columns of `other_outcome` after the strategies, like `cost` and `effectiveness` (#210).
- When `FUN` fails on the base case, `run_owsa_det()` and `run_twsa_det()` now stop with "FUN is not well defined ..." followed by `FUN`'s own error; that check never ran before (#210).
- `plot.exp_loss(col = "bw")` now uses `lsize` for the line width and no longer warns about an unused color scale (#210).
- `owsa_tornado()` no longer omits parameters whose minimum value is negative (#166).
- Code hygiene throughout the package: progress bars in `run_owsa_det()` and `run_twsa_det()` are now closed, loops handle zero-length inputs (for example, `calc_evpi()` with an empty `wtp` returns an empty result), deprecated `trans`/`scales::trans_new()` are replaced by ggplot2's `transform` and `scales::new_transform()`, `cor()` is imported, and the `rdirichlet()` documentation is fixed. Thanks to @IG-ABHINAV for this contribution (#211, #224).
- Fixed a pointer in the PSA vignette to a vignette that no longer exists (#168).

# dampack 1.0.2.1000

## New features

- Added `calculate_icers_psa()`, which computes ICERs from a PSA object with uncertainty intervals (#160).
- `plot.owsa()` and `plot.twsa()` gain an argument to plot specific points, such as base-case values, on top of the default plot (#155).
- `number_ticks()` is now exported (#158).

## Minor improvements

- Code updated to pass lintr style checks, to current dependency versions, and to the current grammar of ggplot2 and dplyr; minor grammar fixes in documentation.

# dampack 1.0.1

## Bug fixes

- Compatibility fix for ggplot2 3.3.1: `scale_fill_discrete()` and `scale_color_discrete()` are now differentiated in `add_common_aes()`. The previous version caused an error with ggplot2 3.3.1.

# dampack 1.0.0

- Initial CRAN submission.
