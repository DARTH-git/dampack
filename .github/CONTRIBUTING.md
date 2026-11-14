# Contributing to dampack

Thank you for helping improve `dampack`. This guide covers how to report problems, how to propose changes, and the conventions the package follows. It applies to DARTH members and outside contributors alike.

By participating, you agree to follow the [Code of Conduct](../CODE_OF_CONDUCT.md).

## Reporting a bug or asking for a feature

- Search the [open issues](https://github.com/DARTH-git/dampack/issues) first.
- For a bug, open an issue with the **Bug report** template and include a minimal reproducible example. The [reprex](https://reprex.tidyverse.org/) package makes this easy: `reprex::reprex()` runs your code in a clean session and formats the output for GitHub. Please include `sessionInfo()` or `sessioninfo::session_info()`.
- For a new feature, open an issue with the **Feature request** template before writing code, so the design can be discussed. Explain the decision-analytic use case: what analysis you are trying to do and what output you expect.

## Branches and pull requests

`main` is the only long-lived branch. It must always pass `R CMD check`.

1. Open (or pick) an issue for the change.
2. Create a short-lived branch from `main`, named after the issue: `<issue number>-<short-description>`, for example `203-currency-propagation`.
3. Make focused commits. One pull request (PR) should address one issue.
4. Open a PR into `main` and fill in the PR template. Use `Closes #<issue>` in the description so the issue closes **when the PR is merged into `main`**, not before.
5. A PR is merged after CI passes and at least one other DARTH member approves it. The branch is deleted automatically after merging.

Do not push directly to `main`, and do not keep long-running integration branches; they are how the package's history diverged before v1.1.0.

## Definition of done

Every PR that changes package code includes:

- **Tests.** A bug fix comes with a regression test that fails before the fix and passes after. A new function or argument comes with tests of its results, not only that it runs. Use `set.seed()` in any test that draws random numbers.
- **Documentation.** roxygen2 comments updated, and `devtools::document()` run so `man/` and `NAMESPACE` are current. New exported functions need `@examples` that run.
- **A NEWS entry.** One bullet in `NEWS.md` under the development version, ending with the issue number, for example `(#203)`. Mark behavior changes and breaking changes explicitly.
- **A clean check.** `devtools::check()` passes locally with no errors or warnings.

## Running the checks locally

```r
# install the development dependencies once
install.packages(c("devtools", "lintr", "covr", "pkgdown"))

devtools::load_all()       # load the package from source
devtools::document()       # regenerate man/ and NAMESPACE from roxygen comments
devtools::test()           # run the tests
devtools::check()          # full R CMD check, as CI and CRAN run it
lintr::lint_package()      # style checks (also run by CI)
covr::report()             # browse test coverage line by line
pkgdown::build_site()      # preview the website locally (output in docs/, not committed)
```

## Coding conventions

### Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/), with lines of up to 120 characters. The `.lintr` file defines the exact rules, and CI runs `lintr` on every PR.
- Use implicit returns; reserve `return()` for early exits.
- Use `seq_len(n)` or `seq_along(x)` instead of `1:n` or `1:length(x)`, which misbehave when `n` is 0.
- Refer to columns that hold strategy names with `df[[name]]` or `df[, name, drop = FALSE]`, never `df$name` or a formula, because strategy names may contain spaces and other characters.

### Naming objects inside the package (DARTH coding framework)

Internal objects follow the DARTH naming convention `<data type>_<variable type>_<description>`, from Alarid-Escudero et al. (2019), "A need for change! A coding framework for improving transparency in decision modeling", *PharmacoEconomics* 37(11):1329-1339, [doi:10.1007/s40273-019-00837-x](https://doi.org/10.1007/s40273-019-00837-x).

| Data type prefix | | Variable type prefix | |
|---|---|---|---|
| (none) | scalar | `n` | number |
| `v` | vector | `p` | probability |
| `m` | matrix | `r` | rate |
| `a` | array | `u` | utility |
| `df` | data frame | `c` | cost |
| `dtb` | data table | `hr` | hazard ratio |
| `l` | list | `q` | QALYs |

Examples: `df_cea`, `v_wtp`, `m_nmb`, `l_psa`, `n_sim`. Either prefix is left out when it does not apply. Note that `n_` means "number", not "scalar": scalars carry no data type prefix.

A prefix must stay true: an object named `m_` must still be a matrix after subsetting (use `drop = FALSE`).

### Exported function names and arguments

Exported function and argument names are part of the public API that users' scripts depend on. Do not rename them in a minor release. A rename goes through soft deprecation with the [lifecycle](https://lifecycle.r-lib.org/) package: keep the old name working, warn with `lifecycle::deprecate_soft()`, record it in `NEWS.md`, and remove it only in the next major version.

New exported functions start with a verb (`calculate_`, `make_`, `plot_`, ...), and new arguments follow the names already used elsewhere in the package (`psa`, `wtp`, `currency`, `effect_units`, `txtsize`, `col`).

### API style (decision record, #214)

Adopted for dampack 1.1.0 (November 2026). It records the names the package uses now, so new code follows them, and the targets for 2.0 that the group still has to agree on. Nothing below renames an existing function or argument in 1.x except through soft deprecation.

**Argument order.** The data object comes first, then the threshold: `calc_exp_loss(psa, wtp)`, `calc_evpi(psa, wtp)`, `calc_evppi(psa, wtp, ...)`, `calc_evsi(psa, wtp, ...)`, `owsa(sa_obj, ...)`, `twsa(sa_obj, ...)`. Since 1.1.0 `ceac(psa, wtp)` does too; the old positional `ceac(wtp, psa)` still works with a soft deprecation warning.

**Plot arguments.** A plot method uses these names whenever it has the option, with these meanings:

| Argument | Meaning |
|---|---|
| `txtsize` | base text size |
| `col` | `c("access", "full", "bw")`: accessible (viridis), full color, or black and white. Only where strategies or groups are told apart by color; `plot.icers()` (shape and line type), `plot.evpi()` and `plot.evppi()` (one line) have none |
| `currency` | `NULL` uses the currency stored in the object (from `make_psa_obj()` or `calculate_icers()`), otherwise `"$"` |
| `effect_units` | units of effectiveness in the plural, default `"QALYs"`; labels that mean "per unit" use the singular (`"$/QALY"`) |
| `n_x_ticks`, `n_y_ticks`, `xbreaks`, `ybreaks` | approximate number of ticks, or exact breaks |
| `xlim`, `ylim` | limits of each continuous axis (`NULL` sets them automatically) |
| `xtrans`, `ytrans` | axis transformations such as `"log10"` (`plot.psa()`) |
| `linewidth` | line width, as in ggplot2. `plot.owsa(size)` and `plot.exp_loss(lsize)` are deprecated in its favor |

**Open questions for 2.0** (meeting decision 3; no aliases are added in 1.1.0):

- Verb prefixes: the package has both `calc_` (`calc_evpi()`, `calc_exp_loss()`) and `calculate_` (`calculate_icers()`, `calculate_icers_psa()`).
- `effect` (`calculate_icers()`) versus `effectiveness` (`make_psa_obj()` and the `psa` object).
- Column case: most result tables use capitalized columns (`Strategy`, `Cost`, `WTP`, `Proportion`), while `owsa` and `twsa` objects use lower case (`strategy`, `parameter`, `outcome_val`). Until this is decided, new columns follow the existing style of their object.

**Accepted minor break in 1.1.0.** `summary.ceac()` renames its third column from `cost_eff_strat` to `optimal_strategy` (#173), as recorded in NEWS.md.

### Files and tests

- Source files live in `R/`, one family of functions per file.
- Each source file has a matching test file in `tests/testthat/`. Existing test files are named `test_<source file>.R`; follow that pattern until the test suite is migrated to the `test-<source file>.R` form that `usethis::use_test()` creates.
- Example datasets are documented in `R/data_<dataset name>.R`.

## Releases

The maintainer prepares CRAN releases from `main` using `usethis::use_release_issue()` as the checklist. Each release is tagged (`vX.Y.Z`), published as a GitHub release, and archived on Zenodo.
