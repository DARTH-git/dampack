#' dampack: Decision-Analytic Modeling Package
#'
#' @description
#' `dampack` analyzes and visualizes the health economic outputs of
#' decision-analytic models. It works on the outputs of any model, whatever
#' language or framework produced them: a vector of costs and effects for a
#' base-case cost-effectiveness analysis, or a probabilistic sensitivity
#' analysis (PSA) with the cost, effect, and parameter values of each sample.
#'
#' @section Main functions:
#' \describe{
#'   \item{Cost-effectiveness analysis}{[calculate_icers()] identifies
#'   dominated and extendedly dominated strategies and computes incremental
#'   cost-effectiveness ratios (ICERs) along the efficient frontier;
#'   [calculate_icers_psa()] does the same from a PSA, with uncertainty
#'   intervals. Results have `print()` and `plot()` methods.}
#'   \item{PSA objects}{[make_psa_obj()] builds a `psa` object from model
#'   outputs, with `summary()` and `plot()` methods for the cost-effectiveness
#'   plane.}
#'   \item{Decision uncertainty}{[ceac()] computes cost-effectiveness
#'   acceptability curves and the acceptability frontier; [calc_exp_loss()]
#'   computes expected loss curves.}
#'   \item{Value of information}{[calc_evpi()], [calc_evppi()], and
#'   [calc_evsi()] compute the expected value of perfect, partial perfect, and
#'   sample information; [metamodel()] fits the regression metamodels they
#'   rely on.}
#'   \item{Deterministic sensitivity analysis}{[run_owsa_det()] and
#'   [run_twsa_det()] run one-way and two-way sensitivity analyses on a
#'   user-supplied model; [owsa()] and [twsa()] derive them from a PSA. Plot
#'   them with `plot()`, [owsa_tornado()], and [owsa_opt_strat()].}
#'   \item{Generating a PSA}{[gen_psa_samp()] draws parameter samples from
#'   common distributions (with helpers such as [beta_params()] and
#'   [gamma_params()] for method-of-moments parameters), and [run_psa()] runs
#'   a user-supplied model over them.}
#' }
#'
#' @section Getting started:
#' The vignettes walk through each analysis with worked examples. Read them in
#' this order:
#' `vignette("basic_cea", package = "dampack")`,
#' `vignette("psa_analysis", package = "dampack")`,
#' `vignette("voi", package = "dampack")`,
#' `vignette("psa_generation", package = "dampack")`, and
#' `vignette("dsa_generation", package = "dampack")`.
#'
#' @section Funding:
#' `dampack` was developed with funding from the National Institute of
#' Allergy and Infectious Diseases of the National Institutes of Health under
#' award no. R01AI138783. The content is solely the responsibility of the
#' authors and does not necessarily represent the official views of the
#' National Institutes of Health.
#'
#' @references
#' Hunink MGM, Weinstein MC, Wittenberg E, Drummond MF, Pliskin JS, Wong JB,
#' Glasziou PP. Decision Making in Health and Medicine: Integrating Evidence
#' and Values. 2nd ed. Cambridge University Press; 2014.
#' \doi{10.1017/CBO9781139506779}
#'
#' @seealso `citation("dampack")` for how to cite the package.
#' @md
"_PACKAGE"
