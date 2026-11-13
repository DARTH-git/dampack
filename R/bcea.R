#' Convert between BCEA and dampack objects
#'
#' @description
#' [bcea_to_psa()] turns the output of [BCEA::bcea()] into a dampack `psa` object, so the
#' dampack analyses and plots (CEAC, expected loss curves, EVPI, ICERs with uncertainty, and so on)
#' can be used on it. [psa_to_bcea()] does the reverse, so a dampack PSA can be analyzed with
#' the BCEA package.
#'
#' @param bcea an object of class `bcea`, created with [BCEA::bcea()].
#' @param currency currency symbol or word used to label costs (e.g., `"$"`, `"£"`, `"€"`).
#' @param parameters optional data frame with the parameter values of each PSA sample (one row per
#' sample, in the same order as in `bcea`). BCEA objects do not store parameter values; supply them
#' to use parameter-based analyses such as [calc_evppi()] or [owsa()].
#'
#' @return [bcea_to_psa()] returns a `psa` object (see [make_psa_obj()]) with one strategy per
#' intervention in `bcea`, keeping the intervention names. [psa_to_bcea()] returns a `bcea` object.
#'
#' @details BCEA stores the costs and effects of each simulation and intervention, which is what a
#' dampack `psa` object needs. BCEA's own results (for example its incremental benefits at each
#' willingness to pay) are not carried over; dampack computes its own from the costs and effects.
#'
#' @examples
#' if (requireNamespace("BCEA", quietly = TRUE)) {
#'   data(Vaccine, package = "BCEA")
#'   m <- BCEA::bcea(eff, cost, ref = 2, interventions = treats, Kmax = 50000)
#'
#'   # BCEA -> dampack
#'   psa_vaccine <- bcea_to_psa(m, currency = "£")
#'   plot(ceac(seq(0, 50000, by = 5000), psa_vaccine))
#'
#'   # dampack -> BCEA
#'   m2 <- psa_to_bcea(psa_vaccine, ref_str = "Vaccination", Kmax = 50000)
#' }
#' @seealso [make_psa_obj()], [BCEA::bcea()]
#' @md
#' @export
bcea_to_psa <- function(bcea, currency = "$", parameters = NULL) {
  if (!inherits(bcea, "bcea")) {
    stop("`bcea` must be an object of class \"bcea\", created with BCEA::bcea().", call. = FALSE)
  }
  if (is.null(bcea$c) || is.null(bcea$e) || is.null(bcea$interventions)) {
    stop("`bcea` must contain the costs (`c`), effects (`e`), and intervention names ",
         "(`interventions`) of each simulation.", call. = FALSE)
  }
  df_cost <- as.data.frame(as.matrix(bcea$c))
  df_effect <- as.data.frame(as.matrix(bcea$e))
  make_psa_obj(cost = df_cost,
               effectiveness = df_effect,
               parameters = parameters,
               strategies = as.character(bcea$interventions),
               currency = currency)
}

#' @rdname bcea_to_psa
#' @param psa a `psa` object, created with [make_psa_obj()].
#' @param ref_str name of the reference strategy, passed to [BCEA::bcea()] as `ref`. Defaults to
#' the first strategy.
#' @param ... further arguments passed to [BCEA::bcea()], for example `Kmax` or `k`.
#' @export
psa_to_bcea <- function(psa, ref_str = NULL, ...) {
  rlang::check_installed("BCEA", reason = "to create a bcea object.")
  check_psa_object(psa)
  if (is.null(ref_str)) {
    ref_str <- psa$strategies[1]
  }
  if (length(ref_str) != 1 || !ref_str %in% psa$strategies) {
    stop("`ref_str` must be one of the strategies: ",
         paste0("\"", psa$strategies, "\"", collapse = ", "), ".", call. = FALSE)
  }
  BCEA::bcea(eff = as.matrix(psa$effectiveness),
             cost = as.matrix(psa$cost),
             ref = match(ref_str, psa$strategies),
             interventions = psa$strategies,
             ...)
}
