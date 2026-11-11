#' Six-strategy PSA sample with a multiply-switching frontier
#'
#' Simulated costs and QALYs for six mutually exclusive strategies, kept in the
#' package because the resulting cost-effectiveness frontier changes optimal
#' strategy three times over a plausible willingness-to-pay range. Most bundled
#' examples switch at most once, which hides defects in the functions that
#' summarize intervals of optimal choice - the reason
#' \code{\link{summary.ceac}} could return duplicated and \code{NA}-range rows
#' unnoticed until 2026 (see the package NEWS for issue #173).
#'
#' It is also the sample used to develop the expected loss curve, which is what
#' the \code{elc} suffix refers to.
#'
#' @format A data frame with 10,000 rows and 12 columns, one row per PSA sample.
#' Costs occupy columns 1 to 6 and effects columns 7 to 12, so the two halves can
#' be handed straight to \code{\link{make_psa_obj}}.
#' \describe{
#'   \item{Cost_1}{simulated cost of strategy 1}
#'   \item{Cost_2}{simulated cost of strategy 2}
#'   \item{Cost_3}{simulated cost of strategy 3}
#'   \item{Cost_4}{simulated cost of strategy 4}
#'   \item{Cost_5}{simulated cost of strategy 5}
#'   \item{Cost_6}{simulated cost of strategy 6}
#'   \item{QALY_1}{simulated QALYs of strategy 1}
#'   \item{QALY_2}{simulated QALYs of strategy 2}
#'   \item{QALY_3}{simulated QALYs of strategy 3}
#'   \item{QALY_4}{simulated QALYs of strategy 4}
#'   \item{QALY_5}{simulated QALYs of strategy 5}
#'   \item{QALY_6}{simulated QALYs of strategy 6}
#' }
#'
#' @source De-identified from a probabilistic analysis of a published decision
#' model. Strategy labels are generic and the underlying parameter draws have
#' been removed, so the sample supports cost-effectiveness analysis but not
#' metamodeling or EVPPI. Costs are rounded to the cent and effects to six
#' decimal places, which leaves every frontier switch point unchanged.
#'
#' The export it was built from interleaved the two outcomes, with costs in its
#' even-numbered columns 2 to 12 and QALYs in its odd-numbered columns 3 to 13;
#' its remaining 34 columns held the parameter draws and were dropped. Here the
#' two outcomes are separated rather than interleaved, and the source column
#' names, which were inconsistent, were regularized to \code{Cost_1} to
#' \code{Cost_6} and \code{QALY_1} to \code{QALY_6}.
#'
#' @seealso \code{\link{example_psa}} for a smaller two-strategy sample.
#'
#' @examples
#' data(df_example_psa_elc)
#'
#' l_psa <- make_psa_obj(cost = df_example_psa_elc[, 1:6],
#'                       effectiveness = df_example_psa_elc[, 7:12])
#'
#' # the frontier switches three times, so the summary has four intervals
#' df_ceac <- ceac(wtp = seq(1000, 150000, 1000), psa = l_psa)
#' summary(df_ceac)
#'
#' # the strategy most likely to be cost effective is not necessarily the one
#' # on the frontier - the plot shows both
#' plot(df_ceac, frontier = TRUE)
"df_example_psa_elc"
