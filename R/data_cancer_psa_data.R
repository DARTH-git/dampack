#' PSA output from a model of three cancer treatments
#'
#' Probabilistic sensitivity analysis (PSA) output from an illustrative
#' decision model comparing three treatments for cancer: chemotherapy,
#' radiation, and surgery. It contains the strategy names, the cost and
#' effectiveness of each strategy in each of 10,000 PSA samples, and the
#' values of the 8 model parameters drawn in each sample. It is the main
#' example dataset used throughout the \code{dampack} examples and vignettes.
#'
#' @format A list with 4 elements:
#' \describe{
#'   \item{strategies}{character vector of the 3 strategy names:
#'   \code{"Chemo"}, \code{"Radio"}, and \code{"Surgery"}.}
#'   \item{cost}{data frame with 10,000 rows (PSA samples) and 3 columns
#'   (\code{Chemo_Cost}, \code{Radio_Cost}, \code{Surg_Cost}) with the cost of
#'   each strategy, in the same order as \code{strategies}.}
#'   \item{effectiveness}{data frame with 10,000 rows and 3 columns
#'   (\code{Chemo_Eff}, \code{Radio_Eff}, \code{Surg_Eff}) with the
#'   effectiveness of each strategy, in the same order as \code{strategies}.}
#'   \item{parameters}{data frame with 10,000 rows and 8 columns with the
#'   parameter values drawn in each PSA sample:
#'   \describe{
#'     \item{pFailChemo}{probability of treatment failure with chemotherapy}
#'     \item{pFailRadio}{probability of treatment failure with radiation}
#'     \item{pFailSurg}{probability of treatment failure with surgery}
#'     \item{pDieSurg}{probability of dying from surgery}
#'     \item{muDieCancer}{cancer-specific mortality}
#'     \item{cChemo}{cost of chemotherapy}
#'     \item{cRadio}{cost of radiation}
#'     \item{cSurg}{cost of surgery}
#'   }}
#' }
#'
#' @details This dataset replaces \code{\link{example_psa}} and
#' \code{\link{example_psa_obj}}, which contain the same PSA samples and are
#' kept only for backward compatibility.
#'
#' @seealso \code{\link{make_psa_obj}} to build a \code{psa} object from this
#' dataset, and \code{vignette("psa_analysis", package = "dampack")}.
#'
#' @examples
#' data("cancer_psa_data")
#' psa_cancer <- make_psa_obj(cost = cancer_psa_data$cost,
#'                            effectiveness = cancer_psa_data$effectiveness,
#'                            parameters = cancer_psa_data$parameters,
#'                            strategies = cancer_psa_data$strategies)
#' plot(psa_cancer)
"cancer_psa_data"
