#' Sample psa object (superseded)
#'
#' @description
#' \code{example_psa_obj} is superseded. Build the same \code{psa} object from
#' \code{\link{cancer_psa_data}} with \code{\link{make_psa_obj}}:
#'
#' \preformatted{psa_cancer <- make_psa_obj(cost = cancer_psa_data$cost,
#'                            effectiveness = cancer_psa_data$effectiveness,
#'                            parameters = cancer_psa_data$parameters,
#'                            strategies = cancer_psa_data$strategies)}
#'
#' It is kept for backward compatibility and will be removed in
#' \code{dampack} 2.0.
#'
#' @format A \code{psa} object (in the \code{dampack} 1.0.x layout) built from
#' the PSA samples in \code{\link{cancer_psa_data}}.
#'
#' @keywords internal
"example_psa_obj"
