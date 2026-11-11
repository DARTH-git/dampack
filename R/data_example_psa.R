#' Sample PSA data (superseded)
#'
#' @description
#' \code{example_psa} is superseded by \code{\link{cancer_psa_data}}, which
#' contains the same PSA samples. It is kept for backward compatibility and
#' will be removed in \code{dampack} 2.0.
#'
#' @format A list with the same \code{strategies}, \code{cost},
#' \code{effectiveness}, and \code{parameters} elements as
#' \code{\link{cancer_psa_data}}, plus \code{wtp}, a vector of 15
#' willingness-to-pay thresholds (\code{seq(1000, 141000, by = 10000)}).
#'
#' @keywords internal
"example_psa"
