% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_psa.R
\name{run_psa}
\alias{run_psa}
\title{Calculate outcomes for a PSA using a user-defined function.}
\usage{
run_psa(
  psa_samp,
  params_basecase = NULL,
  FUN,
  outcomes = NULL,
  strategies = NULL,
  currency = "$",
  progress = TRUE,
  ...
)
}
\arguments{
\item{psa_samp}{A data frame with samples of parameters for a probabilistic sensitivity analysis (PSA)}

\item{params_basecase}{a named list of base case values for input parameters needed by \code{FUN},
the user-defined function.}

\item{FUN}{Function that takes the parameter values in \code{psa_samp} and \code{...} to
produce the \code{outcome} of interest. The \code{FUN} must return a data frame
where the first column are the strategy names and the rest of the columns must be outcomes.}

\item{outcomes}{String vector with the outcomes of interest from \code{FUN}.}

\item{strategies}{vector of strategy names. The default \code{NULL} will use
strategy names in \code{FUN}}

\item{currency}{symbol for the currency being used (ex. "$", "£")}

\item{progress}{\code{TRUE} or \code{FALSE} for whether or not function progress
should be displayed in console.}

\item{...}{Additional arguments to user-defined \code{FUN}}
}
\value{
A list containing PSA objects for each outcome in \code{outcomes}.
}
\description{
\code{run_psa} calculates outcomes using a user-defined function and creates PSA objects
corresponding to the specified outcomes.
}
\seealso{
\code{\link{run_psa}},
\code{\link{make_psa_obj}},
\code{\link{gen_psa_samp}},
}
