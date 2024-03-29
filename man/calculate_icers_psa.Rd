% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icers.R
\name{calculate_icers_psa}
\alias{calculate_icers_psa}
\title{Calculate incremental cost-effectiveness ratios from a \code{psa} object.}
\usage{
calculate_icers_psa(psa, uncertainty = FALSE)
}
\arguments{
\item{psa}{\code{psa} object from \code{link{make_psa_object}}}

\item{uncertainty}{whether or not 95% quantiles for the cost and QALY outcomes should be included
in the resulting ICER table. Defaults to \code{FALSE}.}
}
\value{
A data frame and \code{icers} object of strategies and their associated
status, cost, effect, incremental cost, incremental effect, and ICER. If \code{uncertainty} is
set to \code{TRUE}, four additional columns are provided for the 2.5th and 97.5th quantiles for
each strategy's cost and effect.
}
\description{
The mean costs and QALYs for each strategy in a PSA are used
to conduct an incremental cost-effectiveness analysis. \code{\link{calculate_icers}} should be used
if costs and QALYs for each strategy need to be specified manually, whereas \code{calculate_icers_psa}
can be used if mean costs and mean QALYs from the PSA are assumed to represent a base case scenario for
calculation of ICERS.

Optionally, the \code{uncertainty} argument can be used to provide the 2.5th and 97.5th
quantiles for each strategy's cost and QALY outcomes based on the variation present in the PSA.
Because the dominated vs. non-dominated status and the ordering of strategies in the ICER table are
liable to change across different samples of the PSA, confidence intervals are not provided for the
incremental costs and QALYs along the cost-effectiveness acceptability frontier.
\code{link{plot.psa}} does not show the confidence intervals in the resulting plot
even if present in the ICER table.
}
\seealso{
\code{\link{plot.icers}}

\code{\link{calculate_icers}}
}
