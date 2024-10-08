% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metamodel.R
\name{mm_run_reg}
\alias{mm_run_reg}
\title{Build formula and run linear regression for metamodel}
\usage{
mm_run_reg(analysis, dep, params, dat, type, poly.order, k)
}
\arguments{
\item{analysis}{either "oneway" or "twoway"}

\item{dep}{dependent variable in regression}

\item{params}{all params in PSA}

\item{dat}{data to use in regression}

\item{type}{type of metamodel}

\item{poly.order}{order of polynomial for the linear regression metamodel.
Default: 2}

\item{k}{ the dimension of the basis used to represent the smooth term.
    The default depends on the number of variables that the smooth is a
    function of. \code{k} should not be less than the dimension of the
    null space of the penalty for the term (see
    \code{\link[mgcv]{null.space.dimension}}), but will be reset if
  it is. See \code{\link[mgcv]{choose.k}} for further information.}
}
\value{
a statistical metamodel
}
\description{
Build formula and run linear regression for metamodel
}
\keyword{internal}
