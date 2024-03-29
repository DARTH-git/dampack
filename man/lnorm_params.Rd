% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_psa_samp.R
\name{lnorm_params}
\alias{lnorm_params}
\title{Calculate location and scale parameters of a log-normal distribution.}
\usage{
lnorm_params(m = 1, v = 1)
}
\arguments{
\item{m}{Scalar with the mean of the random variable.}

\item{v}{Scalar with the variance of the random variable.
(i.e., squared standar error).}
}
\value{
A list containing the following:

mu Location parameter of log-normal distribution

sigma Scale parameter of log-normal distribution
}
\description{
Function to calculate the location, \eqn{\mu}, and scale, \eqn{\sigma},
parameteres of a log-normal distribution based on the method of moments (MoM)
using the mean \eqn{m} and variance \eqn{v} of the non-logarithmized random
variable of interest.
}
\section{Details}{

Based on method of moments. If \eqn{m} is the mean and
\eqn{v} is the variance of the random variable, then the
the location, \eqn{\mu}, and scale, \eqn{\sigma}, parameteres are computed
as follows
\deqn{\mu = \ln{(\frac{m}{\sqrt{(1 + \frac{v}{m^2})}})}}
and
\deqn{\sigma = \sqrt{\ln{( 1 + \frac{v}{m^2})}}}
}

\examples{
m <- 3
v <- 0.01
lnorm_params(m, v)
# True values: 100, 30, 70
}
\references{
\enumerate{
\item Ginos BF. Parameter Estimation for the Lognormal Distribution.
Brigham Young University; 2009.
\item Log-normal distribution. (2017, April 20). In Wikipedia, The Free
Encyclopedia. Retrieved 16:47, April 23, 2017,
from https://en.wikipedia.org/w/index.php?title=Log-normal_distribution&oldid=776357974
}
}
\keyword{distribution;}
\keyword{log-normal}
\keyword{method}
\keyword{moments}
\keyword{of}
