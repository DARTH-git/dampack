% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp_loss.R
\name{calc_exp_loss}
\alias{calc_exp_loss}
\title{Calculate the expected loss at a range of willingness-to-pay thresholds}
\usage{
calc_exp_loss(psa, wtp)
}
\arguments{
\item{psa}{object of class \code{psa}, produced by function
\code{\link{make_psa_obj}}}

\item{wtp}{vector of willingness to pay thresholds}
}
\value{
object with classes \code{exp_loss} and \code{data.frame}
}
\description{
The expected loss is the quantification of the foregone benefits
when choosing a suboptimal strategy given current evidence.
}
\details{
Visualize the expected loss at a variety of WTP thresholds using \code{\link{plot.exp_loss}}.
}
\examples{
data("example_psa_obj")
wtp <- seq(1e4, 1e5, by = 1e4)
exp_loss <- calc_exp_loss(example_psa_obj, wtp)

# can use head(), summary(), print(), etc.
head(exp_loss)

# plot an expected loss curve (ELC)
plot(exp_loss)

# the y axis is on a log scale by default
plot(exp_loss, log_y = FALSE)
}
\references{
\enumerate{
\item Alarid-Escudero F, Enns EA, Kuntz KM, Michaud TL, Jalal H.
"Time Traveling Is Just Too Dangerous" But Some Methods Are Worth Revisiting:
The Advantages of Expected Loss Curves Over Cost-Effectiveness Acceptability
Curves and Frontier. Value Health. 2019;22(5):611-618.
\item Eckermann S, Briggs A, Willan AR. Health technology assessment in the
cost- disutility plane. Med Decis Making. 2008;28(2):172–181.
}
}
\seealso{
\code{\link{plot.exp_loss}}, \code{\link{make_psa_obj}}
}
