% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applying_mortality.R
\name{applyMstoch}
\alias{applyMstoch}
\title{Applies stochastic natural mortality}
\usage{
applyMstoch(M, t = 1)
}
\arguments{
\item{M}{a vector of instantaneous mortality rates, such as outputted by the function \code{\link{predM}}}

\item{t}{numeric, the number of days through which losses due to mortality should be followed. Defaults to 1 day and considers that \code{M} is scaled to 1 day}
}
\value{
a logical vector with the fate of the individual (TRUE = survives, FALSE = dies).
}
\description{
Stochastically draws fate to the individuals, i.e. success = survival, failure = mortality. Uses a Bernoulli distribution to assign fate based on the probability of survival calculated from instantaneous mortality rates (e.g. such as the output of \code{\link{predM}}). Contrarily to \code{\link{somaLoss}} this is easily interpretable at the level of the individual, i.e. survived or not.
}
\references{
Morais RA, and Bellwood DR (2020). Principles for estimating fish productivity on coral reefs. Coral Reefs 39: 1221–1231. DOI: 10.1007/s00338-020-01969-9
}
\seealso{
\code{\link{predM}}, \code{\link{somaLoss}}
}
