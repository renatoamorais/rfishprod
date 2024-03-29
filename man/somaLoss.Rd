% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applying_mortality.R
\name{somaLoss}
\alias{somaLoss}
\title{Expected per capita loss due to natural mortality}
\usage{
somaLoss(M, Wei, Lmeas, a, b, t = 1)
}
\arguments{
\item{M}{a vector of instantaneous mortality rates, such as outputted by the function \code{\link{predM}}}

\item{Wei}{a vector of fish sizes, for example, as derived from field data}

\item{Lmeas}{a vector of fish sizes, for example, as derived from field data. Used only is \code{Wei} is missing.}

\item{a}{a vector of length-weight parameter 'a's, i.e. the intercept of the Power law. Used only is \code{Wei} is missing.}

\item{b}{a vector of length-weight parameter 'b's, i.e. the exponent opredMf the Power law. Used only is \code{Wei} is missing.}

\item{t}{numeric, the number of days through which losses due to mortality should be followed. Defaults to 1 day and considers that \code{M} is scaled to 1 day}
}
\value{
a numeric vector with the per capita expected loss, in weight units, due to natural mortality.
}
\description{
This function calculates the expected per capita loss, in weight units, due to natural mortality. It is based on the instantaneous mortality rate as predicted by \code{\link{predM}}. Although the concept of per capita loss through mortality only makes sense at the population level, the function output this loss at the individual level.
}
\references{
Morais RA, and Bellwood DR (2020). Principles for estimating fish productivity on coral reefs. Coral Reefs 39: 1221–1231. DOI: 10.1007/s00338-020-01969-9
}
\seealso{
\code{\link{predM}}, \code{\link{applyMstoch}}
}
