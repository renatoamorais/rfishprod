% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{rbernoulli}
\alias{rbernoulli}
\title{Random samples from a Bernoulli distribution function}
\usage{
rbernoulli(n, p = 0.5)
}
\arguments{
\item{n}{sample sizes}

\item{p}{the probability of a success, i.e. \code{TRUE}}
}
\value{
logical vector
}
\description{
Generates random samples \code{n} from a Bernoulli distribution with probability \code{p}. This is a copy of \code{\link[purrr]{rbernoulli}}
}
\seealso{
applied within \code{\link{applyMstoch}}
}
