% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxiliary.R
\name{mkfact}
\alias{mkfact}
\title{Standardise factors}
\usage{
mkfact(df, var, refdf)
}
\arguments{
\item{df}{data frame hosting the variable to be standardised.}

\item{var}{variable to be standardised.}

\item{refdf}{reference data frame where to extract levels to standardise \code{var}.}
}
\value{
a factor in \code{df} with the same levels of \code{refdf}
}
\description{
Quickly standardises factors of a variable on a data frame based on a reference dataframe.
}
