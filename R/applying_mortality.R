#' @title Expected per capita loss due to natural mortality
#'
#' @description This function calculates the expected per capita loss, in weight units, due to natural mortality. It is based on the instantaneous mortality rate as predicted by \code{\link{predM}}. Although the concept of per capita loss through mortality only makes sense at the population level, the function output this loss at the individual level.
#'
#' @param M a vector of instantaneous mortality rates, such as outputted by the function \code{\link{predM}}
#' @param Wei a vector of fish sizes, for example, as derived from field data
#' @param Lmeas a vector of fish sizes, for example, as derived from field data. Used only is \code{Wei} is missing.
#' @param a a vector of length-weight parameter 'a's, i.e. the intercept of the Power law. Used only is \code{Wei} is missing.
#' @param b a vector of length-weight parameter 'b's, i.e. the exponent opredMf the Power law. Used only is \code{Wei} is missing.
#' @param t numeric, the number of days through which losses due to mortality should be followed. Defaults to 1 day and considers that \code{M} is scaled to 1 day
#'
#' @return a numeric vector with the per capita expected loss, in weight units, due to natural mortality.
#'
#' @seealso \code{\link{predM}}, \code{\link{applyMstoch}} 
#'
#' @references Morais, R.A., and Bellwood, D.R. Principles for estimating fish productivity on coral reefs. Coral Reefs. In press. DOI: 10.1007/s00338-020-01969-9
#' @export

somaLoss <- function (M, Wei, Lmeas, a, b, t = 1) {
	
	if (missing(Wei) & missing(Lmeas)) {
		stop ('Function needs either individual weights or lengths plus length-weight parameters')
	} else if (missing(Wei) & !missing(Lmeas) & (missing(a) | missing(b))) {
		stop ('Please, provide both length-weight parameters')
	} else if (missing(Wei) & !missing(Lmeas) & !missing(a) & !missing(b)){
		Wei <- a * (Lmeas ^ b)
	}
	
	Mt <- M * t	
	(1 - exp (-Mt)) * Wei
	
}


#' @title Applies stochastic natural mortality
#'
#' @description Stochastically draws fate to the individuals, i.e. success = survival, failure = mortality. Uses a Bernoulli distribution to assign fate based on the probability of survival calculated from instantaneous mortality rates (e.g. such as the output of \code{\link{predM}}). Contrarily to \code{\link{somaLoss}} this is easily interpretable at the level of the individual, i.e. survived or not.
#'
#' @param M a vector of instantaneous mortality rates, such as outputted by the function \code{\link{predM}}
#' @param t numeric, the number of days through which losses due to mortality should be followed. Defaults to 1 day and considers that \code{M} is scaled to 1 day
#'
#' @return a logical vector with the fate of the individual (TRUE = survives, FALSE = dies).
#'
#' @seealso \code{\link{predM}}, \code{\link{somaLoss}}
#'
#' @references Morais, R.A., and Bellwood, D.R. Principles for estimating fish productivity on coral reefs. Coral Reefs. In press. DOI: 10.1007/s00338-020-01969-9
#' @export

applyMstoch <- function (M, t = 1) {
	
	Mt <- M * t
	ps <- exp (-Mt)
	
	rbernoulli (length (ps), p = ps)
	
	
}