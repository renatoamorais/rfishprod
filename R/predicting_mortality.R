
#' @title Predicts natural mortality rates M for reef fishes
#'
#' @description This function uses empirical relationships to predict the likely mortality rates, M, of a fish population or individual (depending on the method, see below). M is the instantaneous mortality rate, the exponent describing the decreasing number of individuals from a cohort through time and, by extension, also describing the cumulative per capita mortality rate of an individual.
#'
#' @param Lmeas a vector of fish sizes, for example, as derived from field data
#' @param t numeric, the number of days through which the growth trajectory of the fishes in the sample should be followed. Defaults to 1 day. In most of the fisheries literature, M is scaled by year instead (i.e. use 365 in that case)
#' @param Lmax a vector of fish maximum species lengths, philosophically analogous to Linf
#' @param Kmax a vector of fish Kmax, philosophically analogous to K. Should be as outputted by the function \code{\link{predKmax}} 
#' @param Lr a vector of sizes at settlement. Only relevant for \code{method = 'Function'}
#' @param temp a vector of environmental temperatures to which the fish are exposed. Only relevant for \code{method = c('Pauly', 'Function')}
#' @param p proportion of the body size range of a species to scale the function to be applied to M at the species-population level. Defaults to 0.5, meaning that M at the individual level is equal to M at the species/population level for fishes in the mid point of their body size range
#' @param method method to be used for predicting mortality rates. Can be either \code{'Pauly'}, \code{'Gislason'} or \code{'Function'}. Pauly's equation predicts M at the species/population level from Linf (Lmax here), K (Kmax here) and temperature. Gislason (et al.)'s equation predicts M at the individual-level by incorporating individual size (Lmeas), Lmax and Kmax. The 'Function' method (see Morais and Bellwood, submitted) includes a functional relationship with individual body size to the population-level estimate of M from Pauly. Because there is currently no recommendation on which method should be used for estimating size-specific mortality rates, \code{'Pauly'} is the default, although it should (rather paradoxically) potentially NOT be used.
#'
#' @seealso \code{\link{somaLoss}}, \code{\link{applyMstoch}}
#'
#' @references Morais, R.A., and Bellwood, D.R. Quantifying fish productivity, an emergent tool for managing coral reef ecosystem services. Submitted
#' @references Gislason H, Daan N, Rice JC, and Pope JG. 2010. Size, growth, temperature and the natural mortality of marine fish. Fish Fish 11: 149–58
#' @references Pauly, D. (1980). On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks. ICES J. Mar. Sci. 39, 175–192	
#'
#' @export


predM <- function (Lmeas, t = 1, Lmax, Kmax, Lr, temp, p = 0.5, method = c('Pauly')) {
	
	if (method == 'Gislason') {
		
	logM <- 0.55 - (1.61 * log (Lmeas)) + (1.44 * log (Lmax)) + log (Kmax)
	Mind <- exp (logM) * (t/365)
	return(Mind)	
		
	} else {
	
	logMpop <- (-0.0066 - (0.279 * log(Lmax)) + 0.6543 * log(Kmax) + 0.4634 * log(temp))
	Mpop <- exp (logMpop) * (t/365)	
	
	} 
	
	if (method == 'Pauly') {
	
	return(Mpop)
		
	} else if (method == 'Function') {
		
	Lmed <- (Lmax + Lr) * p
	Mind <- Mpop * (((Lmeas - Lr) / Lmed) ^ -0.75)
	return(Mind)
		
	} else {
		
		stop('Method not recognised. Please, pick one of the three valid ones (Pauly, Gislason or Function).')
		
	}
		
}
