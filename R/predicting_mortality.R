
#' @title Predicts natural mortality rates Z/M for reef fishes
#'
#' @description This function uses empirical relationships to predict the expected natural mortality rates, Z, of a fish population or individual (depending on the method, see below). Z is the instantaneous mortality rate, the exponent describing the decreasing number of individuals from a cohort through time and, by extension, also describing the cumulative per capita mortality rate of an individual. Where fishing is absent, the instantaneous total mortality rate, M, is equal to Z, i.e. instantaneous fishing mortality rate, F, is equal zero.
#'
#' @param Lmeas a vector of fish sizes, for example, as derived from field data
#' @param t numeric, the number of days through which the growth trajectory of the fishes in the sample should be followed. Defaults to 1 day. In most of the fisheries literature, M is scaled by year instead (i.e. use 365 in that case)
#' @param Lmax a vector of fish maximum species lengths, philosophically analogous to Linf
#' @param Kmax a vector of fish Kmax, philosophically analogous to K. Should be as outputted by the function \code{\link{predKmax}} 
#' @param temp a vector of environmental temperatures to which the fish are exposed. Only relevant for \code{method = c('Pauly', 'Function')}
#' @param Lr a vector of sizes at settlement. Only relevant for \code{method = 'Function'}
#' @param exp exponent of the relationship between mortality and length. Theoretically and empirically presumed to be either -0.75 (Jørgensen and Holt 2013) or -1 (Lorenzen et al. 2022). Set as the empirical evidence within this range from Lorenzen et al. 2022: -0.91 (see Table 2, model 3 'All populations' in this ref). Only relevant for \code{method = 'Function'}
#' @param p proportion of the body size range of a species to scale the function to be applied to M at the species-population level. Defaults to 0.5, meaning that M at the individual level is equal to M at the species/population level for fishes in the mid point of their body size range. Only relevant for \code{method = 'Function'}
#' @param method method to be used for predicting mortality rates. Can be \code{'Pauly'}, \code{'Gislason'}, \code{'Lorenzen'} or \code{'Function'}. Pauly's equation predicts M (or Z, see description) at the species/population level from Linf (Lmax here), K (Kmax here) and temperature. Gislason (et al.)'s and Lorenzen (et al.)'s equations predict M at the individual-level by incorporating individual size (Lmeas), Lmax and Kmax. The 'Function' method (see Morais and Bellwood 2020) is my attempt to include a functional relationship with individual body size to the population-level estimate of M from Pauly. Method \code{'Function'} is yet to be properly validated (see Supporting Information from Morais and Bellwood 2020). Method \code{'Pauly'} ignores ontogenetic changes in mortality risk. Therefore, although based on a limited sample universe (particularly of reef fishes), methods \code{'Gislason'} and \code{'Lorenzen'} are temporarily preferred (the latest is set as the default as it is the most recent). Expect that this may change to \code{'Function'} or another method sometime in the future.
#'
#' @seealso \code{\link{somaLoss}}, \code{\link{applyMstoch}}
#'
#' @references Morais RA, and Bellwood DR (2020). Principles for estimating fish productivity on coral reefs. Coral Reefs 39: 1221–1231. DOI: 10.1007/s00338-020-01969-9
#' @references Lorenzen K, Camp EV, and Garlock TM (2022). Natural mortality and body size in fish populations. Fish Res 11: 149–58
#' @references Gislason H, Daan N, Rice JC, and Pope JG (2010). Size, growth, temperature and the natural mortality of marine fish. Fish Fish 11: 149–58
#' @references Pauly D (1980). On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks. ICES J Mar Sci 39: 175–192	
#' @references Jørgensen C, Holt RE (2013). Natural mortality: its ecology, how it shapes fish life histories, and why it may be increased by fishing. J Sea Res 75:8–18
#' @export


predM <- function (Lmeas, t = 1, Lmax, Kmax, temp, Lr, p = 0.5, exp = -0.91, method = c('Lorenzen')) {
	
	if (method == 'Lorenzen') {
	
	logM <- 0.28 - (1.30 * log(Lmeas/Lmax)) + (1.08 * log(Kmax))
	Mind <- exp (logM) * (t/365)
	return(Mind)
	
	}
	
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
	Mind <- Mpop * (((Lmeas - Lr) / Lmed) ^ exp)
	return(Mind)
		
	} else {
		
		stop('Method not recognised. Please, pick one of the valid methods (Lorenzen, Gislason, Pauly or Function).')
		
	}
		
}
