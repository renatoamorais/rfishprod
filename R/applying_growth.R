
#' @title Applies VBGF to fish length data
#'
#' @description This function applies the von Bertalanffy Growth Function to fish length data as taken from fish counts, e.g. underwater visual surveys. The von Bertalanffy Growth function describes the growth trajectory of an animal given its population-level, somatic growth parameters, K and Linf. For non-lethally sampled fishes, the only way to ascertain individual age is through the hypothesis that body size can be used as an accurate predictor of age IF the fish is still in the growth phase. This 'operational age' represents the relative likely age of a fish, given its body size. The function first estimates the operational age from field length data and from the growth parameters (K and Linf, which, for the purpose of this package are rescaled to be Kmax and Lmax, see Morais and Bellwood (2018)), and then runs the classic VBGF to determine the length after a period of growing. A rarely appreciated aspect of the VBGF is the parameter L0, i.e., the length of the fish at age 0. This parameter can bee interpreted as the size at settlement and, at least for reef fishes, this seems to be an intuitive and effective way of constraining the curve to cross the ordinate (y) axis in a realistic point. From \code{v0.0.2}, two methods are available: using L0 or t0 to define the trajectory. The consequences of using either to estimates of prod are minimal (if any) as this will impact age estimates above all; L0 is just a more intuitive way of specifying the VBGF model.
#'
#' @param Lmeas a vector of fish lengths, for example, as derived from field data
#' @param t numeric, the number of days through which the growth trajectory of the fishes in the sample should be followed. Defaults to 1 day.
#' @param Lmax a vector of fish maximum species lengths, philosophically analogous to Linf
#' @param Kmax a vector of fish Kmax, philosophically analogous to K. Should be as outputted by the function predKmax 
#' @param L0 a vector with the theoretical size at age 0 for all fish from the data. This parameter can be interpreted as the size at settlement for reef fishes (see Choat and Robertson 2002, for example). A list of settlement sizes for reef fish species can be found in Grutter et al. 2017. If the user should provides L0, the original VBGM parameterisation (which specifies the intercept L0) will be triggered. Not providing it or providing t0 will trigger the usual VBGM parameterisation (which oncludes t0 as a modulator of the time exponent) .
#' @param t0 a vector with the theoretical age at size 0 for all fish from the data. As discussed before (multiple times, see a review in Morais and Bellwood 2018), this parameter has no biological meaning. Often constrained to 0 or (recommended) by using settlement size (see above at L0), instead. For reef fishes it is normally larger than -0.5 (see argument \code{t0lowbound}). If neither L0 nor t0 are supplied, calculated using the equation from Pauly (1979); re-scaled according to this (somewhat arbitrary) boundary as below.
#' @param t0lowbound lower bound of t0 set as default to -0.5 a (somewhat) arbitrary value that seems to make sense for reef fishes (e.g. see Choat and Robertson 2002).
#' @param silent would you like a function that talks to you? Here it defaults to \code{silent = TRUE}, i.e. maybe I would!
#'
#' @return a vector of fish lengths represeting the size of the individuals after growing for a period of \code{t} days
#'
#' @seealso \code{\link{predKmax}}, \code{\link{somaGain}}
#'
#' @references Choat JH and Robertson, DR (2002). Age-based studies. In P Sale (Ed.), Coral reef fishes: Dynamics and diversity in a complex ecosystem (pp.57–80). San Diego, CA: Academic Press. https://doi.org/10.1016/B978-012615185-5/50005-0
#' @references Depczynski M, Fulton CJ, Marnane MJ, Bellwood DR (2007). Life history patterns shape energy allocation among fishes on coral reefs. Oecologia 153, 111–120. doi:10.1007/s00442-007-0714-2
#' @references Grutter AS, Blomberg SP, Fargher B, Kuris AM, McCormick MI, Warner RR (2017) Size-related mortality due to gnathiid isopod micropredation correlates with settlement size in coral reef fishes. Coral Reefs, 36, 549–559
#' @references Morais RA, and Bellwood DR (2020). Principles for estimating fish productivity on coral reefs. Coral Reefs, 39, 1221–1231. DOI: 10.1007/s00338-020-01969-9
#' @references Morais RA, and Bellwood DR (2018). Global drivers of reef fish growth. Fish and Fisheries 19, 874–889. doi:10.1111/faf.12297
#' @references Pauly D (1979). Gill size and temperature as governing factors in fish growth: A generalization of von Bertalanffy′s growth formula. Berichte aus dem Institut für Meereskunde Kiel, 63, 1–156
#'
#' @export


applyVBGF <- function (Lmeas, t = 1, Lmax, Kmax, L0, t0, t0lowbound = -0.5,  silent = T) {

	Li <- ifelse ((Lmeas / Lmax) > 1, 1, (Lmeas / Lmax))
	
	if(missing(L0)){	
		if(missing(t0)){
			t0 <- -10 ^ (-0.3922 - (0.2752 * log10(Lmax)) - 1.038 * log10(Kmax))
			t0 <- scales::rescale(t0, to = c(t0lowbound, max(t0)))
			}
		L0 <- Lmax * (1 - exp(Kmax * t0))
		L0 <- ifelse((L0 / Lmeas) >= 1, Lmeas, L0)
		tt <- (1 / Kmax) * log((Lmax - L0) / ((1 - Li) * Lmax)) + (t / 365)
		Lgr <- Lmax * (1 - exp(-Kmax * (tt - t0)))
	} else {
		L0 <- ifelse((L0 / Lmeas) >= 1, Lmeas, L0)
		tt <- (1 / Kmax) * log((Lmax - L0) / ((1 - Li) * Lmax)) + (t / 365)
		Lgr <- L0 + (Lmax - L0) * (1 - exp(-Kmax * tt))
	}

	ctrgr(Lmeas = Lmeas, Lgr = Lgr, silent = silent)	
	
	return(Lgr)
	
}




#' @title Expected somatic growth in weight
#'
#' @description This function calculates the expected somatic growth of a fish in weight units. It uses \code{applyVBGF} to apply the von Bertalanffy Growth Function to fish length data and then uses length-weight relationships to estimate the amount of weight that this somatic growth generated. It is the basis for productivity estimates.
#'
#' @param a a vector of length-weight parameter 'a's, i.e. the intercept of the Power law
#' @param b a vector of length-weight parameter 'b's, i.e. the exponent of the Power law
#' @param Lmeas a vector of fish lengths, for example, as derived from field data
#' @param t numeric, the number of days through which the growth trajectory of the fishes in the sample should be followed. Defaults to 1 day.
#' @param Lmax a vector of fish maximum species lengths, philosophically analogous to Linf
#' @param Kmax a vector of fish Kmax, philosophically analogous to K. Should be as outputted by the function \code{\link{predKmax}} 
#' @param t0 a vector with the theoretical size at age 0 for all fish from the data. As discussed before (multiple times, see a review in Morais and Bellwood 2018), has little biological meaning. Often constrained at 0 or using settlement size. For reef fishes it is normally larger than -0.5 (see argument \code{t0lowbound}).
#' @param t0lowbound lower bound of t0 set as default to -0.5 a (somewhat) arbitrary value that seems to make sense for reef fishes.
#' @param silent would you like a function that talks to you? Here it defaults to \code{silent = TRUE}, i.e. no I would not!
#'
#' @return a vector of weight increments, represeting the net growth of individuals for a period of \code{t} days
#'
#' @seealso \code{\link{applyVBGF}}, \code{\link{predKmax}}
#'
#' @export


somaGain <- function (a, b, Lmeas, t = 1, Lmax, Kmax, t0, t0lowbound = -0.5,  silent = T) {
	
	Lgr <- applyVBGF (Lmeas = Lmeas, t = t, Lmax = Lmax, Kmax = Kmax, t0 = t0, t0lowbound = t0lowbound, silent = silent)
	Wei <- a * (Lmeas ^ b)
	Wgr <- a * (Lgr ^ b)
	
	Wgr - Wei 
		
}
