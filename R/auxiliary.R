

#' @title Standardise factors
#'
#' @description Quickly standardises factors of a variable on a data frame based on a reference dataframe.
#'
#' @param df data frame hosting the variable to be standardised.
#' @param var variable to be standardised.
#' @param refdf reference data frame where to extract levels to standardise \code{var}.
#'
#' @return a factor in \code{df} with the same levels of \code{refdf}
#'
#' @export


mkfact <- function (df, var, refdf)  {
	
	if (is.null(levels (refdf[, var]))) { 
		stop (paste ('Variable', var, 'has no detectable levels. Either this is not the correct variable, or tidytrait could not add levels to it.'))
	} else {
	
		factor (df[, var], levels (refdf[, var]))
	
	}

}



#' @title Apply standardised factors to growth prediction data frame
#'
#' @description Makes sure that the trait object (species for which to predict growth) has factors perfectly matching the db object (species from which to predict growth)
#'
#' @param traits data frame, intuitively the trait object for which predictions will be made
#' @param dataset reference data frame, intuitively the db dataset object from which predictions will be made
#' @param levels.dataset Includes variable levels required later on for predicting growth. Defaults to TRUE
#' @param include.method Should include a variable for the method used to derive the growth curves? Because db in Morais and Bellwood (2018) includes growth curves obtained from different methods, this argument makes sure that predictions are made based on the curves obtained by ageing from otoliths only. Defaults to \code{FALSE}, assuming that the variable is already present in traits
#'
#' @return a tidied up traits data frame with factors ready to be used to predict growth
#'
#' @examples
#' data (db)
#' levels (db$Diet)
#'
#' @references Morais, R.A., and Bellwood, D.R. (2018). Global drivers of reef fish growth. Fish and Fisheries. 19, 874–889. doi:10.1111/faf.12297
#'
#' @export


tidytrait <- function (traits, dataset, levels.dataset = TRUE, include.method = FALSE) {
	
varmod <- c ('Diet', 'Position', 'Method')
	
	if (levels.dataset == T) {
		
	dataset[,'Diet'] <- factor(dataset[,'Diet'], 
		c("HerMac", "HerDet", "Omnivr", "Plktiv", "InvSes", "InvMob", "FisCep"))
	dataset[,'Position'] <- factor(dataset[,'Position'], 
		c("PelgAs", "PelgDw", "BtPlAs", "BtPlDw", "BnthAs", "BnthDw"))
	dataset[,'Method'] <- factor(dataset[,'Method'], 
		c("LenFrq", "MarkRc", "Otolth", "Unknow", "OthRin", "ScalRi"))
		
	}
	
	if (include.method == T) traits[, 'Method'] <- 'Otolth'
	if (!all(varmod %in% colnames (traits))) {
		absentvar <- varmod [!varmod %in% colnames (traits)]
		stop (paste('The variable', absentvar, 'was not found in the dataset.	'))
	}
		
	traits[,'Diet'] <- mkfact (traits, 'Diet', dataset)
	traits[,'Position'] <- mkfact (traits, 'Position', dataset)
	traits[,'Method'] <- mkfact (traits, 'Method', dataset)
	
	return(traits)
	
}


#' @title Control flow for the applyVBGF function
#'
#' @description Detects if the operational procedure of submitting fishes to the VBGF actually resulted in growth in length (as expected)
#'
#' @param Lmeas a vector of fish sizes, for example, as derived from field data 
#' @param Lgr a vector of fish sizes as the output after applying the function VBGF on Lmeas
#' @param silent would you like a function that talks to you? Defaults to \code{silent = FALSE}, i.e. yes I would!
#'
#' @return a potential error or encouragement message relative to the lengths after applying growth
#'
#' @export


ctrgr <- function (Lmeas, Lgr, silent = FALSE) {
	
	msg <- c("All good mate, your fish are growing!", 
			 "Virtual fish keeper medal to you, your fish are growing!",
			 "This message has a probability of occurrence of only 1% (and your fish are growing)!")
	
	if (any(Lgr < Lmeas)) {	
		
		stop("Fish should not decrease in size (Lgr < Lmeas)!")	
		
		} else if (silent == F) {
		
			print(sample(msg, 1, prob = c(0.495, 0.495, 0.01)))
		
		}

}



#' @title Random samples from a Bernoulli distribution function
#'
#' @description Generates random samples \code{n} from a Bernoulli distribution with probability \code{p}. This is a copy of \code{\link[purrr]{rbernoulli}}
#'
#' @param n sample sizes 
#' @param p the probability of a success, i.e. \code{TRUE}
#'
#' @return logical vector
#'
#' @seealso applied within \code{\link{applyMstoch}}
#'
#' @export


rbernoulli <- function (n, p = 0.5) {
	
	stats::runif(n) > (1 - p)

}