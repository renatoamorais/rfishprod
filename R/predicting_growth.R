
#' @title Predicts standardised growth parameter Kmax for reef fishes
#'
#' @description This function uses xgboost to predict Kmax, a standardised von Bertlanffy Growth parameter, for reef fishes. xgboost is an extremely efficient regression tree boosting system. As default, the function uses the dataset provided by Morais and Bellwood (2018), but other datasets can be supplied. Prediction is attained after executing bootstrapping xgboost estimates. Xgboost predictions are ON AVERAGE very accurate and precise, but are somewhat unstable. Because of that, I recomment any predictions to be bootstrapped at least hundreds of times (I have been using 1000 iterations, which takes something in between a few and many minutes, depending on the machine).
#' 
#' @param traits data frame, intuitively, the trait object for which predictions will be made
#' @param dataset reference data frame from which predictions will be made. If not supplied, will use the the db dataset object from the reference above 
#' @param fmod model formula, should be specified
#' @param params set of parameters to optimise the performance of xgboost. 
#' @param niter number of xgboost models run for the bootstrap procedure
#' @param nrounds maximum number of boosting interactions per model
#' @param verbose should xgboost speak to you? If you do not want you workspace flooded, keep this at 0. Alternatively, check xgboost help file
#' @param print_every a friend of verbose, treat it likewise
#' @param return what type of information to return. With three options: predictions (\code{pred}), relative importance of the variables used to predict (\code{relimp}), xgboost models (\code{models}). Defaults to \code{c('pred', 'relimp', 'models')}, recommended \code{c('pred')}.
#' @param lowq low quantile for predictions, defaults to 0.25
#' @param uppq upper quantile for predictions, defaults to 0.75
#'
#' @return a list including : 1) the original traits data frame with added Kmax predictions and lower and upper quantiles; 2) a table with the relative importance of the variables used in the model; and 3) a list of xgboost models
#'
#' @seealso \code{\link{applyVBGF}}, \code{\link{somaGain}}
#'
#'
#' @references Morais, R.A., and Bellwood, D.R. (2018). Global drivers of reef fish growth. Fish and Fisheries. 19, 874–889. doi:10.1111/faf.12297
#'
#' @export
#' @import xgboost


predKmax <- function(traits, dataset, fmod, params = NULL, niter, nrounds = 150, verbose = 0, print_every = 1000, return = c('pred', 'relimp', 'models'), lowq = 0.25, uppq = 0.75) {

if (identical (dataset, rlang::.data)) {
	
	params <- xgboostparams
	
} else if (is.null (params)) {
	
	params <- list ()
	warning('Consider optimising xgboost parameters with xgb.train')
	
}

if (missing (dataset)) {
	
	dataset <- db
	
}
	
modmatnd <- stats::model.matrix(fmod, data = traits) [, -1]
modmat <- stats::model.matrix(fmod, dataset) [, -1]
	
xgbmod <- list()
rel_imp <- list()
pred.grid <- as.data.frame(matrix(ncol = niter, nrow = nrow(traits), 
								  dimnames = list(NULL, paste0('Boot',1:niter))))


for (i in 1:niter) {
	
	set.seed (i)
	xgbmod[[i]] <- xgboost::xgboost(modmat, label = dataset[, 'Kmax'], nrounds = nrounds, 
							 params = params, verbose = verbose, print_every = print_every)
	rel_imp[[i]] <- xgboost::xgb.importance(colnames (modmat), model = xgbmod[[i]])
	
	pred.grid[, i] <- stats::predict(xgbmod [[i]], newdata = modmatnd)
	
	cat(paste("Bootstrapping the model, round", i), "\n")
	
}

df <- cbind(traits, Kmax = apply(pred.grid, 1, stats::median),
					Kmax_lowq = apply(pred.grid, 1, stats::quantile, probs = lowq),
					Kmax_uppq = apply(pred.grid, 1, stats::quantile, probs = uppq))

ret <- list(pred = df,
			relimp = rel_imp,
			models = xgbmod) [return]

return(ret)
	
}
