#' @keywords internal
"_PACKAGE"
#' @name rfishprod-package
#' @docType package
#' @section \code{rfishprod} functions:

#' Use the functions \code{predKmax} and \code{predM} to predict growth and mortality trajectories, respectively. Then, use \code{somaGain}, \code{applyMstoch} and \code{somaLoss} to position your fishes in these trajectories and estimate components of production/productivity!

NULL


.onLoad <- function(libname, pkgname) {
  utils::data(db, package=pkgname)
}


.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion(pkgname)
  packageStartupMessage(paste0("This is rfishprod v",v,'. Please, note that this is still a beta release. However, a proper vignette could come soon(ish)...'))
}

