##' pRoloc interactive visualisation
##' 
##' @title Visualise your pRoloc data
##' @param object An instance of class \code{MSnSet}, of list of \code{MSnSet}
##' objects of length 2 if using dynamic application.
##' @param what The type of application requested: "pca", "profiles",
##' "classification", "dynamic". The default is "pca". See description below.
##' @param fcol The feature meta data column containing the 
##' classification result. 
##' @param fig.height Height of the figure. Default is \code{"600px"}.
##' @param fig.width Width of the figure. Default is \code{"600px"}.
##' @param legend.width Width of the legend. Default is \code{"100\%"}.
##' @param legend.cex Character expansion for the vignette
##' labels. Default is 1.
##' @param nchar Maximum number of characters if the markers class
##' names, before their names are truncated. Default is 10.
##' @param all If there are more than 10 clusters, only the first
##' three are discplayed on start-up, unless \code{all} is set to
##' \code{TRUE}. Default is \code{FALSE}.
##' @param ... Additional parameters that can be used to choose the
##' dimentionality reduction method, as defined in
##' \code{\link{plot2D}}.
##' @author Laurent Gatto and Lisa Breckels
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(dunkley2006)
##' ## markers matrix ecoding
##' dunkley2006 <- mrkVecToMat(dunkley2006)
##' ## order the fractions 
##' dunkley2006 <- dunkley2006[, order(dunkley2006$fraction)]
##' if (interactive())
##'   pRolocVis(dunkley2006, what = "pca")


## Note remove code to check scol and mcol object etc. inside functions? 
## However if I do we can not call these separately

pRolocVis <- function(object,
                      what = c("pca", "profiles", "classification", "dynamic"),
                      fcol,
                      scol,
                      mcol,
                      legend.cex = 1,
                      legend.ncol = 1,
                      fig.height = "600px",
                      fig.width = "100%",
                      legend.width = "100%",
                      all = FALSE,
                      nchar = 15,
                      ...) {
  
  if (!inherits(object, "MSnSet"))
    stop("The input must be of class MSnSet")
  
  if (missing(fcol)) 
    stop("No fcol specified")
  
  if (missing(what)) 
    what = "pca"
  
  if (what == "pca") {
    pRolocVis_pca(object, fcol = fcol,
                  fig.height = fig.height,
                  fig.width = fig.width,
                  legend.width = legend.width,
                  legend.cex = legend.cex,
                  nchar = nchar,
                  all = all,
                  ...)
  }
  
  if (what == "profiles") {
    pRolocVis_profiles(object, fcol = fcol,
                       legend.cex = legend.cex,
                       legend.ncol = 1,
                       all = all,
                       ...)
    
  }
  
  if (what == "classification") {
    if (missing(scol)) 
      scol <- paste0(fcol, ".scores")
    if (missing(mcol))
      mcol = "markers"
    pRolocVis_classify(object,
                       fcol = fcol,
                       mcol = mcol,
                       legend.cex = legend.cex,
                       legend.ncol = 1,
                       ...)
  }
  
  if (what == "dynamic") {
    pRoloc_comp(object, ...)
  }
}