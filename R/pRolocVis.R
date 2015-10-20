##' These functions allow one to explore spatial proteomics data interactively. 
##' 
##' The function \code{pRolocVis} is a wrapper for \code{pRolocVis_pca}, 
##' \code{pRolocVis_profiles}, \code{pRolocVis_classification}, 
##' \code{pRolocVis_compare} and \code{pRolocVis_legacy}.These Shiny apps 
##' allow to explore and analyse interactively spatial proteomics data.
##'  
##' The \code{pca} Shiny app allows exploration of quantitatuve data (1) visually 
##' through Principle Component Analysis (PCA), (2) protein profiles, and (3)
##' a searchable feature data table, allowing visualisation of particualr proteins
##' of interest. 
##' 
##' The \code{profiles} Shiny app allows one to simultaneously view protein
##' profiles and PCA locations of sub-cellular annotated sets of proteins.
##' 
##' The \code{classify} Shiny app is used to visualise classification results
##' and set user-specified thresholds for sub-cellular location predictions. 
##' 
##' The \code{legacy} Shiny app is the original (legacy) \code{pRolocVis} app
##' that appears in older versions of \code{pRolocGUI}, the \code{pca} app has
##' been designed to replace this app. 
##' 
##' The \code{compare} Shiny app is meant for comparing protein localisation 
##' between two conditions, or two different experiments, replicates etc. 
##' 
##' @title Interactive visualisation of `pRoloc` data 
##' @aliases pRolocVis_pca
##' @aliases pRolocVis_profiles
##' @aliases pRolocVis_classify
##' @aliases pRolocVis_compare
##' @aliases pRolocVis_legacy
##' @rdname pRolocVis-apps
##' @param object An instance of class \code{MSnSet}, or a list of \code{MSnSet}
##' objects of length 2 if using "compare" application.
##' @param what The type of application requested: "pca", "profiles",
##' "classify", "compare", "legacy". The default is "pca". See description below.
##' @param fcol The feature meta-data label (fData column name). This will correspond
##' to the prediction column if using "classify", or the markers (labelled data) 
##' to be plotted otherwise.
##' @param legend.cex Character expansion for the vignette
##' labels. Default is 1.
##' @param method An instance of class \code{matrix} where its 
##' rownames must match the object's feature names and represent 
##' a projection of the data in object in two dimensions, as 
##' produced (and invisibly returned) by \code{plot2D}.
##' @param ... Additional parameters.
##' @author Laurent Gatto, Lisa Breckels and Thomas Naake
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(hyperLOPIT2015)
##' if (interactive())
##'   pRolocVis(hyperLOPIT2015, what = "pca")
##'   pRolocVis(hyperLOPIT2015, what = "profiles")
##' ## Classification  
##' opt <- knnOptimisation(hyperLOPIT2015, times = 10)
##' res <- knnClassification(hyperLOPIT2015, opt)
##' if (interactive())
##'   myThreshold <- pRolocVis(res, what = "classify", fcol = "knn")
##'   newPredictions <- getPredictions(res, fcol = "knn", t = myThreshold) 
  
pRolocVis <- function(object, what, fcol, legend.cex = 1, ...) {

  if (missing(what)) 
    what = "pca"
  
  if (missing(fcol) && what != "classify") 
    fcol = "markers"

  if (what == "pca") {
    pRolocVis_pca(object, fcol = fcol, ...)
  }
  
  if (what == "profiles") {
    pRolocVis_profiles(object, fcol = fcol, legend.cex = legend.cex, ...)
    
  }
  
  if (what == "classify") {
    weights <- pRolocVis_classify(object, fcol = fcol,  
                       legend.cex = legend.cex, ...)
    
  }
  
  if (what == "compare") {
    pRolocVis_compare(object, ...)
  }
 
  if (what == "legacy") {
    pRolocVis_legacy(object, ...)
  }
return(weights)
}