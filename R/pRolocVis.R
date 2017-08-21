##' These functions allow one to explore spatial proteomics data
##' interactively.
##' 
##' The function \code{pRolocVis} is a wrapper for
##' \code{pRolocVis_pca}, \code{pRolocVis_classify},\code{pRolocVis_compare}. 
##' and \code{pRolocVis_aggregate}. These Shiny apps allow to explore and
##' analyse interactively spatial proteomics data.
##'  
##' The \code{pca} Shiny app allows exploration of quantitative data
##' (1) visually through Principle Component Analysis (PCA), (2)
##' protein profiles, and (3) a searchable feature data table,
##' allowing visualisation of particular proteins of interest.
##' 
##' The \code{classify} Shiny app is used to visualise classification
##' results and set user-specified thresholds for sub-cellular
##' location predictions.
##' 
##' The \code{compare} Shiny app is meant for comparing protein
##' localisation between two conditions, or two different experiments,
##' replicates etc. Please note that passing the argument \code{method} 
##' to \code{...} will not work as it is already specified internally.
##' 
##' The \code{aggregation} Shiny app displays a scatter plot of the
##' maximum or mean distances within each feature (e.g. protein group)
##' according to its components (e.g. peptides) defined by the
##' \code{groupBy} argument. A PCA plot of the components is also
##' displayed. It can be used for visualising peptides, PSMs or any
##' other features defined in the feature data of the \code{MSnSet}
##' and their distributions.
##' 
##' @title Interactive visualisation of spatial proteomics data
##' @rdname pRolocVis-apps
##' @param object An instance of class \code{MSnSet}, or an
##'     \code{MSnSetList} of length 2 if using \code{"compare"}
##'     application.
##' @param app The type of application requested: \code{"pca"}
##'     (default), \code{"classify"}, \code{"compare"} or
##'     \code{"aggregate"}. See description below.
##' @param fcol The feature meta-data label (\code{fData} column name)
##'     to be used for colouring. Default is \code{"markers"}. This
##'     will correspond to the prediction column if using "classify",
##'     or the markers (labelled data) to be plotted otherwise. If set
##'     to \code{NULL}, no annotation is expected.
##' @param legend.cex Point character expansion for the the legend.
##'     Default is 1.
##' @param ... Additional parameters passed to \code{plot2D} for the
##'     \code{"pca"} (such as a different dimensionality reduction
##'     technique than PCA, and its arguments), \code{"classify"},
##'     \code{"compare"} apps. For the \code{"aggregate"} app this is
##'     for additional parameters to be passed to
##'     \code{combineFeatures}.
##' @author Laurent Gatto, Lisa Breckels and Thomas Naake
##' @seealso The package vignette: \code{vignette("pRolocGUI")}.
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(hyperLOPIT2015)
##' ## Load the PCA app
##' if (interactive()) {
##'   pRolocVis(hyperLOPIT2015)
##' }
##' 
##' ## Load classification results from hyperLOPIT stored in fData
##' if (interactive()) {
##'   myThreshold <- pRolocVis(hyperLOPIT2015, app = "classify", 
##'                            fcol = "svm.classification", 
##'                            scol = "svm.score")
##'   newPredictions <- getPredictions(hyperLOPIT2015, fcol = "svm.classification", 
##'                                    scol = "svm.score", t = myThreshold)
##' }
##' 
##' ## Visualise the location and distribution of peptides per protein group
##' data("hyperLOPIT2015ms2psm")
##' if (interactive()) {
##'   ## Combine PSM data to peptides
##'   hl <- combineFeatures(hyperLOPIT2015ms2psm, 
##'                         groupBy = fData(hyperLOPIT2015ms2psm)$Sequence, 
##'                         fun = median)
##'   ## Visualise peptides according to protein group
##'   pRolocVis(hl, app = "aggregate", fcol = "markers", 
##'             groupBy = "Protein.Group.Accessions")                    
##' }
pRolocVis <- function(object, app = "pca", fcol, ...) {
  res <- NULL
  app <- match.arg(app, c("pca", "compare", "classify", "aggregate"))
  if (inherits(object, "MSnSetList"))
    app <- "compare"
  if (missing(app))
    app <- "pca"
  if (missing(fcol) && app != "classify")
    fcol <- "markers"
  if (app == "pca")
    pRolocVis_pca(object, fcol, ...)
  if (app == "classify")
    res <- pRolocVis_classify(object, fcol, ...)
  if (app == "compare")
    pRolocVis_compare(object, ...)
  if (app == "aggregate")
    res <- pRolocVis_aggregate(object, fcol, ...)
  invisible(res)
}
