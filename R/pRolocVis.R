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
##' @title Interactive visualisation of spatial proteomics data 
##' @rdname pRolocVis-apps
##' @param object An instance of class \code{MSnSet}, or a list of \code{MSnSet}
##' objects of length 2 if using "compare" application.
##' @param app The type of application requested: "pca", "profiles",
##' "classify", "compare". The default is "pca". See description below.
##' @param fcol The feature meta-data label (fData column name). This will correspond
##' to the prediction column if using "classify", or the markers (labelled data) 
##' to be plotted otherwise.
##' @param legend.cex Point character expansion for the the legend.
##' Default is 1.
##' @param method Either a \code{character} of a \code{matrix}. When
##' the former, one of \code{"PCA"} (default), \code{"MDS"},
##' \code{"kpca"} or \code{"t-SNE"}, defining if dimensionality
##' reduction is done using principal component analysis (see
##' \code{\link{prcomp}}), classical multidimensional scaling (see
##' \code{\link{cmdscale}}), kernel PCA (see \code{kernlab::kpca}) or
##' t-SNE (see \code{tsne::tsne}). If a \code{matrix} is passed, its rownames
##' must match object's feature names and represent a projection of
##' the data in \code{object} in two dimensions, as produced (and invisibly
##' returned) by \code{plot2D}. This enables to re-generate the figure without
##' computing the dimensionality reduction over and over again, which
##' can be time consuming for certain methods. Available methods are listed
##' in \code{plot2Dmethods}.
##' @param ... Additional parameters.
##' @author Laurent Gatto, Lisa Breckels and Thomas Naake
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(hyperLOPIT2015)
##' if (interactive()) {
##'   pRolocVis(hyperLOPIT2015, app = "pca")
##'   pRolocVis(hyperLOPIT2015, app = "profiles")
##' }
##' ## Load classification results from hyperLOPIT stored in fData
##' if (interactive()) {
##'   myThreshold <- pRolocVis(hyperLOPIT2015, app = "classify", 
##'                            fcol = "svm.classification", 
##'                            scol = "svm.score")
##'   newPredictions <- getPredictions(hyperLOPIT2015, fcol = "svm.classification", 
##'                                    scol = "svm.score", t = myThreshold)
##' }
pRolocVis <- function(object, app, fcol, legend.cex = 1, ...) {
    res <- NULL
    if (missing(app))
        app <- "pca"
    if (missing(fcol) && app != "classify")
        fcol <- "markers"
    if (app == "pca")
        pRolocVis_pca(object, fcol = fcol, ...)
    if (app == "profiles")
        pRolocVis_profiles(object, fcol = fcol, legend.cex = legend.cex, ...)
    if (app == "classify")
        res <- pRolocVis_classify(object, fcol = fcol,
                                  legend.cex = legend.cex, ...)
    if (app == "compare")
        pRolocVis_compare(object, ...)
    if (app == "legacy")
        pRolocVis_legacy(object, ...)
    return(res)
}
