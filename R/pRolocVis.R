##' These functions allow one to explore spatial proteomics data interactively. 
##' 
##' The function \code{pRolocVis} is a wrapper for \code{pRolocVis_main}, 
##' \code{pRolocVis_classify} and \code{pRolocVis_compare}.These Shiny apps 
##' allow to explore and analyse interactively spatial proteomics data.
##'  
##' The \code{main} Shiny app allows exploration of quantitative data (1) visually 
##' through Principle Component Analysis (PCA), (2) protein profiles, and (3)
##' a searchable feature data table, allowing visualisation of particular proteins
##' of interest. 
##' 
##' The \code{classify} Shiny app is used to visualise classification results
##' and set user-specified thresholds for sub-cellular location predictions. 
##' 
##' The \code{compare} Shiny app is meant for comparing protein localisation 
##' between two conditions, or two different experiments, replicates etc. 
##' 
##' @title Interactive visualisation of spatial proteomics data 
##' @rdname pRolocVis-apps
##' @param object An instance of class \code{MSnSet}, or a list of \code{MSnSet}
##' objects of length 2 if using "compare" application.
##' @param app The type of application requested: "main",
##' "classify", "compare". The default is "main". See description below.
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
##'   pRolocVis(hyperLOPIT2015)
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
        app <- "main"
    if (missing(fcol) && app != "classify")
        fcol <- "markers"
    if (app == "main")
        pRolocVis_pca(object, fcol = fcol, ...)
    if (app == "classify")
        res <- pRolocVis_classify(object, fcol = fcol,
                                  legend.cex = legend.cex, ...)
    if (app == "compare")
        pRolocVis_compare(object, ...)
    if (app == "legacy")
        pRolocVis_legacy(object, ...)
    return(res)
}
