## @title Test for Features of Interests or Collection of Features of Interest
## @description  \code{.areFeaturesOfInterest} tests if an object is of class 
## \code{"FeaturesOfInterest"}, \code{.isFoIColection} if object is of class 
## \code{"FoICollection"}, respectively. Returning \code{TRUE} or \code{FALSE}.
## @return \code{TRUE} or \code{FALSE}.
## @author Thomas Naake <tn299@@cam.ac.uk>
## @usage .areFeaturesOfInterest(x) 
## .isFoICollection(xx)
## @param x An object tested of being of class \code{FeaturesOfInterest}
## @param xx An object tested of being of class \code{FoICollection}
.areFeaturesOfInterest <- function(x)
  inherits(x, "FeaturesOfInterest")
.isFoICollection <- function(xx)
  inherits(xx, "FoICollection")

## @title Return information about set of features
## @description \code{.fnamesFOI} and \code{.descriptionFOI} accept both 
## \code{FeaturesOfInterest} and \code{FoICollection} objects and 
## return \code{featureNames} and \code{description}.
## @param x An object of class \code{FeaturesOfInterest} or 
## \code{FoICollection}
## @param flist If \code{TRUE}, .fnamesFOI returns list-like object, if 
## \code{FALSE} it will return a vector. Set \code{TRUE} or \code{FALSE}.
.fnamesFOI <- function(x, flist=TRUE) {
  if(.areFeaturesOfInterest(x)) 
    return(foi(x))
  else
    if (!isTRUE(flist)) {
      y <- vector("character", length=0)
      for (i in 1:length(x)) {
        y <- c(y, foi(foi(x)[[i]]))}
    } else {
      y <- list()
      for (i in 1:length(x)) {
        y[[i]] <- foi(foi(x)[[i]])}
      } 
      return(y)
}

.descriptionFOI <- function(x) {
  if(.areFeaturesOfInterest(x))
    return(description(x))
  else
    y <- vector("character", length=0)
    for (i in 1:length(x)) {
      y <- c(y, description(foi(x)[[i]]))
    }
    return(y)
}

## @title .show.FOI
## @description A function which gives a result similar to executing
## \code{x} when \code{x} is of class {"FeaturesOfInterest"} with 
## information about the number of identical feature names in the feature set
## or a feature set in a collection of feature sets with regard to a specified 
## \code{\link{"MSnSet"}}. It is a helper function used in pRolocGUI 
## to display saved search results. 
## @usage .show.FOI(x, tan2009r1)
## @author Thomas Naake <tn299@@cam.ac.uk>
## @param x An object of class \code{FeaturesOfInterest} or \code{FoICollection}
## @param fMSnSet An object of class MSnSet
## @param index An integer n referring to the n-th \code{FeaturesOfInterest} 
## object in an \code{FoICollection}
.showFOI <- function(x, fMSnSet, index=1) {
  if (.areFeaturesOfInterest(x)) 
    showFOI <- capture.output(x)
  else
    showFOI <- capture.output(show(foi(x)[[index]]))
  showFOI <- c(showFOI, "Therefrom in selected MSnSet:")
  if (.areFeaturesOfInterest(x))
    showFOI <- c(showFOI, fnamesIn(x, fMSnSet, TRUE))
  else
    showFOI <- c(showFOI, fnamesIn(foi(x)[[index]], fMSnSet, TRUE))
  return(showFOI)
}


## @title Helper functions for \code{reactivePoll} in \code{pRolocGUI}
## @name digestFOI readSR descrFOI
## @aliases .digestFOI .readSR .descrFOI
## @description \code{.digestFOI} is used as the \code{checkFunc}
## argument, \code{.readSR} and \code{.descrFOI} as \code{valueFunc} in
## \code{reactivePoll}
.digestFOI <- function() {
  if (exists("pRolocGUI_SearchResults", .GlobalEnv))
    digest(get("pRolocGUI_SearchResults", .GlobalEnv))
  else 
    return(NULL)
}

.readSR <- function() {
  if (exists("pRolocGUI_SearchResults", .GlobalEnv))
    get("pRolocGUI_SearchResults", .GlobalEnv)
  else
      return(NULL)
}

.descrFOI <- function() {
  if (exists("pRolocGUI_SearchResults", envir = .GlobalEnv))
    substring(.descriptionFOI(get("pRolocGUI_SearchResults", .GlobalEnv)), 1, 15)
  else
    return(NULL)
}

## @title get name of object parsed to pRolocVis
##nameObj <- function(obj)
##  MSnbase:::getVariableName(match.call(), "obj")

##@title Function for PCA plot
## PCA plot, add legend or not, add points via function 
## highlightOnPlot
.plotPCA <- function(data, fcolours, fcex, xrange, yrange,
                     sb, PCAn1, PCAn2, legend, legendpos,
                     sI, cIS) {
  par(mfrow=c(1, 1))
  
  if (length(fcolours)) {
    if (fcolours %in% fvarLabels(data))
      colour <- fcolours
    else
      colour <- NULL
  }
  
  if (length(fcex)) {
    if (fcex %in% fvarLabels(data))
      fcex <- fData(data)[, fcex]
    else
      fcex <- 1  ## as.numeric(fcex)
  } 
  else
    fcex <- 1
  
  if (!is.null(xrange)) { 
    if (is.null(sb) || 
          sb == "none") 
      ## create plot2D and assign reactive variables to 
      ## arguments, do not assign fpch (no symboltypes 
      ## are plotted)
      plot2D(data, fcol = colour,
             xlim = c(xrange[1], xrange[2]),
             ylim = c(yrange[1], yrange[2]),
             dims = c(as.numeric(PCAn1),
                      as.numeric(PCAn2)),
             cex = fcex)
    else
      ## create plot2D and assign reactive variables to 
      ## arguments take input$fsymboltype for symboltype
      plot2D(data,fcol = colour, fpch = input$fsymboltype,
             xlim = c(xrange[1], xrange[2]),
             ylim = c(yrange[1], yrange[2]),
             dims = c(as.numeric(PCAn1),
                      as.numeric(PCAn2)),
             cex = fcex)
  }
  
  if(length(legend)) 
    if (fcolours %in% fvarLabels(data) && 
          legend)
      ## add a legend to the plot with reactive 
      ## variable as arguments
      addLegend(data, fcol = colour, 
                where = legendpos,
                bty = "n", cex = 1)
  
  if(length(sI)) {
    if(length(cIS)) {
      foiPCA <- FeaturesOfInterest(description = "hoP",
                                   fnames = featureNames(data)[c(sI)],
                                   object=data)
      highlightOnPlot(data, foiPCA, 
                      args = list(
                        fcol = fvarLabels(data)[1],
                        xlim = c(xrange[1], 
                                 xrange[2]),
                        ylim = c(yrange[1], 
                                 yrange[2]),
                        dims = c(as.numeric(PCAn1),
                                 as.numeric(PCAn2))),
                      col="black", cex=1.5)
    }
  }
} ## end function .plotPCA

