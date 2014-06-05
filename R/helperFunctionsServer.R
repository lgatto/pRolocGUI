#############################
## helper functions server ##
#############################

## START: Display selection ##

## A helper function to select the checkbox of "PCA" in the 
## Display selection widget, used in observer for assigning to dSelect$PCA
.selPCA <- function(dPCA, PCAclick, protPCA) {
    if (is.null(PCAclick) || is.null(protPCA))
        dPCA <- NULL
    else {
        isolate({
            PCAclick
            dPCA <- "mousePCA"
        })
    }
    ans <- unique(dPCA)
    return(ans)
}

## A helper function to select the checkbox of "protein profiles" in the 
## Display selection widget, used in observer for assigning to dSelect$PCA
.selPlotDist <- function(dplotDist, plotDistclick, protPlotDist) {
    if (is.null(plotDistclick) || 
            is.null(protPlotDist))
        dplotDist <- NULL
    else {
        isolate({
            plotDistclick
            dplotDist <- "mousePlotDist"
        })
    }
    ans <- unique(dplotDist)
    return(ans)
}

## A helper function to select the checkbox of "query" in the 
## Display selection widget, used in observer for assigning to dSelect$text
.selText <- function(dtext, saveText, resetMult, protText) {
    if (is.null(saveText) || saveText == 0)
        dtext <- NULL
    else  {
        dtext <- NULL
        isolate({
            isolate(saveText)
            if (saveText > 0)
            dtext <- "text"
        })
        
        isolate({
            resetMult
            if (resetMult > 0 && length(protText) < 1)
                dtext <- NULL
        })      
    }
    ans <- unique(dtext)
    return(ans)
}

## A function to forward indices of selected features to several 
## reactive expressions
.sI <- function(cIS, tagSelectList, protText, 
                    protPCA, protPlotDist, protSearch) {
    ans <- NULL
    if ("text" %in% cIS)
        ans <- c(ans, protText)
    if ("mousePCA" %in% cIS)
        ans <- c(ans, protPCA)
    if ("mousePlotDist" %in% cIS)
        ans <- c(ans, protPlotDist)
    if ("savedSearches" %in% cIS && !is.null(tagSelectList))
        ans <- c(ans, protSearch)
    unique(ans)
}

## A helper function to subset the selection in the query when entering 
## a search string
.sRsubset <- function(data, search, levelSearch) {
    subset(
        (
            if(search != "protein")
                names(table(fData(data)[search]))
            else
                rownames(data)
        ), 
        grepl(levelSearch,
              if(search != "protein")
                  names(table(fData(data)[search]))
              else
                  rownames(data)
        )
    )
}

## END: Display selection ##



## START: TAB Search ##

## point size
.fcex <- function(data) {
    ## check for numericcolums in fData(data) 
    colNum <- which(sapply(fData(data), is.numeric))
    ## write indices in vector colNum
    colNum <- as.vector(colNum)
    if (length(colNum))
        ## return fvarLabels of numeric colums 
        fvarLabels(data)[colNum]
}

## values PCA
.vPCA <- function(data, PCAn1, PCAn2) {
    if (!is.null(data) && !is.null(PCAn1))
        plot2D(data, fcol=NULL,
               dims=c(as.numeric(PCAn1),
                      as.numeric(PCAn2)), 
               plot=FALSE)
}

## UI for colours
.colourPCAUI <- function(data) {
    if (!is.null(data))
        selectInput("fcolours", "colour", c("none",fvarLabels(data)),
            selected="none")
}

## UI for symbol type
.symbolPCAUI <- function(data, colours) {
    if (!is.null(colours) && 
            colours %in% fvarLabels(data)) 
        selectInput("fsymboltype", "symbol type", 
                    c("none", fvarLabels(data)),
                    selected="none")
}

## UI for point size
.fcexPCAUI <- function(data, colours) {
    ## initially !length(input$fcolours)
    ## to avoid an error message we have an outer if statement
    ## only show when there are numeric columns in fData (.fcex())
    if (length(colours) && length(.fcex(data))) 
        if (colours != "none")
            selectInput("fcex", "point size", c("1", .fcex(data)),
                        selected = "1")
}
## END: TAB Search


## Returns the feature names of the FeaturesOfInterest of
## FoICollection provided as input. If flist is TRUE, the 
## output is listed in the latter case.
.fnamesFOI <- function(x, flist=TRUE) {
    if (inherits(x, "FeaturesOfInterest")) {
        ans <- foi(x)
    } else {
        ans <- lapply(foi(x), foi)
        if (!flist) ans <- unlist(ans)      
    }
    return(ans)
}

## Returns information about FoICollection or FeaturesOfInterest
## and the number of features present in the MSnSet
.showFOI <- function(x, fMSnSet, index=1) {
    if (inherits(x, "FoICollection")) {
        n <- fnamesIn(foi(x)[[index]], fMSnSet, TRUE)
        showFOI <- c(capture.output(show(foi(x)[[index]])),
                    paste("Therefrom in selected MSnSet:", n))
    } 
    else { "FeaturesOfInterest"
        n <- fnamesIn(x, fMSnSet, TRUE)
        showFOI <- c(capture.output(show(x)),
                    paste("Therefrom in selected MSnSet:", n))
    }
    return(showFOI)
}

## a helper function for plotting the PCA plot and highlighting
## FeaturesOfInterest using highlightOnPlot
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
        if (is.null(sb) || sb == "none") 
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
            plot2D(data,fcol = colour, fpch = sb,
                xlim = c(xrange[1], xrange[2]),
                ylim = c(yrange[1], yrange[2]),
                dims = c(as.numeric(PCAn1),
                    as.numeric(PCAn2)),
                cex = fcex)
    }
    
    if (length(legend)) 
        if (fcolours %in% fvarLabels(data) && legend)
            ## add a legend to the plot with reactive 
            ## variable as arguments
            addLegend(data, fcol = colour, 
                where = legendpos,
                bty = "n", cex = 1)
    
    if (length(sI) && length(cIS)) {
        foiPCA <- FeaturesOfInterest(description = "hoP",
                    fnames = featureNames(data)[sI],
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

## A helper function for plotting protein profile plots 
## and highlighting FeaturesOfInterest
.plotPlotDist <- function(data, levPlotDist,
                            levPlotDistOrg,
                            quantity, sI) {
    if (!is.null(data) &&
            !is.null(levPlotDist) &&
                !(is.null(levPlotDistOrg))) {
        
        if (as.numeric(quantity) %% 2 == 0)
            col <- as.numeric(quantity) / 2
        else
            col <- (as.numeric(quantity) + 1) / 2
        
        if (as.numeric(quantity) == 1)
            par(mfrow=c(1, 1))
        else
            par(mfrow=c(2, col))
        
        
        ## Actual plotting
        for (i in 1:min(quantity, length(levPlotDist))) { 
            
            if (levPlotDist[i] == "all")
                objPlotDist <- data
            else
                objPlotDist <- subset(data, 
                    fData(data)[, levPlotDist[i]] == levPlotDistOrg[i])
            
            if (is.null(sI))
                ylim <- range(exprs(objPlotDist))
            else {
                ylim1 <- range(exprs(objPlotDist))
                if (length(sI)) {
                    ylim2 <- range(exprs(data)[sI,])
                    ylim <- range(ylim1, ylim2)
                }
                else 
                    ylim <- ylim1
            }
            
            plotDist(objPlotDist, ylim = ylim)
            
            if (!is.null(sI) && length(sI) > 0)
                apply(X = exprs(data[sI, ]), MARGIN = 1, FUN = lines)
            title(levPlotDistOrg[i])            
        } ## end for loop
    }
}

