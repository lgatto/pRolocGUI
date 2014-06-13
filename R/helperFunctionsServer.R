#############################
## helper functions server ##
#############################

## is there a pRolocGUI_SearchResults in .GlobalEnv? is so load it.
## if FeaturesOfInterest -> create FoICollection
## if FoICollection -> FoICollection else NULL
.createSR <- function () {
    if (exists("pRolocGUI_SearchResults", .GlobalEnv)) {
        sr <- get("pRolocGUI_SearchResults", .GlobalEnv)
        if (inherits(sr, "FoICollection"))
            ans <- sr
        else {
            if (inherits(sr, "FeaturesOfInterest")) { 
                coll <- FoICollection()
                ans <- addFeaturesOfInterest(sr, coll)
            } else
                ans <- NULL
        }
    } else {
        ans <- NULL
    }
    return(ans)
}

## START: Display selection ##

## A helper function to select the checkbox of "PCA" or "protein profiles" 
## in the Display selection widget, used in observer for assigning to 
## dSelect$PCA or dSelect$plotDist
.selClick <- function(dBox, click1, prot, PCA, click2 = NULL) {
    if (!is.null(click1) || !is.null(click2)) {
        isolate({
            click1
            click2
            dBox <- ifelse(PCA, "mousePCA", "mousePlotDist")
        })
    } else
        dBox <- NULL
    if (is.null(prot))
        dBox <- NULL
    ans <- unique(dBox)
    return(ans)
}



## A helper function to select the checkbox of "query" in the 
## Display selection widget, used in observer for assigning to dSelect$text
.selText <- function(dtext, saveText, resetMult, protText) {
    if (!is.null(saveText)) {
        if (saveText > 0)
            dtext <- "text"
        else
            dtext <- dtext
        
        if (!is.null(resetMult)) {
            isolate({
                resetMult
                if (resetMult > 0 && length(protText) < 1)
                    dtext <- NULL
            })
        }
        ans <- unique(dtext)
        return(ans)
    }
}

## A function to compute indices from feature names
.computeInd <- function(obj, fnames, ind = c("object1", "object2")) {
    ind <- match.arg(ind)
    obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
    ans <- match(fnames, rownames(obj))
    if (NA %in% ans)
        ans <- ans[-which(is.na(ans))]
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
.sRsubset <- function(obj, search, levelSearch, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        subset(
            (
                if(search != "protein")
                    names(table(fData(obj)[search]))
                else
                    rownames(obj)
            ), 
            grepl(levelSearch,
                if(search != "protein")
                    names(table(fData(obj)[search]))
                else
                    rownames(obj)
            )
        )
    }
}

## a check function to test if features are already internally stored
## returning TRUE or FALSE
.checkFeatText <- function(obj, protText, sRText, 
                           search, ind = c("object1", "object2"), name = FALSE) {
    if (length(obj) != 0) {
        
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (!name)
            protText <- rownames(obj)[protText]
        if (search == "protein") 
            ans <- length(which(sRText %in% protText)) == length(sRText)
        else {
            feat <- fData(obj)[search]
            indices <- which(feat == sRText)
            Feat <- which(rownames(obj)[indices] %in% protText)
            ans <- length(Feat) == length(indices)
        }
    }
}


.obsProtText <- function(obj, protText, button, sRText, search, 
                         ind = c("object1", "object2"), names = FALSE) {
    if (length(obj) != 0 && !is.null(button)) {        
                    
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        
        if (!is.null(search)) {
            if (!names) {
                if (search == "protein") 
                    newFeat <- which(rownames(obj) == sRText)
                else 
                    newFeat <- which(fData(obj)[search] == sRText)
            } else {
                if (search == "protein") 
                    newFeat <- sRText
                else 
                    newFeat <- rownames(obj)[which(fData(obj)[search] == sRText)]
            }
            if (!is.null(newFeat)) {
                if (button == 1 && length(newFeat > 0)) 
                    isolate({
                        protText <- isolate(c(protText, isolate(newFeat)))
                    })
            }
        }
        return(unique(protText))
    }
}

## concatenate new Indices to old ones when clicking, for PCA and plotDist
.obsProtClick <- function(protMult, minDist, click) {
    ## will be empty initially
    if(!is.null(click)) {
        isolate({
            protMult <- c(protMult, minDist)
            ## remove indices when indices are double-clicked
            if (length(which((as.vector(table(protMult)) > 1))))
                protMult <- protMult[-which(
                    protMult == names(which(table(protMult) > 1))
                )]
        })
    }
    return(protMult)
}

## checkBox helper function for Display selection
.checkBoxdSelect <- function(dPCA, dPlotDist, dText) {
    checkboxGroupInput("chooseIdenSearch", 
        label = "",
        choices = c("PCA" = "mousePCA",
                "protein profiles" = "mousePlotDist",
                "saved searches" = "savedSearches",
                "query" = "text"),
        selected = c(dPCA, dPlotDist, dText)
    )
}

## selectInput helper for Display selection
.selVarText <- function(obj, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        selectInput("search", "", 
                    choices = c("protein", fvarLabels(obj)))
    }
}

## selectInput helper for Display selection, to select results
.selResText <- function(search, results) {
    if (!is.null(search))
        if (length(results))
            selectInput("sRTextInput", label="",
                        choices = results)
    else
        return("not found")
}

## reset Button
.reset <- function(ind1, ind2 = NULL) {
    if (!is.null(ind1) || !is.null(ind2))
        if (length(ind1) > 0 || length(ind2) > 0)
            actionButton("resetMult", "Clear features")
}
## END: Display selection ##



## START: TAB PCA ##
## values PCA
.vPCA <- function(obj, PCAn1, PCAn2, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (!is.null(PCAn1) && !is.null(PCAn2)) {
            ans <- plot2D(obj, fcol=NULL,
                    dims=c(as.numeric(PCAn1), as.numeric(PCAn2)), 
                    plot=FALSE)
            return(ans)
        }
    }
}

## UI for colours
.colourPCA <- function(obj, sel = "none", ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ans <- selectInput("fcolours", "colour", c("none", fvarLabels(obj)),
                    selected = sel)
        return(ans)
    }
}

## point size
## obj will be MSnSet
.fcex <- function(obj) {
    if (length(obj) != 0) {
        ## check for numericcolums in fData(data) 
        colNum <- which(sapply(fData(obj), is.numeric))
        ## write indices in vector colNum
        colNum <- as.vector(colNum)
        if (length(colNum))
            ## return fvarLabels of numeric colums 
            fvarLabels(obj)[colNum]
    }
}

## UI for point size
.fcexPCA <- function(obj, colours, sel = "1", ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ## only show when there are numeric columns in fData (.fcex())
        if (length(colours) > 0 && length(.fcex(obj)) > 0) 
            if (colours != "none") {
                ans <- selectInput("fcex", "point size", c("1", .fcex(obj)),
                        selected = sel)
                return(ans)
            }
    }
}

## UI for symbol type
.symbolPCA <- function(obj, colours, sel = "none", ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (!is.null(colours) && colours %in% fvarLabels(obj)) {
            ans <- selectInput("fsymboltype", "symbol type", 
                    c("none", fvarLabels(obj)), selected = sel)
            return(ans)
        }
    }
}

## UI for xrange or yrange (zoom)
.rangePCA <- function(valuesPCA, col, id = "xrange") {
    if(!is.null(valuesPCA))
        ## get max and min values of first principal component
        ## create a range slider
        sliderInput(id,
                    ifelse(col == 1, "zoom x-axis", "zoom y-axis"),
                    min = min(valuesPCA[, col]) - 1,
                    max = max(valuesPCA[, col]) + 1,
                    value = c(min(valuesPCA[, col]), 
                              max(valuesPCA[, col]))
        )  
}

## UI for principal components
.PC <- function(obj, axis, sel, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ans <- selectInput(
                    ifelse(axis == "x", "PCAn1", "PCAn2"),
                    ifelse(axis == "x", "PC along x-axis", "PC along y-axis"),
                    selected = sel,
                    choices = isolate(c(1:nrow(pData(obj)))))
        return(ans)
    }
}

## checkBox UI for legend
.legendPCA <- function(obj, colours, sel = c(FALSE, TRUE), ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (length(colours) && colours %in% fvarLabels(obj)) {
            ans <- checkboxInput("legendyes", "legend", value = sel)
            return(ans)
        }
    }
}

## position for legend, sliderInput
.legendPosPCA <- function(obj, colours, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (length(colours))
            if (colours %in% fvarLabels(obj)) {
                ## drop down menu for position of legend
                ans <- selectInput("legendpos", "position of legend",
                        choices = c("bottomright", "bottom",
                            "bottomleft","left", "topleft", "top",
                            "topright", "right","center"), 
                        selected="bottomright")
                return(ans)
            }
    }
}

## a helper function for plotting the PCA plot and highlighting
## FeaturesOfInterest using highlightOnPlot
.plotPCA <- function(obj, fcolours, fcex, xrange, yrange,
                     sb, PCAn1, PCAn2, legend = c(FALSE, TRUE), legendpos,
                     sI, cIS, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        
        if (length(fcolours)) {
            if (fcolours %in% fvarLabels(obj))
                colour <- fcolours
            else
                colour <- NULL
        }
    
        if (length(fcex) && !is.null(legend)) {
            if (fcex %in% fvarLabels(obj) && legend)
                fcex <- fData(obj)[, fcex]
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
                plot2D(obj, fcol = colour,
                        xlim = c(xrange[1], xrange[2]),
                        ylim = c(yrange[1], yrange[2]),
                        dims = c(as.numeric(PCAn1),
                                as.numeric(PCAn2)),
                        cex = fcex)
            else
                ## create plot2D and assign reactive variables to 
                ## arguments take input$fsymboltype for symboltype
                plot2D(obj, fcol = colour, fpch = sb,
                        xlim = c(xrange[1], xrange[2]),
                        ylim = c(yrange[1], yrange[2]),
                        dims = c(as.numeric(PCAn1),
                                as.numeric(PCAn2)),
                        cex = fcex)
        }
    
        if (fcolours %in% fvarLabels(obj) && legend)
            ## add a legend to the plot with reactive 
            ## variable as arguments
            addLegend(obj, fcol = colour, 
                where = legendpos,
                bty = "n", cex = 1)

        if (length(sI) && length(cIS)) {
            foiPCA <- FeaturesOfInterest(description = "hoP",
                                         fnames = featureNames(obj)[sI],
                                         object = obj)
            highlightOnPlot(obj, foiPCA, 
                    args = list(
                        fcol = fvarLabels(obj)[1],
                        xlim = c(xrange[1], 
                                 xrange[2]),
                        ylim = c(yrange[1], 
                                 yrange[2]),
                        dims = c(as.numeric(PCAn1),
                                 as.numeric(PCAn2))),
                        col = ifelse(fcolours == "none", "steelblue", "black"),
                        cex = 1.5,
                        lwd = ifelse(fcolours == "none", 2, 1.5))
        }
    }
}
## END: TAB PCA ## 

## START: TAB protein profiles ##

.quantPlotDist <- function(choices, sel) {
    selectInput("quantityPlotDist", "number of plots to display",
        choices = choices, selected = sel)
}

.nC <- function(numberPlotDist, quantityPlotDist) {
    if (quantityPlotDist == "1")
        1
    else
        as.numeric(numberPlotDist)
}

## organelle names, levels in fData
.orgName <- function(obj, fnames, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (!is.null(fnames)) {
            if (fnames != "all")
                ans <- names(table(fData(obj)[fnames]))
            else
                ans <- "all"
            return(ans)
        }
    }
}

## selectInput for fvarLabels/"all" to select for plotDist
.featuresPlotDist <- function(obj, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ans <- selectInput("fNamesplDist",
                    "feature(s) in",
                    choices = c("all", fvarLabels(obj))) 
        return(ans)
    }
}
## selectInput for levels in fvarLabels or "all"
.flevelPlotDist <- function(flevels, fnames) {
    if (!is.null(flevels) && !is.null(fnames))
        selectInput("organelleAll",
                    "assigned to",
                    choices = flevels)
}

## sliderInput for number of plots to plot
.numPlotDist <- function(quantity) {
    if (!as.numeric(quantity) == 1) 
        sliderInput("numberPlotDist", "Selected plot",
                    min = 1, max = as.numeric(quantity), 
                    value = 1, step = 1
        )
}

## A helper function for plotting protein profile plots 
## and highlighting FeaturesOfInterest
.plotPlotDist <- function(obj, levPlotDist, levPlotDistOrg,
                            quantity, sI, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
    
        if (!is.null(levPlotDist) && !is.null(quantity) && 
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
                    objPlotDist <- obj
                else
                    objPlotDist <- subset(obj, 
                            fData(obj)[, levPlotDist[i]] == levPlotDistOrg[i])
            
                if (is.null(sI))
                    ylim <- range(exprs(objPlotDist))
                else {
                    ylim1 <- range(exprs(objPlotDist))
                    if (length(sI)) {
                        ylim2 <- range(exprs(obj)[sI,])
                        ylim <- range(ylim1, ylim2)
                    }
                    else 
                        ylim <- ylim1
                }
                
                plotDist(objPlotDist, ylim = ylim)
                
                if (!is.null(sI) && length(sI) > 0)
                    apply(X = exprs(obj[sI, ]), MARGIN = 1, FUN = lines)
                
                title(levPlotDistOrg[i])
            } ## end for loop
        }
    }
}
## END: TAB protein profiles ##



## START: TAB Quantitation Data ##

## END: TAB Quantitation Data and feature meta-data ##

.radioButton <- function(indices, quant) {
    radioButtons(
        ifelse(quant == TRUE, "exprsRadio", "fDataRadio"),
        "Features",
        choices = list("all or"="all", "selected"="selected"),
        selected = ifelse(length(indices), "selected", "all")
    )
}

.dTable <- function(obj, mdata, radiobutton = "all", 
                    indices = NULL, ind = c("object1", "object2")) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
    
        if (radiobutton == "all") {
            ## cbind to display data properly
            if(mdata == "quant")
                ans <- as.data.frame(
                    cbind(" " = rownames(exprs(obj)), exprs(obj)))
            if (mdata == "fD")
                ans <- as.data.frame(
                    cbind(" " = rownames(fData(obj)), fData(obj)))
            if (mdata == "pD")
                ans <- as.data.frame(
                    cbind(" " = rownames(pData(obj)), pData(obj)))
        } else {
            ## cbind to display data properly
            if(mdata == "quant") 
                ans <- as.data.frame(cbind(" " = rownames(exprs(obj[indices])), 
                                       exprs(obj[indices])))
            else
                ans <- as.data.frame(cbind(" " = rownames(fData(obj[indices])),
                         fData(obj[indices])))
        }
        return(ans)
    }
}
## END: TAB Quantitation and feature meta-data ##


## START: TAB Search ##
.obsSavedSearch <- function(coll, newfeat, indices, button, descr) {
    if (!is.null(button)
        && button > 0
            && length(indices) > 0) {
        isolate(
            if (!(descr %in% description(coll))) {
                coll <- addFeaturesOfInterest(newfeat, coll)
            }
        )
    }
    return(coll)
}

## which features of interests are selected in the drop-down menu
.whichTag <- function(tag, coll)
    which(tag == description(coll))[1]

## returns indices of collection of features which are selected in tab search
.whichFOI <- function(obj, coll, index, 
                        ind = c("object1", "object2"))  {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ans <- which((match(rownames(obj), .fnamesFOI(coll)[[index]])) != NA)
        return(ans)
    }
}

## selectInput for collection of features to choose between in tab search
.tagListSearch <- function(coll) {
    if(length(coll) != 0)
        selectInput("tagSelectList", "Select search result", 
            choices = description(coll)
        )
}

## text Input to enter description for new features of interest
.textDescription <- function() 
    textInput("savedSearchText", "Description", value="new search result")

## actionButton to save features to FoICollection
.buttonSearch <- function(coll, indices, descr) {
    ## actionButton will only appear when 
    ## there is a description and features are selected
    if (nchar(descr) != 0 && length(indices) != 0) {
        if (!(descr %in% description(coll)) || !length(coll))
            actionButton("saveLists2SR",
                         "Create new features of interest")
        else
            return("name already exists, choose another name")
    }   
}

## helper function to create new features 
.obsNewFoI <- function(obj, feat, descr, button, 
                       ind = c("object1", "object2"), indtrace = TRUE) {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        button
        sI <- isolate(feat)
        searchText <- isolate(descr)
        dataInput <- isolate(obj)
        if (!is.null(searchText)
            && !is.null(dataInput)
                && !is.null(sI)) {
            if (indtrace)
                ans <- FeaturesOfInterest(description = searchText,
                            fnames = featureNames(dataInput)[sI],
                            object = dataInput)
            else
                ans <- FeaturesOfInterest(description = searchText, fnames = sI)
            return(ans)
        }
    }
}

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
.showFOI <- function(x, obj, index = 1, comp = c(FALSE, TRUE)) {
    if (inherits(x, "FoICollection")) {
        if (comp == FALSE) {
            n <- fnamesIn(foi(x)[[index]], obj[[1]], TRUE)
            showFOI <- c(capture.output(show(foi(x)[[index]])),
                        paste("Therefrom in selected MSnSet:", n))
        } else {
            n1 <- fnamesIn(foi(x)[[index]], obj[[1]], TRUE)
            n2 <- fnamesIn(foi(x)[[index]], obj[[2]], TRUE)
            showFOI <- c(capture.output(show(foi(x)[[index]])),
                         paste("Therefrom in object1:", n1),
                         paste("Therefrom in object2:", n2))}   
    } 
    else { 
        if (comp == FALSE) {
            n <- fnamesIn(x, obj[[1]], TRUE)
            showFOI <- c(capture.output(show(x)),
                        paste("Therefrom in selected MSnSet:", n))
        } else {
            n1 <- fnamesIn(x, obj[[1]], TRUE)
            n2 <- fnamesIn(x, obj[[2]], TRUE)
            showFOI <- c(capture.output(show(x)),
                        paste("Therefrom in object1:", n1),
                        paste("Therefrom in object2:", n2))
        }
    }
    return(showFOI)
}

## END: TAB Search ##

