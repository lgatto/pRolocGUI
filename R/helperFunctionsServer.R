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

## START: DATA (pRolocVis) ##

## a helper function to create names from an object x
## which can be selected in the 'Data' tab
.namesObj <- function(x, name = NULL, upload = FALSE) {
    ## argument name is of type character and is the name of the object 
    ## obtained by MSnbase:::getVariableName(match.call(), "object")
    ## e.g. "andy2011" when starting pRolocVis(andy2011)
    if (is.null(names(x))) {
        ans <- c(paste("object", 1:length(x), sep = ""), "upload")
        if (!is.list(x))
            ans <- c(name, "upload")
    }
    if (!is.null(names(x))) {
        ans <- vector("character", length(x) + 1)
        
        if (FALSE %in% !nchar(names(x))) {
            ##nolist <- MSnbase:::getVariableName(match.call(), "x")
            unnamed <- paste("object", which(!nchar(names(x))), sep = "")
            ans[which(!nchar(names(x)))] <- unnamed
        }
        ans[which(nchar(names(x)) > 0)] <- names(x)[which(nchar(names(x)) > 0)]
        ans[length(ans)] <- "upload"
    }
    if (!upload) {
        ans <- ans[-length(ans)]
    }
    return(ans)
}

## END: DATA (pRolocVis) ##



## START: Display selection ##

## A helper function to select the checkbox of "PCA" or "protein profiles" 
## in the Display selection widget, used in observer for assigning to 
## dSelect$PCA or dSelect$plotDist
.selClick <- function(dBox, click1, prot, PCA, click2 = NULL) {
    if (!is.null(click1) || !is.null(click2)) {
        isolate({
            click1
            click2
            dBox <- ifelse(PCA, "cursorPCA", "cursorPlotDist")
        })
    } else
        dBox <- NULL
    if (is.null(prot))
        dBox <- NULL
    ans <- unique(dBox)
    return(ans)
}



## A helper function to select the checkbox of "query"/"summary matrix" in the 
## Display selection widget, used in observer for assigning to dSelect$text/
## dSelect$data
.selButton <- function(dtext, button, resetMult, protText, sel = "text") {
    if (!is.null(button)) {
        if (button > 0)
            dtext <- sel
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
                    protPCA, protPlotDist, protSearch, protData) {
    ans <- NULL
    if ("text" %in% cIS)
        ans <- c(ans, protText)
    if ("cursorPCA" %in% cIS)
        ans <- c(ans, protPCA)
    if ("cursorPlotDist" %in% cIS)
        ans <- c(ans, protPlotDist)
    if ("savedSearches" %in% cIS && !is.null(tagSelectList))
        ans <- c(ans, protSearch)
    if ("summat" %in% cIS)
        ans <- c(ans, protData)
    unique(ans)
}

## for unique features
.sIUni <- function(protDataU, cIS) {
    ans <- NULL
    if ("summat" %in% cIS) ans <- protDataU
    return(ans)
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
        return(ans)
    }
}

## function to concatenate new features to old ones
.obsProtText <- function(obj, protText, button, sRText, search, 
                    ind = c("object1", "object2"), names = FALSE, add = TRUE) {
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
        
        if (add == TRUE)
            return(unique(protText))
        else 
            return(newFeat)
    }
}

## remove Features used for .prot$summat
.removeFeat <- function(oldFeat, newFeat, button) {
   # observe({
        ind <- match(newFeat, oldFeat)
        if (button > 0) {
          #  isolate({
                if (!NA %in% ind)
                    oldFeat <- oldFeat[-ind]
         #  })
        }
        return(oldFeat)
  # })
}


## a check function to test if features are already internally stored
## returning TRUE or FALSE
.checkFeatData <- function(common, unique1, unique2, newfeat, sel, rem = FALSE) {

    len <- length(newfeat)
    
    if (is.null(sel))
        sel <- "common"
    
    if (sel == "common") 
        ans <- length(intersect(common, newfeat)) == len
    if (sel == "unique1")
        ans <- length(intersect(unique1, newfeat)) == len
    if (sel == "unique2")
        ans <- length(intersect(unique2, newfeat)) == len
    
   # if (rem) { ## for .removeFeat/removeData button
        if (is.null(newfeat))
            ans <- FALSE
   # }
    
    return(ans)
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
.checkBoxdSelect <- function(dPCA, dPlotDist, dSaSe, dText, dData = NULL, 
                                                                comp = FALSE) {
    choices <- c("PCA" = "cursorPCA",
                   "protein profiles" = "cursorPlotDist",
                   "saved searches" = "savedSearches",
                   "query" = "text")
    if (comp)
        choices <- c(choices[1:3], "summary matrix" = "summat", choices[4])
    
    checkboxGroupInput("chooseIdenSearch", 
        label = "",
        choices = choices,
        selected = c(dPCA, dPlotDist, dSaSe, dText, dData)
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
.vPCA <- function(obj, PCAn1, PCAn2,
                  ind = c("object1", "object2"), 
                  mX = FALSE, mY = FALSE, method = "PCA") { 
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        if (!is.null(PCAn1) && !is.null(PCAn2)) {
            ans <- plot2D(obj, fcol=NULL,
                          dims=c(as.numeric(PCAn1), as.numeric(PCAn2)), 
                          mirrorX = mX, mirrorY = mY, plot=FALSE,
                          method = method)
            if (mX) ans[, 1] <- -ans[, 1]
            if (mY) ans[, 2] <- -ans[, 2]
            return(ans)
        }
    }
}

## UI for colours
.colourPCA <- function(obj, sel = "none", ind = c("object1", "object2"),
                       inputname = "fcolours", label = "colour") {
    if (length(obj) != 0) {
        ind <- match.arg(ind)
        obj <- ifelse(ind == "object1", obj[1], obj[2])[[1]]
        ans <- selectInput(inputname, label, c("none", fvarLabels(obj)),
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
                    min = round(min(valuesPCA[, col]), 2) - 1,
                    step = 0.01,
                    max = round(max(valuesPCA[, col]), 2) + 1,
                    value = c(round(min(valuesPCA[, col]), 2), 
                        round(max(valuesPCA[, col]), 2))
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
                     sb, PCAn1, PCAn2, legend = c(FALSE, TRUE),
                     legendpos, sI, cIS, ind = c("object1", "object2"), 
                     mX = FALSE, mY = FALSE, listSaSe = list(),
                     method) {
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
            if (fcex %in% fvarLabels(obj))## && legend)
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
                       cex = fcex,
                       mirrorX = mX, mirrorY = mY,
                       method = method)
            else
                ## create plot2D and assign reactive variables to 
                ## arguments take input$fsymboltype for symboltype
                plot2D(obj, fcol = colour, fpch = sb,
                       xlim = c(xrange[1], xrange[2]),
                       ylim = c(yrange[1], yrange[2]),
                       dims = c(as.numeric(PCAn1),
                           as.numeric(PCAn2)),
                       cex = fcex,
                       mirrorX = mX, mirrorY = mY,
                       method = method)
        }
        
        if (!is.null(legend))
            if (fcolours %in% fvarLabels(obj) && legend)
                ## add a legend to the plot with reactive 
                ## variable as arguments
                addLegend(obj, fcol = colour, 
                          where = legendpos,
                          bty = "n", cex = 1)

        if (length(na.exclude(sI)) && length(cIS)) {
            foiPCA <- FeaturesOfInterest(description = "hoP",
                                         fnames = featureNames(obj)[sI]) ##,
            ## object = obj)
            highlightOnPlot(obj, foiPCA,
                            args = list(
                                method = method,
                                fcol = fvarLabels(obj)[1],
                                xlim = c(xrange[1], 
                                    xrange[2]),
                                ylim = c(yrange[1], 
                                    yrange[2]),
                                dims = c(as.numeric(PCAn1),
                                    as.numeric(PCAn2)),
                                mirrorX = mX, mirrorY = mY
                                ),
                            col = ifelse(fcolours == "none", "steelblue", "black"),
                            cex = 1.5,
                            lwd = 3)
        }
        
        if (length(listSaSe) > 0 && length(cIS)) {

            for (i in 1:length(listSaSe)) {
                if (!(length(listSaSe[[i]]) == 1 && is.na(listSaSe[[i]]))) {
                    .ind <- listSaSe[[i]]
                    .ind <- na.exclude(listSaSe[[i]])
                    if (length(.ind) == 0)
                        .ind <- NULL
                    .f <- i/13
                    .f <- floor(.f)
                    if ((i %% 13) == 0)
                        .f <- .f - 1
                
                    if (length(.ind) > 0) {
                        foiSaSe <- FeaturesOfInterest(description = "hoP",
                                                      fnames = featureNames(obj)[.ind])
                        highlightOnPlot(obj, foiSaSe, 
                                        args = list(
                                            method = method,
                                            fcol = fvarLabels(obj)[1],
                                            xlim = c(xrange[1], 
                                                xrange[2]),
                                            ylim = c(yrange[1],
                                                yrange[2]),
                                            dims = c(as.numeric(PCAn1), 
                                                as.numeric(PCAn2)),
                                            mirrorX = mX, mirrorY = mY
                                            ),
                                        cex = 1.5, lwd = 3, pch = 21, 
                                        col = getStockcol()[i - (.f*13)],
                                        bg = paste0(getStockcol()[i - (.f*13)], "50")) 
                    }
                }
            }
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
                    apply(X = exprs(obj[sI, ]), MARGIN = 1, FUN = lines, lwd = 3)
                
                title(levPlotDistOrg[i])
            } ## end for loop
        }
    }
}
## END: TAB protein profiles ##



## START: TAB Quantitation Data  and feature meta-data##

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
        
        if (!is.null(radiobutton)) {
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
        ans <- which(match(rownames(obj), .fnamesFOI(coll)[[index]]) != "NA")
        return(ans)
    }
}

## selectInput for collection of features to choose between in tab search
.tagListSearch <- function(coll) {
    if (!is.null(coll)) 
        if(length(coll) != 0)
            selectInput("tagSelectList", "Display information", 
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
            return("name exists already")
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

## START: TAB Data (pRolocComp) ##

## subset object
.dataSub <- function(object, 
                    subsetData = c("common & unique", "unique", "common")) {
    if (!is.null(subsetData)) {
        subsetData <- match.arg(subsetData)
        if (subsetData == "common & unique")
            ans <- object
        else {
            obj1 <- object[[1]]
            obj2 <- object[[2]]
            inter <- intersect(rownames(obj1), rownames(obj2))
            if (subsetData == "unique")
                ans <- list(
                    obj1[setdiff(rownames(obj1), inter)],
                    obj2[setdiff(rownames(obj2), inter)])
            else
                ans <- list(
                    obj1[rownames(obj1) %in% inter],
                    obj2[rownames(obj2) %in% inter])
        }
        return(ans)
    }
}

## returns marker levels of selected fvarLabels
.mC <- function(obj, fcol1, fcol2) {
    obj1 <- obj[[1]]
    obj2 <- obj[[2]]
    ans <- union(unique(fData(obj1)[, fcol1]), unique(fData(obj2)[, fcol2]))
    return(ans)
}

## helper function for compfnames to create matrix 
.calcCompNumbers <- function(flist) {
    .len <- length(flist)
    ans <- matrix(NA, nrow = .len, ncol = 3)
    colnames(ans) <- c("common", "unique1", "unique2")
    .name <- vector("character", .len)
    for (i in 1:.len) {
        .name[i] <- flist[[i]]@name
        ans[i, 1] <- length(flist[[i]]@common)
        ans[i, 2] <- length(flist[[i]]@unique1)
        ans[i, 3] <- length(flist[[i]]@unique2) 
    }
    rownames(ans) <- .name
    return(ans)
}

## create HTML cell
.cellHTML <- function(cell) paste0('<td>', cell, '</td>')
## create bold HTML cell
.boldHTML <- function(cell) 
    paste0("<td>", "<strong>", cell, "</strong></td>")


## create HTML rows
.rowHTML <- function(row, rb = "common", marker = "all", rowname) {
    
    if (marker == rowname) {
        cells <- sapply(row, .cellHTML)
        cells[rb] <- .boldHTML(row[rb])
    } else
        cells <- sapply(row, .cellHTML)
    
    ans <- paste(cells, collapse = "")
    ans <- paste0(ans, '</tr>')
    return(ans)
}

## create HTML table
.tableHTML <- function(mat, rb, marker) {
    
    if (!is.null(marker)) {
        vec <- vector("character", dim(mat)[1])
    
        for (i in 1:dim(mat)[1]) {
            if (marker == rownames(mat)[i]) {
                rn <- paste0("<tr>", "<th align='left'>", marker, "</th>")
            } else
                rn <- paste0("<tr>", "<td align='left'>", rownames(mat)[i], "</td>")
        
            vec[i] <- paste0(rn, 
                .rowHTML(mat[i,], rb, marker, rownames(mat)[i]), collapse = "")
        }
    
        common <- ifelse(rb == "common", "<th>common</th>", "<td>common</td>")
        unique1 <- ifelse(rb == "unique1", "<th>unique1</th>", "<td>unique1</td>")
        unique2 <- ifelse(rb == "unique2", "<th>unique2</th>", "<td>unique2</td>")
    
        head <- paste("<tr>","<th></th>", common, unique1, unique2, "</tr>", sep="")
        ans <- paste0(vec, collapse="")
        ans <- paste0(head, ans, collapse="")
        ans <- paste0("<table border='0' cellspacing='10' cellpadding='10'>", 
                        ans, "</table>")
        return(ans)
    }
}



## helper function to create data frame with featureNames 
## with FeatComp infrastructure

## not used
## .namesCompFeat <- function(obj1, obj2, mL1, mL2, sel, compRadio) {
##     
##     .indData <- 1
##     if (mL1 == "none" || mL2 == "none") {
##         mL1 <- mL2 <- NULL
##     } else 
##         .indData <- which(sel == c("all", .mC(list(obj1, obj2), mL1, mL2)))
##         
##     .comp <- compfnames(obj1, obj2, mL1, mL2, verbose = FALSE)
##     
##     if (compRadio %in% c("unique1", "common")) {
##         obj <- obj1
##     } else
##         obj <- obj2
##     
##     ## get feature names in .comp
##     if (is.null(mL1) && is.null(mL2))
##         .Feat <- slot(.comp, compRadio)
##     else
##         .Feat <- slot(.comp[[.indData]], compRadio)
##     
##     .lenFeat <- length(.Feat)
##     
##     if (.lenFeat != 0) {
##         .names <- sort(.Feat)        
##         .ceil <- ceiling(.lenFeat/4)
##         ans <- matrix("-", .ceil, 4)
##         
##         if (.lenFeat <= 4)
##             ans[1, 1:.lenFeat] <- .names 
##         else {
##         
##             if ((.lenFeat / 4) %% 1 == 0.25) {
##                 ans[,1] <- .names[1:.ceil]
##                 ind2 <- .ceil + floor(.lenFeat / 4)
##                 ans[1:(.ceil-1),2] <- .names[(.ceil + 1):ind2]
##                 ind3 <- ind2 + floor(.lenFeat / 4)
##                 ans[1:(.ceil - 1),3] <- .names[(ind2 + 1):ind3]
##                 ind4 <- ind3 + floor(.lenFeat / 4)
##                 ans[1:(.ceil - 1),4] <- .names[(ind3 + 1):ind4]}
##         
##             if ((.lenFeat / 4) %% 1 == 0.5) {
##                 ans[,1] <- .names[1:.ceil]
##                 ans[,2] <- .names[(.ceil + 1):(2 * .ceil)]
##                 ind3 <- .ceil*2 + floor(.lenFeat / 4)
##                 ans[1:(.ceil - 1),3] <- .names[(2 * .ceil + 1):ind3]
##                 ind4 <- ind3 + floor(.lenFeat / 4)
##                 ans[1:(.ceil - 1),4] <- .names[(ind3 + 1):ind4]}
##         
##             if ((.lenFeat / 4) %% 1 == 0.75) {
##                 ans[,1] <- .names[1:.ceil]
##                 ans[,2] <- .names[(.ceil + 1):(2 * .ceil)]
##                 ans[,3] <- .names[(2 * .ceil + 1):(3 * .ceil)]
##                 ind4 <- .ceil*3 + floor(.lenFeat / 4)
##                ans[1:(.ceil-1),4] <- .names[(3 * .ceil + 1):ind4]}
##         
##             if ((.lenFeat / 4) %% 1 == 0) {
##                 ans[,1] <- .names[1:.ceil]
##                 ans[,2] <- .names[(.ceil + 1):(2 * .ceil)]
##                 ans[,3] <- .names[(2 * .ceil + 1):(3 * .ceil)]
##                 ans[,4] <- .names[(3 * .ceil + 1):(4 * .ceil)]}
##         }
##         
##         ans <- as.data.frame(ans)
##         
##         colnames(ans) <- c(" ", " ", " ", " ")
##     } else {
##         ans <- as.data.frame("no features comprised")
##         colnames(ans) <- " "
##     }
##     return(ans)    
## }
##    
## END: TAB Data (pRolocComp) ## 

