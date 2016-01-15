## DOUBLE CLICKING TO HIGHLIGHT on/off on PCA plot, or selection via table

## Shiny: spinning loading wheel on top of plot while plot is recalculating
## https://gist.github.com/daattali/edd7c20cd09f484b7f32

## Very slow with bigger data (fusion data), server side table flaky
## and warnings about size

## References
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://gallery.shinyapps.io/095-plot-interaction-advanced/
## https://gallery.shinyapps.io/105-plot-interaction-zoom/
## https://gallery.shinyapps.io/106-plot-interaction-exclude/
## https://github.com/rstudio/shiny-examples

## http://shiny.rstudio.com/articles/selecting-rows-of-data.html
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html

##' @rdname pRolocVis-apps
##' @param foi A \code{\link{FeaturesOfInterest}} object.
##' @param fig.height Height of the figure. Default is \code{"600px"}.
##' @param fig.width Width of the figure. Default is \code{"100px"}.
##' @param legend.width Width of the legend. Default is \code{"200\%"}.
##' @param nchar Maximum number of characters of the markers class
##' names, before their names are truncated. Default is 10.
##' @param all If \code{TRUE} all clusters are displayed on startup.
##' If \code{FALSE} only first cluster in the list is displayed.
##' \code{\link{FoICollection}}, that will be available for display.
##' @param fDataInd A \code{numeric} or \code{character} corresponding to
##' the feature variables one wishes to retains for the data table. 
##' If not specified, by default \code{markers} will be kept along with 
##' the first 5 and last 6 feature variables.
##' @return For \code{pca} a \code{character} of protein names, of the 
##' proteins selected upon application closure.
pRolocVis_pca <- function(object, 
                          fcol,
                          foi,
                          fig.height = "600px",
                          fig.width = "100%",
                          legend.width = "200%",
                          legend.cex = 1,
                          nchar = 40,
                          all = TRUE,
                          fDataInd) {
  
  ## Return featureNames of proteins selected
  on.exit(return(invisible(names(idxDT))))
  
  ## Specify fDataInd first. 
  ## There can't be more than 12 columns in the DT table
  
  if (missing(fDataInd)) {
    if (length(fvarLabels(object)) > 12) {
      message("There can't be more than 12 feature variables. Using 6 first and last.")
      xx <- pRolocGUI:::narrowFeatureData(object, )
      fDataInd <- fvarLabels(xx)
    } else {
      fDataInd <- fvarLabels(object)
    }
  } else {
    if (is.numeric(fDataInd))
      fDataInd <- fvarLabels(object)[fDataInd]
    
    if (length(fDataInd) > 12) {
      stop("There can't be more than 12 feature variables, check fDataInd")
    } else {
      if (!all(fDataInd %in% fvarLabels(object)))
        stop("Check fDataInd, not all features found in fData")
    }  
  }

  ## Usual checks
  if (!inherits(object, "MSnSet"))
    stop("The input must be of class MSnSet")
  if (missing(fcol)) fcol = "markers"
  if (!missing(fcol)) {
    if (!fcol %in% fvarLabels(object))
      stop("fcol missing in fData")
    if (!isMrkMat(object, fcol)) {
      mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
      object <- mrkVecToMat(object, fcol, mfcol = mName)
      fcol <- mName
    }  
    pmarkers <- fData(object)[, fcol]
  }
  
  ## Create column of unknowns (needed later for plot2D in server)
  newName <- paste0(format(Sys.time(), "%a%b%d%H%M%S%Y"), "unknowns")
  fData(object)[, newName] <- "unknown"
  
  ## Setting features to be displayed
  if (!missing(foi)) {
    if (inherits(foi, "FeaturesOfInterest"))
      foi <- FoICollection(list(foi))
    foimarkers <- as(foi, "matrix")
    if (exists("pmarkers", inherits = FALSE)) {
      pmarkers <- merge(pmarkers, foimarkers,
                        by = 0, all.x = TRUE)
      rownames(pmarkers) <- pmarkers[, "Row.names"]
      pmarkers <- pmarkers[featureNames(object), -1]            
    } else pmarkers <- foimarkers
  }
  if (length(grep("GO:", colnames(pmarkers))) > 0) {
    cn <- pRoloc::flipGoTermId(colnames(pmarkers))
    if (all(!is.na(cn))) {
      names(cn) <- NULL
      colnames(pmarkers) <- cn
    }
  }
  
  ## Marker colours
  cols <- getStockcol()
  if (length(cols) < ncol(pmarkers)) {
    message("Too many features for available colours. Some colours will be duplicated.")
    n <- ncol(pmarkers) %/% length(cols)
    cols <- rep(cols, n + 1)
  }

  ## Shorten markers names if too long
  cn <- sapply(colnames(pmarkers),
               function(x) {
                 if (nchar(x) > nchar) {
                   x <- strsplit(x, "")[[1]]
                   x <- paste(x[1:nchar], collapse = "")
                   x <- sub(" +$", "", x)
                   x <- paste0(x, "...")
                 }
                 return(x)
               })    
  names(cn) <- NULL
  colnames(pmarkers) <- cn
  
  ## If there are too many marker sets, better
  ## to display few and let the user choose
  pmsel <- TRUE
  if (!all & ncol(pmarkers) > 10)
    pmsel <- 1
  
  pcas <- plot2D(object, fcol = NULL, plot = FALSE)
  profs <- exprs(object)
  
  
  ## all feautres are displayed on start
  feats <- toSel <- 1:nrow(object)
  idxDT <- numeric()
  namesIdxDT <- character()

  
  ## Build shiny app
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectizeInput("markers", "Labels",
                       choices = colnames(pmarkers),
                       multiple = TRUE,
                       selected = colnames(pmarkers)[pmsel]),
        sliderInput("trans", "Transparancy",
                    min = 0,  max = 1, value = 0.5),
        checkboxInput("checkbox", label = "Show labels", value = TRUE),
        br(),
        actionButton("resetButton", "Zoom/reset plot"),
        br(),
        actionButton("clear", "Clear selection"),
        width = 2),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("PCA", id = "pcaPanel",
                             fluidRow(
                               column(9, 
                                      plotOutput("pca",
                                                 height = fig.height,
                                                 width = fig.width,
                                                 dblclick = "dblClick",
                                                 brush = brushOpts(
                                                   id = "pcaBrush",
                                                   resetOnNew = TRUE)),
                                      offset = 0),
                               column(3, 
                                      plotOutput("legend1",
                                                 height = fig.height,
                                                 width = legend.width))
                             )
                    ),
                    tabPanel("Profiles", id = "profilesPanel",
                             fluidRow(
                               column(8,
                                      plotOutput("profile",
                                                 height = "400px",
                                                 width = "120%"),
                                      offset = 0),
                               
                               column(3, 
                                      plotOutput("legend2",
                                                 width = "80%"),
                                      offset = 1)
                               )
                    ),
                    ## feature data table is always visible
                    fluidRow(
                      column(12,
                             column(length(fDataInd), 
                                    DT::dataTableOutput("fDataTable"))))
        )
      )
    ))
  
  
  
  server <-
    function(input, output, session) {
      ranges <- reactiveValues(x = NULL, y = NULL)
      brushBounds <- reactiveValues(i =  try(pcas[, 1] >= min(pcas[, 1]) & 
                                               pcas[, 1] <= max(pcas[, 1])),
                                    j = try(pcas[, 2] >= min(pcas[, 2]) & 
                                              pcas[, 2] <= max(pcas[, 2])))
      resetLabels <- reactiveValues(logical = FALSE)
      
      ## Get coords for proteins according to selectized marker class(es)
      mrkSel <- reactive({
        lapply(input$markers,
               function(z) which(pmarkers[, z] == 1))
      })
      
      ## Update colour transparacy according to slider input
      myCols <- reactive({
        scales::alpha(cols,
                      input$trans)[sapply(input$markers, function(z) 
                        which(colnames(pmarkers) == z))]
      })
      
      
      ## PCA plot
      output$pca <- renderPlot({
        par(mar = c(4, 4, 0, 0))
        par(oma = c(1, 0, 0, 0))
        # plot(pcas, 
        
        plot2D(object,
               col = rep(getUnknowncol(), nrow(object)),
               pch = 21, cex = 1,
               xlim = ranges$x,
               ylim = ranges$y,
               fcol = newName)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) 
            points(pcas[mrkSel()[[i]], ], pch = 16, 
                   cex = 1.4, col = myCols()[i])
        } 
          
        
        ## highlight point on plot by selecting item in table
        idxDT <<- feats[input$fDataTable_rows_selected]
        
        if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
        
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          highlightOnPlot(object, namesIdxDT, cex = 1.3)
          if (input$checkbox) 
            highlightOnPlot(object, namesIdxDT, labels = TRUE, pos = 3)
        }
        resetLabels$logical <- FALSE
      })
      
      
      ## Protein profile
      output$profile <- renderPlot({
        par(mar = c(8, 3, 1, 1))
        par(oma = c(0, 0, 0, 0))
        ylim <- range(profs)
        n <- nrow(profs)
        m <- ncol(profs)
        fracs <- sampleNames(object)
        plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
             type = "n", xaxt = "n", xlab = "")
        axis(1, at = 1:m, labels = fracs, las = 2)
        title(xlab = "Fractions", line = 5.5)
        matlines(t(profs[feats, ]),
                 col = getUnknowncol(),
                 lty = 1,
                 type = "l")
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) { 
            matlines(t(profs[mrkSel()[[i]], ]),
                     col = myCols()[i],
                     lty = 1,
                     lwd = 1.5) 
          }
        }
        
        ## If an item is clicked in the table highlight profile
        idxDT <<- feats[input$fDataTable_rows_selected]
        
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          matlines(t(profs[namesIdxDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 2)
        }
      })             
      
      
      ## Feature data table
      output$fDataTable <- DT::renderDataTable({
        
        feats <<- which(brushBounds$i & brushBounds$j)
        
        ## Double clicking to identify protein
        
        if (!is.null(input$dblClick)) {
          dist <- apply(pcas, 1, function(z) sqrt((input$dblClick$x - z[1])^2 
                                                  + (input$dblClick$y - z[2])^2))
          idxPlot <- which(dist == min(dist))
          if (idxPlot %in% idxDT) {                          ## 1--is it already clicked?
            setsel <- setdiff(names(idxDT), names(idxPlot))  ## Yes, remove it from table
            idxDT <<- idxDT[setsel]
          } else {                                           ## 2--new click?
            idxDT <<- c(idxDT, idxPlot)                      ## Yes, highlight it to table
          }
        }

        namesIdxDT <<- names(idxDT)                         ## update idx of names to track clicking
        toSel <- match(namesIdxDT,                          ## selection to highlight in DT
                       featureNames(object)[brushBounds$i & brushBounds$j])
        if (resetLabels$logical) toSel <- numeric()         ## reset labels

        ## Display data table (with clicked proteins highlighted)
        DT::datatable(data = fData(object)[brushBounds$i & brushBounds$j, fDataInd], 
                      rownames = TRUE,
                      selection = list(mode = 'multiple', selected = toSel))
      })
      
      
      ## When a the reset button is clicked check to see is there is a brush on
      ## the plot, if yes zoom, if not reset the plot.
      observeEvent(input$resetButton, {
        brush <- input$pcaBrush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          brushBounds$i <- pcas[, 1] >= brush$xmin & pcas[, 1] <= brush$xmax
          brushBounds$j <- pcas[, 2] >= brush$ymin & pcas[, 2] <= brush$ymax
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
          brushBounds$i <- try(pcas[, 1] >= min(pcas[, 1]) 
                               & pcas[, 1] <= max(pcas[, 1]))
          brushBounds$j <- try(pcas[, 2] >= min(pcas[, 2]) 
                               & pcas[, 2] <= max(pcas[, 2]))
        }
      })
      
      ## When clear selection is pressed update clear idxDT above and reset selection 
      observeEvent(input$clear, {
         resetLabels$logical <- TRUE
      })
  

      ## Output legend
      output$legend1 <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        par(oma = c(0, 0, 0, 0))
        plot(0, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             bty = "n")
        if (!is.null(input$markers)) {
          legend("topleft",
                 c(input$markers, "unlabelled"),
                 col = c(myCols(), getUnknowncol()),
                 ncol = 1, bty = "n",
                 pch = c(rep(16, length(myCols())), 21),
                 cex = legend.cex)
        } else {
          legend("topleft",
                 "unlabelled",
                 col = getUnknowncol(),
                 ncol = 1, bty = "n",
                 pch = 21,
                 cex = legend.cex)
        }
      })
      ## Output legend
      output$legend2 <- renderPlot({
        par(mar = c(0, 0, 0, 0))
        par(oma = c(0, 0, 0, 0))
        plot(0, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             bty = "n")
        if (!is.null(input$markers)) {
          legend("topleft",
                 c(input$markers, "unlabelled"),
                 col = c(myCols(), getUnknowncol()),
                 ncol = 1, bty = "n",
                 pch = c(rep(16, length(myCols())), 21),
                 cex = legend.cex
          )
        } else {
          legend("topleft",
                 "unlabelled",
                 col = getUnknowncol(),
                 ncol = 1, bty = "n",
                 pch = 21,
                 cex = legend.cex)
        }
      })
      
    }
  app <- list(ui = ui, server = server)
  runApp(app)
}


## feats
##  features to display on PCA plot
##  profiles to diplay on matplot
##  features to show in DT::datatable

## feats[input$fDataTable_rows_selected]
##  features to highlight
##  feature selected in DT::datatable
