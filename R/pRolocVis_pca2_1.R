## This app: brushing does not work but clicking on and off by PCA and TABLE
## works (SINGLE CLICK)

## TODO:
## - profiles selection and highlighting

## Shiny: spinning loading wheel on top of plot while plot is recalculating
## https://gist.github.com/daattali/edd7c20cd09f484b7f32

## Very slow with bigger data (fusion data), server side table flaky
## and warnings about size

## Possibly automate mrkVecToMat?

## References
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://gallery.shinyapps.io/095-plot-interaction-advanced/
## https://gallery.shinyapps.io/105-plot-interaction-zoom/
## https://gallery.shinyapps.io/106-plot-interaction-exclude/
## https://github.com/rstudio/shiny-examples

## http://shiny.rstudio.com/articles/selecting-rows-of-data.html
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html

##' @rdname pRolocVis-apps
##' @param foi A \code{\link{FeaturesOfInterest}} or a
##' @param fig.height Height of the figure. Default is \code{"600px"}.
##' @param fig.width Width of the figure. Default is \code{"600px"}.
##' @param legend.width Width of the legend. Default is \code{"100\%"}.
##' @param nchar Maximum number of characters of the markers class
##' names, before their names are truncated. Default is 10.
##' @param all If \code{TRUE} all clusters are displayed on startup.
##' If \code{FALSE} only first cluster in the list is displayed.
##' \code{\link{FoICollection}}, that will be available for display.
##' @param fdataInd A \code{numeric} or \code{character} of valid feature
##' variables to retain for the data table. If not specified, by default
##' \code{markers} will be kept along with the first 5 and last 6 feature
##' variables.
plotpca <- function(object, fcol,
                          foi,
                          fig.height = "600px",
                          fig.width = "100%",
                          legend.width = "200%",
                          legend.cex = 1,
                          nchar = 40,
                          all = TRUE,
                          method,
                          fdataInd) {

    if (!inherits(object, "MSnSet"))
        stop("The input must be of class MSnSet")
    if (missing(foi) & missing(fcol)) 
        fcol <- "markers"
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
    ## Remove fcol from fData(object)    
    fData(object) <- fData(object)[, -grep(fcol, fvarLabels(object))]
    ## There can't be more than 12 columns in the DT table
    if (missing(fdataInd)) {
      if ((nfd <- length(fvarLabels(object))) > 12) {
        message("There can't be more than 12 feature variables. Using 6 first and last.")
        object <- narrowFeatureData(object)
      }
    } else {
      if(length(fdataInd) > 12)
        stop("There can't be more than 12 feature variables, check fdataInd")
      object <- selectFeatureData(object, fcol = fdataInd)
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
    feats <- 1:nrow(object)
    
    idxTable <- numeric()
    
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
                width = 2),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("PCA",
                                     fluidRow(
                                         column(9, 
                                                plotOutput("pca",
                                                           height = fig.height,
                                                           width = fig.width,
                                                           click = "pcaClick",
                                                           dblclick = "pcaDblclick",
                                                           brush = brushOpts(
                                                               id = "pcaBrush",
                                                               resetOnNew = TRUE)),
                                                offset = 0),
                                         column(3, 
                                                plotOutput("legend",
                                                           height = fig.height,
                                                           width = legend.width))
                                         )
                                     ),
                            tabPanel("Profiles",
                                     fluidRow(
                                         column(9, offset = 1,
                                                plotOutput("profile",
                                                           height = fig.height,
                                                           width = fig.width)))
                                     ),
                            ## feature data table is always visible
                            fluidRow(
                                column(12,
                                       column(ncol(fData(object)), 
                                              DT::dataTableOutput("fDataTable"))))
                            )
                )
            ))
    
    server <-
        function(input, output, session) {
            ranges <- reactiveValues(x = NULL, y = NULL)

            ## Get coords for proteins according to selectized marker class(es)
            pcaMrkSel <- reactive({
                lapply(input$markers,
                       function(z) pcas[which(pmarkers[, z] == 1), ])
            })
            profMrkSel <- reactive({
                lapply(input$markers,
                       function(z) profs[which(pmarkers[, z] == 1), ])
            })

            ## Update colour transparacy according to slider input
            myCols <- reactive({
                scales::alpha(cols,
                              input$trans)[sapply(input$markers, function(z) 
                                  which(colnames(pmarkers) == z))]
            })
            
            ## PCA plot
            output$pca <- renderPlot({
                idxTable <<- feats[input$fDataTable_rows_selected]
                par(mar = c(4, 4, 0, 0))
                par(oma = c(1, 0, 0, 0))
                # plot(pcas, 
                plot2D(object,
                       col = getUnknowncol(),
                       pch = 21, cex = 1,
                       xlim = ranges$x,
                       ylim = ranges$y)
                usr <<- par("usr")
                for (i in 1:length(input$markers)) 
                    points(pcaMrkSel()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
                
                ## highlight point on plot by selecting item in table
                idxTable <<- feats[input$fDataTable_rows_selected]
                if (length(idxTable)) {
                    selfoi <- featureNames(object)[idxTable]
                    if (input$checkbox) {
                      # posLabs <- ifelse(pcas[selfoi, 1] > 0, yes = 2, no = 4)
                      pos = 3
                      highlightOnPlot(object, selfoi, cex = 1.3)
                      highlightOnPlot(object, selfoi, labels = TRUE, pos = 3)
                    } else {
                      highlightOnPlot(object, selfoi, pos = 3, cex = 1.3)
                    }
                }
             })
            
            ## Protein profile
            output$profile <- renderPlot({
                par(mar = c(5.1, 4.1, 1, 1))
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
                idxTable <- feats[input$fDataTable_rows_selected]
                if (length(idxTable))
                    matlines(t(profs[idxTable, , drop = FALSE]),
                             col = "black",
                             lty = 1,
                             lwd = 2)
            })             
            
            ## Feature data table
            output$fDataTable <- DT::renderDataTable({
                if (is.null(input$pcaBrush)) {
                    i <- try(pcas[, 1] >= usr[1] & pcas[, 1] <= usr[2])
                    j <- try(pcas[, 2] >= usr[3] & pcas[, 2] <= usr[4])
                } else {
                    i <- pcas[, 1] >= input$pcaBrush$xmin & pcas[, 1] <= input$pcaBrush$xmax
                    j <- pcas[, 2] >= input$pcaBrush$ymin & pcas[, 2] <= input$pcaBrush$ymax
                }
                feats <<- which(i & j)
 
                if (!is.null(input$pcaClick)) {         
                  dist <- apply(pcas, 1, function(z) sqrt((input$pcaClick$x - z[1])^2 
                                                          + (input$pcaClick$y - z[2])^2))
                  idxPlot <- which(dist == min(dist))
                  if (idxPlot %in% idxTable)                     ## 1--is it already clicked?
                    idxTable <<- setdiff(idxTable, idxPlot)           ## Yes, remove it from table
                  else                                           ## 2--new click?
                    idxTable <<- c(idxTable, idxPlot)                 ## Yes, add it to table
                }
                idxTable <<- as.numeric(idxTable)
                DT::datatable(data = fData(object)[i & j, ], 
                              rownames = TRUE,
                              selection = list(mode = 'multiple', selected = idxTable))
            })
            
            ## When a double-click happens, check if there'idxTable a brush on the plot.
            ## If so, zoom to the brush bounds; if not, reset the zoom.
            observeEvent(input$pcaDblclick, {
                             brush <- input$pcaBrush
                             if (!is.null(brush)) {
                                 ranges$x <- c(brush$xmin, brush$xmax)
                                 ranges$y <- c(brush$ymin, brush$ymax)
                             } else {
                                 ranges$x <- NULL
                                 ranges$y <- NULL
                             }                                         
                         })
            
            ## When an area is clicked on the pca plot, a protein is then
            ## highlighted with label (see output$pca), now select row in
            ## datatable to highlight
            
#             
#             observeEvent(input$pcaClick, {
#               if (!is.null(input$pcaClick$x)) {
#               highlightRow <- 
#               select
#               
#               
#               
#                 dist <- apply(pcas, 1, function(z) 
#                   sqrt((input$pcaClick$x - z[1])^2 + (input$pcaClick$y - z[2])^2))
#                 idxTable <- which(dist == min(dist))
#               
#               
#               
#            })
            
            ## Output legend
            output$legend <- renderPlot({
                par(mar = c(0, 0, 0, 0))
                par(oma = c(0, 0, 0, 0))
                plot(0, type = "n",
                     xaxt = "n", yaxt = "n",
                     xlab = "", ylab = "",
                     bty = "n")
                legend("topleft",
                       input$markers,
                       col = myCols(),
                       ncol = 1, bty = "n",
                       pch = 16,
                       cex = legend.cex)
            })
            


#             ## Print out protein name
#             output$pcaHoverInfo <- {
#               if (!is.null(.minHelper()))
#                 rownames(pcas)[.minHelper()]
#             }
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
