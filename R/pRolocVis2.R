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

##' pRoloc interactive visualisation
##' 
##' @title Visualise your pRoloc data
##' @param object An instance of class \code{MSnSet}.
##' @param fcol The name of the markers matrix (default is
##' \code{"Markers"}). Can be missing if \code{foi} is available.
##' @param foi A \code{\link{FeaturesOfInterest}} or a
##' \code{\link{FoICollection}}, that will be available for display.
##' @param fig.height Height of the figure. Default is \code{"600px"}.
##' @param fig.width Width of the figure. Default is \code{"600px"}.
##' @param legend.width Width of the legend. Default is \code{"100\%"}.
##' @param legend.cex Character expansion for the vignette
##' labels. Default is 1.
##' @param nchar Maximum number of characters if the markers class
##' names, before their names are truncated. Default is 10.
##' @param all If there are more than 10 clusters, only the first
##' three are discplayed on start-up, unless \code{all} is set to
##' \code{TRUE}. Default is \code{FALSE}.
##' @param ... Additional parameters that can be used to choose the
##' dimentionality reduction method, as defined in
##' \code{\link{plot2D}}.
##' @author Laurent Gatto
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(dunkley2006)
##' ## markers matrix ecoding
##' dunkley2006 <- mrkVecToMat(dunkley2006)
##' ## order the fractions 
##' dunkley2006 <- dunkley2006[, order(dunkley2006$fraction)]
##' if (interactive())
##'   pRolocGUI:::pRolocVis2(dunkley2006)
pRolocVis2 <- function(object, fcol,
                       foi,
                       fig.height = "600px",
                       fig.width = "100%",
                       legend.width = "100%",
                       legend.cex = 1,
                       nchar = 15,
                       all = FALSE,
                       ...) {
    if (!inherits(object, "MSnSet"))
        stop("The input must be of class MSnSet")
    if (missing(foi) & missing(fcol)) 
        fcol <- "Markers"
    if (!missing(fcol)) {
        if (!fcol %in% fvarLabels(object))
            stop("fcol missing in fData")
        if (!isMrkMat(object, fcol))
            stop("Markers must be encoded as a matrix. See ?markers for details.")
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
    if ((nfd <- length(fvarLabels(object))) > 12) {
        message("There can't be more than 12 feature variables. Using 6 first and last.")
        fData(object) <- fData(object)[, c(1:6, (nfd-5):nfd)]
    }
    ## a hyphen in a pmarkers name breaks the app?!?
    colnames(pmarkers) <- gsub("-", "", colnames(pmarkers))
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
        pmsel <- 1:3

    ## data to be displayed
    pcas <- pRoloc::plot2D(object, fcol = NULL, plot = FALSE, ...)
    profs <- exprs(object)

    ## all feautres are displayed on start
    feats <- 1:nrow(object)
    
    ## Build shiny app
    ui <- fluidPage(
        sidebarLayout(
            sidebarPanel(
                selectizeInput("markers", "Markers",
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
                                         column(10, 
                                                plotOutput("pca",
                                                           height = fig.height,
                                                           width = fig.width,
                                                           click = "pcaClick",
                                                           dblclick = "pcaDblclick",
                                                           hover = hoverOpts(
                                                               id = "pcaHover"
                                                               ),
                                                           brush = brushOpts(
                                                               id = "pcaBrush",
                                                               resetOnNew = TRUE)),
                                                offset = 0),
                                         column(2, 
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
                par(mar = c(4, 4, 0, 0))
                par(oma = c(1, 0, 0, 0))
                plot(pcas,
                     col = getUnknowncol(),
                     pch = 21, cex = 1,
                     xlim = ranges$x,
                     ylim = ranges$y)
                usr <<- par("usr")
                for (i in 1:length(input$markers)) 
                    points(pcaMrkSel()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
                ## FIXME this does not work when brushed/subset of points selected
                s <- feats[input$fDataTable_rows_selected]
                if (length(s)) {
                    points(pcas[s, , drop = FALSE],
                           pch = 19, cex = 2, col = "#00000060")
                    if (input$checkbox)
                        text(pcas[s, 1], pcas[s, 2],
                             rownames(pcas)[s],
                             pos = 1)
                }
            })
            ## Protein profile
            output$profile <- renderPlot({
                par(mar = c(5.1, 4.1, 1, 1))
                matplot(t(profs[feats, ]),
                        col = getUnknowncol(),
                        lty = 1,
                        type = "l")
                ## for (i in 1:length(input$markers)) 
                ##     matlines(t(profMrkSel()[[i]]),
                ##              col = myCols()[i],
                ##              lty = 1,
                ##              lwd = 1.5)
                s <- feats[input$fDataTable_rows_selected]
                if (length(s))
                    matlines(t(profs[s, , drop = FALSE]),
                             col = "black",
                             lty = 1,
                             lwd = 2)
            })                        
            ## Freature data table
            output$fDataTable <- DT::renderDataTable({
                if (is.null(input$pcaBrush)) {
                    i <- try(pcas[, 1] >= usr[1] & pcas[, 1] <= usr[2])
                    j <- try(pcas[, 2] >= usr[3] & pcas[, 2] <= usr[4])
                } else {
                    i <- pcas[, 1] >= input$pcaBrush$xmin & pcas[, 1] <= input$pcaBrush$xmax
                    j <- pcas[, 2] >= input$pcaBrush$ymin & pcas[, 2] <= input$pcaBrush$ymax
                }
                feats <<- which(i & j)
                DT::datatable(fData(object)[i & j, ],
                              rownames = TRUE,
                              options = list(searchHighlight = TRUE))
                              ## filter = 'top')
            })
            ## When a double-click happens, check if there's a brush on the plot.
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
            ## Output legend
            output$legend <- renderPlot({
                par(mar = rep(1, 4))
                par(oma = c(1, 0, 0, 0))
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
