## TODO:
## - foi, passed as arg
## - fix legend
## - profiles selection and highlighting

## Shiny: spinning loading wheel on top of plot while plot is recalculating
## https://gist.github.com/daattali/edd7c20cd09f484b7f32

## Very slow with bigger data (fusion data), server side table flaky
## and warinings about size

## References
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://gallery.shinyapps.io/095-plot-interaction-advanced/
## https://gallery.shinyapps.io/105-plot-interaction-zoom/
## https://gallery.shinyapps.io/106-plot-interaction-exclude/
## https://github.com/rstudio/shiny-examples

##' pRoloc interactive visualisation
##' 
##' @title Visualise your pRoloc data
##' @param object An instance of class \code{MSnSet}.
##' @param fcol The name of the markers matrix. Default is
##' \code{"Markers"}.
##' @param ... Additional parameters that can be used to choose the
##' dimentionality reduction method, as defined in
##' \code{\link{plot2D}}.
##' @author Laurent Gatto
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(dunkley2006)
##' ## adds matrix markers
##' dunkley2006 <- mrkVecToMat(dunkley2006)
##' dunkley2006 <- dunkley2006[, order(dunkley2006$fraction)]
##' pRolocVis2(dunkley2006)
pRolocVis2 <- function(object, fcol = "Markers",
                       foi,
                       fig.height = "600px",
                       fig.width = "600px",
                       legend.cex = 1,
                       ...) {
    if (!inherits(object, "MSnSet"))
        stop("The input must be of class MSnSet")
    if (is.null(fData(object)[, fcol]))
        stop("fcol missing in fData")
    if (!isMrkMat(object, fcol))
        stop("Markers must be encoded as a matrix.")
    pmarkers <- fData(object)[, fcol]        
    if (length(grep("GO:", colnames(pmarkers))) > 0) {
        cn <- pRoloc::flipGoTermId(colnames(pmarkers))
        names(cn) <- NULL
        colnames(pmarkers) <- cn
    }
    if (!pRoloc::isMrkMat(object, fcol))
        stop("Selected feature data is not a matrix of markers")
    pcas <- pRoloc::plot2D(object, fcol = NULL, plot = FALSE, ...)
    profs <- exprs(object)
    cols <- pRoloc::getLisacol()
    if (length(cols) < ncol(pmarkers)) {
        n <- ncol(pmarkers) %/% length(cols)
        cols <- rep(cols, n + 1)
    }
    pmsel <- TRUE
    if (ncol(pmarkers) > 10)
        pmsel <- 1:3
    
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
                plotOutput("legend"),
                width = 2),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("PCA",
                                     fluidRow(
                                         column(9, offset = 1,
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
                                                               resetOnNew = TRUE))))
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
                                       column(ncol(fData(object)), ## FIXME - this is limited to 12
                                              DT::dataTableOutput("brushDataTable"))))
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
                scales::alpha(cols, input$trans)[
                                                 sapply(input$markers, function(z) 
                                                     which(colnames(pmarkers) == z))]
            })
            ## PCA plot
            output$pca <- renderPlot({
                par(mar = c(5.1, 4.1, 1, 1))
                plot(pcas,
                     col = getUnknowncol(),
                     pch = 21, cex = 1,
                     xlim = ranges$x,
                     ylim = ranges$y)
                usr <<- par("usr")
                for (i in 1:length(input$markers)) 
                    points(pcaMrkSel()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
                ## FIXME this does not work when brushed/subset of points selected
                s <- input$brushDataTable_rows_selected
                if (length(s))
                    points(pcas[s, , drop = FALSE], pch = 19, cex = 2)
            })
            ## Protein profile
            output$profile <- renderPlot({
                par(mar = c(5.1, 4.1, 1, 1))
                matplot(t(profs),
                        col = getUnknowncol(),
                        lty = 1,
                        type = "l")
                for (i in 1:length(input$markers)) 
                    matlines(t(profMrkSel()[[i]]),
                             col = myCols()[i],
                             lty = 1,
                             lwd = 1.5)
                s <- input$brushDataTable_rows_selected
                if (length(s))
                    matlines(t(profs[s, ]),
                             col = "black",
                             lty = 1,
                             lwd = 2)
            })                        
            ## Freature data table
            output$brushDataTable <- DT::renderDataTable({
                if (is.null(input$pcaBrush)) {
                    i <- try(pcas[, 1] >= usr[1] & pcas[, 1] <= usr[2])
                    j <- try(pcas[, 2] >= usr[3] & pcas[, 2] <= usr[4])
                } else {
                    i <- pcas[, 1] >= input$pcaBrush$xmin & pcas[, 1] <= input$pcaBrush$xmax
                    j <- pcas[, 2] >= input$pcaBrush$ymin & pcas[, 2] <= input$pcaBrush$ymax
                }
                DT::datatable(fData(object)[i & j, ],
                              rownames = TRUE,
                              options = list(searchHighlight = TRUE),
                              filter = 'top')
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
                plot(0, type = "n",
                     xaxt = "n", yaxt = "n",
                     xlab = "", ylab = "",
                     bty = "n")
                legend("center",
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


