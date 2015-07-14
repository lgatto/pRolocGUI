##' Shiny App for visualising a matrix of markers
##' 
##' @title Plot a matrix of markers
##' @param object An instance of class \code{MSnSet}.
##' @param fcol The name of the markers matrix. Default is
##' \code{"Markers"}.
##' @param ncol. Number of columns to be used for the legend. Default
##' is 2.
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
##' pRolocVis2(dunkley2006)
pRolocVis2 <- function(object, fcol = "Markers", ncol. = 2, ...) {
    if (!inherits(object, "MSnSet"))
        stxoop("The input must be of class MSnSet")
    if (is.null(fData(object)[, fcol]))
        stop("fcol missing in fData")
    pmarkers <- fData(object)[, fcol]
    if (length(grep("GO:", colnames(pmarkers))) > 0) {
        cn <- pRoloc::flipGoTermId(colnames(pmarkers))
        names(cn) <- NULL
        colnames(pmarkers) <- cn
    }
    if (!pRoloc::isMrkMat(object, fcol))
        stop("Selected feature data is not a matrix of markers")
    pcas <- pRoloc::plot2D(object, fcol = NULL, plot = FALSE, ...)
    cols <- pRoloc::getLisacol()
    if (length(cols) < ncol(pmarkers)) {
        n <- ncol(pmarkers) %/% length(cols)
        cols <- rep(cols, n + 1)
    }
    ## Build shiny app
    ui <- shinyUI(pageWithSidebar(
        headerPanel("pRoloc visualisation interface"),
        sidebarPanel(
            selectizeInput("goTerms", "GO CC term",
                           choices = colnames(pmarkers),
                           multiple = TRUE, selected = colnames(pmarkers)[1]),
            sliderInput("trans", "Transparancy",
                        min = 0,  max = 1, value = 0.5),
            plotOutput("legend")),
        mainPanel(
            plotOutput("pca",
                       click = "pcaClick",
                       dblclick = "pcaDblclick",
                       hover = hoverOpts(
                           id = "pcaHover"
                           ),
                       brush = brushOpts(
                           id = "pcaBrush",
                           resetOnNew = TRUE)),
            fluidRow(
                column(ncol(fData(dunkley2006)),
                       DT::dataTableOutput("brushDataTable")
                       )
                )
            )
        ))

    server <-
        shinyServer(function(input, output, session) {
                        ranges <- reactiveValues(x = NULL, y = NULL)                        
                        ## Get coords for proteins according to GO term specified in input
                        pcasGo <- reactive({
                            lapply(input$goTerms, function(z) pcas[which(pmarkers[, z] == 1), ])
                        })
                        ## Update colour transparacy according to slider input
                        myCols <- reactive({
                            scales::alpha(cols, input$trans)[
                                                             sapply(input$goTerms, function(z) 
                                                                 which(colnames(pmarkers) == z))]
                        })
                        ## Output main plot
                        output$pca <- renderPlot({
                            par(mar = c(5.1, 4.1, 0, 1))
                            plot(pcas,
                                 col = getUnknowncol(),
                                 pch = 21, cex = 1,
                                 xlim = ranges$x,
                                 ylim = ranges$y)
                            for (i in 1:length(input$goTerms)) {
                                points(pcasGo()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
                            }
                            s <- input$brushDataTable_rows_selected
                            if (length(s))
                                points(pcas[s, , drop = FALSE], pch = 19, cex = 2)
                        })
                        ## points information
                        ## output$click_info <- renderPrint({
                        ##     cat("input$pcaClick:\n")
                        ##     str(input$pcaClick)
                        ## })
                        ## output$hover_info <- renderPrint({
                        ##     cat("input$pcaHover:\n")
                        ##     str(input$pcaHover)
                        ## })
                        output$brushDataTable <- DT::renderDataTable({
                            if (is.null(input$pcaBrush)) {
                                j <- i <- TRUE
                            } else {
                                    i <- pcas[, 1] >= input$pcaBrush$xmin & pcas[, 1] <= input$pcaBrush$xmax
                                    j <- pcas[, 2] >= input$pcaBrush$ymin & pcas[, 2] <= input$pcaBrush$ymax
                                }
                            DT::datatable(fData(object)[i & j, ],
                                          rownames = TRUE,
                                          options = list(searchHighlight = TRUE))
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
                            legend("center",
                                   input$goTerms, col = myCols(),
                                   ncol = ncol., bty = "n",
                                   pch = 16, cex = 1.4)
                        })
                    })
    app <- list(ui = ui, server = server)
    runApp(app)
}
