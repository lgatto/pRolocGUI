##' Shiny App for visualising a matrix of markers
##' 
##' @title Plot a matrix of markers
##' @param object An instance of class \code{MSnSet}.
##' @param fcol The name of the markers matrix. Default is
##' \code{"Markers"}.
##' @param ncol. Number of columns to be used for the legend. The
##' default value (4 of less) depends on the length of the
##' labels. Reduce this if the labels are too long and the legend does
##' not fit in the display panel.
##' @param ... Additional parameters that can be used to choose the
##' dimentionality reduction method, as defined in
##' \code{\link{plot2D}}.
##' @author Lisa M Breckels
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(dunkley2006)
##' ## adds matrix markers
##' dunkley2006 <- mrkVecToMat(dunkley2006)
##' if (interactive())
##'    plotMat2D(dunkley2006)
plotMat2D <- function(object, fcol = "Markers", ncol., ...) {
    if (!inherits(object, "MSnSet"))
        stop("The input must be of class MSnSet")
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
    ## when labels are too long, parts of the legend
    ## span outside the panel
    if (missing(ncol.)) {
        mx <- max(nchar(colnames(pmarkers)))
        if (mx > 50) ncol. <- 1
        if (mx <= 50) ncol. <- 2
        if (mx < 35) ncol. <- 3
        if (mx < 15) ncol. <- 4
    }
    ## Build shiny app
    ui <- shinyUI(pageWithSidebar(
        headerPanel("Marker Visualisation"),
        sidebarPanel(
            selectizeInput("goTerms", "GO CC term",
                           choices = colnames(pmarkers),
                           multiple = TRUE, selected = colnames(pmarkers)[1]),
            sliderInput("trans", "Transparancy",
                        min = 0,  max = 1, value = 0.5)),
        mainPanel(plotOutput("plot1"),
                  h4("Legend", align = "center"),
                  plotOutput("legend"))
        ))

    server <-
        shinyServer(function(input, output, session) {
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
                        output$plot1 <- renderPlot({
                            par(mar = c(5.1, 4.1, 0, 1))
                            plot(pcas,
                                 col = getUnknowncol(),
                                 pch = 21, cex = 1)
                            for (i in 1:length(input$goTerms)) {
                                points(pcasGo()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
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
