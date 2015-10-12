##' pRoloc interactive visualisation
##' 
##' @title Visualise protein profiles in pRoloc data
##' @param object An instance of class \code{MSnSet}.
##' @param fcol The name of the markers matrix (default is
##' \code{"Markers"}). Can be missing if \code{foi} is available.
##' @param legend.cex Character expansion for the vignette
##' labels. Default is 1.
##' @param legend.ncol Number of columns to use for the legeng. 
##' Default is 1.
##' @param all If there are more than 10 clusters, only the first
##' three are discplayed on start-up, unless \code{all} is set to
##' \code{TRUE}. Default is \code{FALSE}.
##' @param method An instance of class \code{matrix} where its 
##' rownames must match the object's feature names and represent 
##' a projection of the data in object in two dimensions, as 
##' produced (and invisibly returned) by \code{plot2D}.
##' @param ... Additional parameters that can be used to choose the
##' dimentionality reduction method, as defined in
##' \code{\link{plot2D}}.
##' @author Lisa M Breckels
##' @examples
##' library("pRoloc")
##' library("pRolocdata")
##' data(dunkley2006)
##' ## markers matrix ecoding
##' dunkley2006 <- mrkVecToMat(dunkley2006)
##' if (interactive())
##'   pRolocVis_profiles(dunkley2006)
pRolocVis_profiles <- function(object, fcol,
                               legend.cex = 1,
                               legend.ncol = 1,
                               all = FALSE,
                               method,
                               ...) {
  if (!inherits(object, "MSnSet"))
    stop("The input must be of class MSnSet")
  if (missing(fcol)) 
    fcol <- "Markers"
  if (!missing(fcol)) {
    if (!fcol %in% fvarLabels(object))
      stop("fcol missing in fData")
    if (!isMrkMat(object, fcol))
      stop("Markers must be encoded as a matrix. See ?markers for details.")
    pmarkers <- fData(object)[, fcol]
  }
  if (!missing(method)) {
    if (!inherits(method, "matrix"))
      stop("Input object method must be of class matrix")
    if (all(rownames(method) != featureNames(object)))
      stop("rownames in method and featureNames in object do not match") 
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

  ## a hyphen in a pmarkers name breaks the app?!?
  colnames(pmarkers) <- gsub("-", "", colnames(pmarkers))
  
  ## If there are too many marker sets, better
  ## to display few and let the user choose
  pmsel <- TRUE
  if (!all & ncol(pmarkers) > 10)
    pmsel <- 1:3
  
  ## data to be displayed
  if (missing(method)) 
    pcas <- plot2D(object, fcol = NULL, plot = FALSE, ...)
  else
    pcas <- method[, 1:2]
  profs <- exprs(object)

  ## --------------------Build shiny app--------------------------
  ui <- fluidPage(
    headerPanel("LOPIT visualisation"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput("markers", "Markers",
                       choices = colnames(pmarkers),
                       multiple = TRUE,
                       selected = colnames(pmarkers)[pmsel]),
        sliderInput("trans", "Transparancy",
                    min = 0,  max = 1, value = 0.5),
        width = 3),
      mainPanel(
        fluidRow(
          column(7,
                 plotOutput("pca",
                            height = "350px",
                            width = "100%"),
                 plotOutput("profile",
                            height = "400px",
                            width = "700px")),
          column(3, 
                 plotOutput("legend",
                            height = "350px",
                            width = "500px"))
        )
      )
    )
  )
                                      
  
  ### ---------------------------------SERVER-----------------------------
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
        par(mar = c(5.1, 4.1, 2, 1)) 
        par(oma = c(0, 0, 0, 0))
        plot(pcas,
             col = getUnknowncol(),
             pch = 21, cex = 1, main = "PCA")
        for (i in 1:length(input$markers)) {
          points(pcaMrkSel()[[i]], 
                 pch = 16, 
                 cex = 1.4, 
                 col = myCols()[i])
        } 
      })
      
      ## Protein profile
      output$profile <- renderPlot({
        ylim <- range(profs)
        n <- nrow(profs)
        m <- ncol(profs)
        fracs <- sampleNames(object)
        par(mar = c(8, 5, 2, 1))
        par(oma = c(0, 0, 0, 0))
        plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
             type = "n", xaxt = "n", xlab = "", main = "Protein profiles")
        axis(1, at = 1:m, labels = fracs, las = 2)
        title(xlab = "Fractions", line = 5.5)
        matlines(t(profs[1:n, ]),
                col = getUnknowncol(),
                lty = 1,
                type = "l")
        for (i in 1:length(input$markers)) { 
          matlines(t(profMrkSel()[[i]]),
                   col = myCols()[i],
                   lty = 1,
                   lwd = 1.5) 
        }
      })
         
      ## Output legend
      output$legend <- renderPlot({
        par(mar = c(1, 1, 2, 1))
        par(oma = c(0, 0, 0, 0))
        plot(0, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             bty = "n", main = "Legend",
             adj = 0.05)
        legend("topleft",
               input$markers,
               col = myCols(),
               ncol = legend.ncol, 
               bty = "n",
               pch = 16,
               cex = legend.cex)
      })
    }
  app <- list(ui = ui, server = server)
  runApp(app)
}

