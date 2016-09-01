##' @rdname pRolocVis-apps
##' @param scol The feature meta data column containing the 
##' classification scores.
##' @param mcol The feature meta data column containing the labelled training 
##' data, for use with "classify".
##' @return For \code{classify} a \code{numeric} vector of thresholds, 
##' one per class, to use with \code{\link{getPredictions}}

pRolocVis_classify <- function(object,
                               fcol,
                               scol,
                               mcol = "markers",
                               legend.cex = 1,
                               ...) {

  if (!inherits(object, "MSnSet"))
    stop("The input must be of class MSnSet")
  if (missing(fcol))
    stop("fcol prediction column not specified")
  if (missing(scol)) 
    scol <- paste0(fcol, ".scores")
  if (!scol %in% fvarLabels(object)) 
    stop("scol is not found in fData, please provide scol")
  if (missing(mcol)) 
    mcol <- "markers"
  if (!mcol %in% fvarLabels(object)) 
    stop("fcol missing in fData, please provide mcol")
    

  
  ## Return scores
  on.exit(return(us))
  
  ## Get pcas coords for plot
  pcas <- plot2D(object, fcol = NULL, plot = FALSE, ...)

 
  ## Marker colours
  orgs <- getMarkerClasses(object, fcol = mcol)
  nk <- length(orgs)
  cols <- getStockcol()
  nc <- length(cols)
  if (nc < nk) {
    message("Too many features for available colours. 
            Some colours will be duplicated.")
    n <- nk %/% nc
    cols <- rep(cols, n + 1)
  }
  us <- rep(1, nk)
  names(us) <- orgs
  
  ## scores by organelle for boxplot
  uns <- unknownMSnSet(object, fcol = mcol)
  class.ind <- sapply(orgs, function(z) which(fData(uns)[, fcol] == z))
  scores <- sapply(class.ind, function(z) fData(uns)[z, scol])
  mrk.ind <- sapply(orgs, function(z) which(fData(object)[, mcol] == z))
  
  
  ## User sliders
  min.vals <- rep(0, length(orgs))
  max.vals <- rep(1, length(orgs))
  user.sliders <- vector("list", length(orgs))
  n <- length(orgs)
  for (i in 1:n) {
    user.sliders[[i]] <- sliderInput(inputId = 
                                        paste0("org", 
                                               formatC(i, width = 2, flag = "0")),
                                      label = orgs[i],
                                      min = min.vals[i], 
                                      max = max.vals[i],
                                      value = 1, 
                                      step = .05)
  }
  ## Quantile slider: Needed to add leading zeros to maintain organelle order for `us`
  labelNames <- paste0("org", formatC(1:nk, width = 2, flag = "0"))
  quantile.slider <- sliderInput(inputId = "quantile",
                                 label = NULL,
                                 min = 0,
                                 max = 1,
                                 value = .5,
                                 step = .05)
  
  ## Build shiny interface
  ui <- fluidPage(
    headerPanel("Classification results"),
    sidebarLayout(
      sidebarPanel(
        h3("Type of cut-off"),
        radioButtons("cutoff", NULL, 
                     choices = c("Quantile" = "quant",
                                 "User-defined" = "user")),
        h3("Quantile"),
        quantile.slider,
        h3("User-specified scores"),
        user.sliders,
        width = 3),
        mainPanel(
          absolutePanel(
            fixed = TRUE,
            fluidRow(
              column(10,
                     plotOutput("pca",
                                height = "380px",
                                width = "500px"),
                     plotOutput("boxplot",
                                height = "450px",
                                width = "500px")),
              column(2, 
                     plotOutput("legend",
                                height = "600px",
                                width = "200px"))
              )
            )
          )
      )
    )
  
  ## Shiny server
  server <-
    function(input, output, session) {
      ranges <- reactiveValues(x = NULL, y = NULL)
      ## Update PCA plot according to sliders      
      pcaSel <- reactive({
        k <- names(input)[grep("org", names(input))]
        if (input$cutoff == "quant") {
          us <<- orgQuants(object, fcol = fcol, scol = scol, mcol = mcol,
                           t = input$quantile, verbose = FALSE)
          pred <- getPredictions(object, fcol = fcol, mcol = mcol,
                                 scol = scol, t = us, verbose = FALSE)
          pred <- fData(pred)[, paste0(fcol, ".pred")]
          sapply(orgs, function(z) pcas[which(pred == z), ], USE.NAMES = TRUE)
          ## update user.sliders with the values **RETURNED** 
          ## from getAssignments
        } else {
          for (ii in 1:nk) us[ii] <<- input[[k[ii]]]
          pred <- getPredictions(object, fcol = fcol, mcol = mcol,
                                 scol = scol, t = us, verbose = FALSE)
          pred <- fData(pred)[, paste0(fcol, ".pred")]
          sapply(orgs, function(z) pcas[which(pred == z), ], USE.NAMES = TRUE)
        }
      }) 
      ## PCA plot
      output$pca <- renderPlot({
        par(mar = c(5.1, 4.1, 2, 0)) 
        par(oma = c(0, 0, 0, 0))
        setStockcol(NULL)
        newcols <- paste0(getStockcol(), 60)
        plot(pcas,
             col = getUnknowncol(),
             pch = 21, cex = 1)
        for (i in 1:length(orgs)) {
          points(pcaSel()[[i]],
                 pch = 16,
                 cex = 1,
                 col = newcols[i]
          )
          points(pcas[mrk.ind[[i]], 1], pcas[mrk.ind[[i]], 2], 
                 pch = 16,
                 cex = .7,
                 col = getStockcol()[i])
        }
      })
      ## Add points to boxplot
      nn <- reactive({
        k <- names(input)[grep("org", names(input))]
        if (input$cutoff == "quant") {
          us <<- orgQuants(object, fcol = fcol, scol = scol, 
                           mcol = mcol, t = input$quantile,
                           verbose = FALSE)
        } else {
          for (ii in 1:nk) us[ii] <<- input[[k[ii]]]
          us
        }
      })
      ## Boxplot of scores
      output$boxplot <- renderPlot({
        par(mar = c((max(sapply(orgs, nchar))/2), 4.1, 2, 1)) 
        par(oma = c(0, 0, 0, 0))
        boxplot(scores, las = 2, ylab = "Scores", ylim = c(0, 1.1))
        if (input$cutoff == "quant") 
          lcol = paste0(getStockcol(), 80)[1]
        else
          lcol = paste0(getStockcol(), 80)[2]
        points(1:length(orgs), nn(), col = lcol, pch = 19, cex = 1.5) 
      })
      ## Output legend
      output$legend <- renderPlot({
        par(mar = c(1, 0, 2, 1))
        par(oma = c(0, 0, 0, 0))
        plot(0, type = "n",
             xaxt = "n", yaxt = "n",
             xlab = "", ylab = "",
             bty = "n", main = "Legend",
             adj = 0.05)
        legend("topleft",
               orgs,
               col = getStockcol()[1:length(orgs)],
               ncol = 1, 
               bty = "n",
               pch = 16,
               cex = .8)
      })
#       ##
#       observe({
#         if (input$cutoff == "quant") {
#           k <- names(input)[grep("org", names(input))]
#           n <- as.vector(us)
#           updateSliderInput(session, "org1", value = n[1]) 
#           updateSliderInput(session, "org2", value = n[2]) 
#           updateSliderInput(session, "org3", value = n[3]) 
#         }
#         #updateSliderInput(session, "org01", value = .5)
#       })
    }
  app <- list(ui = ui, server = server)
  runApp(app)
}
