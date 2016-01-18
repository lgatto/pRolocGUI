pRolocVis_compare2 <- function(object, fcol1, fcol2,
                               foi,
                               fig.height = "600px",
                               fig.width = "100%",
                               legend.width = "200%",
                               legend.cex = 1,
                               remap = TRUE,
                               nchar = 40,
                               all = TRUE,
                               method,
                               fDataInd1, fDataInd2) {
  
  
  ## Return featureNames of proteins selected
  on.exit(return(invisible(names(idxDT))))
  
  ## Specify fDataInd first
  ## There can't be more than 12 columns in the DT table
  if (missing(fDataInd1)) {
    if (length(fvarLabels(object[[1]])) > 12) {
      message("There can't be more than 12 feature variables. Using 6 first and last.")
      xx <- pRolocGUI:::narrowFeatureData(object[[1]])
      fDataInd1 <- fvarLabels(xx)
    } else {
      fDataInd1 <- fvarLabels(object[[1]])
    }
  } else {
    if (is.numeric(fDataInd1))
      fDataInd1 <- fvarLabels(object[[1]])[fDataInd1]
    
    if (length(fDataInd1) > 12) {
      stop("There can't be more than 12 feature variables, check fDataInd1")
    } else {
      if (!all(fDataInd1 %in% fvarLabels(object[[1]])))
        stop("Check fDataInd1, not all features found in fData")
    }  
  }
  if (missing(fDataInd2)) {
    if (length(fvarLabels(object[[2]])) > 12) {
      message("There can't be more than 12 feature variables. Using 6 first and last.")
      xx <- pRolocGUI:::narrowFeatureData(object[[2]])
      fDataInd2 <- fvarLabels(xx)
    } else {
      fDataInd2 <- fvarLabels(object[[2]])
    }
  } else {
    if (is.numeric(fDataInd2))
      fDataInd2 <- fvarLabels(object[[2]])[fDataInd2]
    
    if (length(fDataInd2) > 12) {
      stop("There can't be more than 12 feature variables, check fDataInd2")
    } else {
      if (!all(fDataInd2 %in% fvarLabels(object[[2]])))
        stop("Check fDataInd2, not all features found in fData")
    }  
  }
  
  ## Class checks
  if (!inherits(object, "MSnSetList"))
    stop("The input must be of class MSnSetList")
  if (!all.equal(featureNames(object[[1]]), featureNames(object[[2]]))) {
    message("Subsetting MSnSetList to their common feature names")
    object <- commonFeatureNames(object)
  }
  
  
  ## Take intersection of datasets
  object <- commonFeatureNames(object)
  
  
  ## fcol checks
  if (missing(fcol1 | fcol2)) {
    if (missing(fcol1 & fcol2)) {
      fcol1 <- "markers"
      if (!fcol1 %in% fvarLabels(object[[1]])) {
        stop("No fcol1 specificed, default fcol1 = markers can not be found")
      } else {
        fcol2 <- "markers"
        if (!fcol2 %in% fvarLabels(object[[2]]))
          fData(object[[2]])[, fcol2] <- fData(object[[1]])[, fcol1]
      }
    } else {
      if (missing(fcol1)) {
        fcol1 <- fcol2
        if (!fcol1 %in% fvarLabels(object[[1]])) {
          message("No fcol1 is specified, using the markers from fcol2")
          fData(object[[1]])[, fcol1] <- fData(object[[2]])[, fcol2]
        }
      } else {
        if (missing(fcol2)) {
          fcol2 <- fcol1
          if (!fcol2 %in% fvarLabels(object[[2]])) {
            message("No fcol2 is specified, using the markers from fcol1")
            fData(object[[2]])[, fcol2] <- fData(object[[1]])[, fcol1]
          }
        }
      }
    }
  } else {
    if (!fcol1 %in% fvarLabels(object[[1]])) 
      stop("fcol1 is not found in fvarLabels")
    if (!fcol2 %in% fvarLabels(object[[2]])) 
      stop("fcol2 is not found in fvarLabels")  
  } 
  
  ## Make fcol matrix of markers if it's not already
  tf <- !unlist(lapply(object, isMrkMat, fcol))
  if (any(tf)) {
    ind <- which(tf)
    x <- vector("list", 2)
    for (i in 1:length(ind)) {
      mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
      x[[i]] <- mrkVecToMat(object[[i]], fcol, mfcol = mName)
      fcol.tmp <- mName 
    }
  }   
  pmarkers <- lapply(x, function(z) fData(z)[, fcol.tmp])

  ## Setting features to be displayed
  if (!missing(foi)) {
    if (inherits(foi, "FeaturesOfInterest"))
      foi <- FoICollection(list(foi))
    foimarkers <- as(foi, "matrix")
    if (exists("pmarkers", inherits = FALSE)) {
      pmarkers <- lapply(pmarkers, function(z) 
        merge(z, foimarkers, by = 0, all.x = TRUE))
      for (i in 1:length(pmarkers)) {
        rownames(pmarkers[[i]]) <- pmarkers[[i]][, "Row.names"]
        pmarkers[[i]] <- pmarkers[[i]][featureNames(object[[i]]), -1]     
      }
               
    } else pmarkers <- foimarkers
  }
  
  ## Convert GO names to CC names
  if (length(grep("GO:", colnames(pmarkers[[1]]))) > 0) {
    for (i in 1:length(pmarkers)) {
      cn <- pRoloc::flipGoTermId(colnames(pmarkers[[i]]))
      if (all(!is.na(cn))) {
        names(cn) <- NULL
        colnames(pmarkers[[i]]) <- cn
      } 
    }
  }  
  
  ## Marker colours
  cols <- getStockcol()
  if (length(cols) < max(sapply(pmarkers, ncol))) {
    message("Too many features for available colours. Some colours will be duplicated.")
    ind <- which.max(sapply(pmarkers, ncol))
    n <- ncol(pmarkers[[ind]]) %/% length(cols)
    cols <- rep(cols, n + 1)
  }
  
  ## Shorten markers names if too long
  for (i in 1:length(object)) {
    cn <- sapply(colnames(pmarkers[[i]]),
                 function(x) {
                   if (nchar(x) > nchar) {
                     x <- strsplit(x, "")[[i]]
                     x <- paste(x[1:nchar], collapse = "")
                     x <- sub(" +$", "", x)
                     x <- paste0(x, "...")
                   }
                   return(x)
                 })    
    names(cn) <- NULL
    colnames(pmarkers[[i]]) <- cn
  }
  
  
  ## Display all classes unless user specifies not to
  pmsel <- TRUE
  if (!all & max(sapply(pmarkers, ncol)) > 10)
    pmsel <- 1    
  
  
  # data to be displayed
  if (missing(method)) {
    pcas <- lapply(object, function(z) plot2D(z, fcol = NULL, plot = FALSE))
  } else {
    if (pRoloc:::.validpRolocVisMethod(method)) {
      if (class(method) == "matrix") {
        if (nrow(method) != nrow(object))
          stop("nrow(method) matrix is not equal to nrow(object)")
        pcas <- method
      } else {
        pcas <- plot2D(object, fcol = NULL, plot = FALSE, method = method)
      }
    } else {
      stop("Invalid visualisation method")
    }
  }   

    

    
    

#     ## get data to display
#     if (remap) {
#       pcas <- remap(object, ref = ref)
#       pcas <- lapply(pcas, plot2D, method = "none", plot = FALSE)
#     } else {
#       pcas <- lapply(object, plot2D, plot = FALSE)
#     }
#     profs <- lapply(object, exprs)
#     
# 
#     ## all feautres are displayed on start
#     feats <- 1:nrow(object[[1]])
#     
#     ## Build shiny app
#     ui <- fluidPage(
#         sidebarLayout(
#             sidebarPanel(
#                 selectizeInput("markers", "Labels",
#                                choices = colnames(pmarkers[[1]]),
#                                multiple = TRUE,
#                                selected = colnames(pmarkers[[1]])[pmsel]),
#                 sliderInput("trans", "Transparancy",
#                             min = 0,  max = 1, value = 0.5),
#                 checkboxInput("checkbox", label = "Show labels", value = TRUE),
#                 width = 2),
#             mainPanel(
#                 tabsetPanel(type = "tabs",
#                             tabPanel("PCA",
#                                      fluidRow(
#                                          column(4, 
#                                                 plotOutput("pca1",
#                                                            height = fig.height,
#                                                            width = fig.width,
#                                                            click = "pcaClick",
#                                                            dblclick = "pcaDblclick",
#                                                            hover = hoverOpts(
#                                                                id = "pcaHover"
#                                                                ),
#                                                            brush = brushOpts(
#                                                                id = "pcaBrush",
#                                                                resetOnNew = TRUE)),
#                                                 offset = 0),
#                                          column(4, 
#                                                 plotOutput("pca2",
#                                                            height = fig.height,
#                                                            width = fig.width,
#                                                            click = "pcaClick",
#                                                            dblclick = "pcaDblclick",
#                                                            hover = hoverOpts(
#                                                              id = "pcaHover"
#                                                            ),
#                                                            brush = brushOpts(
#                                                              id = "pcaBrush",
#                                                              resetOnNew = TRUE)),
#                                                 offset = 0),
#                                          column(4, 
#                                                 plotOutput("legend",
#                                                            height = fig.height,
#                                                            width = legend.width))
#                                          )
#                                      ),
#                             tabPanel("Profiles",
#                                      fluidRow(
#                                          column(4, offset = 0,
#                                                 plotOutput("profile1",
#                                                            height = fig.height,
#                                                            width = fig.width)),
#                                          column(4, offset = 0,
#                                                 plotOutput("profile2",
#                                                            height = fig.height,
#                                                            width = fig.width)))
#                                      
#                                      ),
#                             ## feature data table is always visible
#                             fluidRow(
#                                 column(12,
#                                        column(ncol(fData(object[[1]])), 
#                                               DT::dataTableOutput("fDataTable"))))
#                             )
#                 )
#             ))
#     
#     server <-
#         function(input, output, session) {
#             ranges <- reactiveValues(x = NULL, y = NULL)
#             ## Get coords for proteins according to selectized marker class(es)
#             ## Can I change hard coded 1 and 2 to lapply???
#             pcaMrkSel1 <- reactive({
#                 lapply(input$markers,
#                        function(z) pcas[[1]][which(pmarkers[[1]][, z] == 1), ])
#             })
#             pcaMrkSel2 <- reactive({
#               lapply(input$markers,
#                      function(z) pcas[[2]][which(pmarkers[[2]][, z] == 1), ])
#             })
#             profMrkSel1 <- reactive({
#                 lapply(input$markers,
#                        function(z) profs[[1]][which(pmarkers[[1]][, z] == 1), ])
#             })
#             profMrkSel2 <- reactive({
#               lapply(input$markers,
#                      function(z) profs[[2]][which(pmarkers[[2]][, z] == 1), ])
#             })
# 
#             ## Update colour transparacy according to slider input
#             myCols <- reactive({
#                 scales::alpha(cols,
#                               input$trans)[sapply(input$markers, function(z) 
#                                   which(colnames(pmarkers[[1]]) == z))]
#             })
#             ## PCA plot
#             output$pca1 <- renderPlot({
#                 par(mar = c(4, 4, 0, 0))
#                 par(oma = c(1, 0, 0, 0))
#                 plot(pcas[[1]],
#                      col = getUnknowncol(),
#                      pch = 21, cex = 1,
#                      xlim = ranges$x,
#                      ylim = ranges$y)
#                 usr <<- par("usr")
#                 for (i in 1:length(input$markers)) 
#                     points(pcaMrkSel1()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
#                 ## FIXME this does not work when brushed/subset of points selected
#                 s <- feats[input$fDataTable_rows_selected]
#                 if (length(s)) {
#                     points(pcas[[1]][s, , drop = FALSE],
#                            pch = 19, cex = 2, col = "#00000060")
#                     if (input$checkbox)
#                         text(pcas[[1]][s, 1], pcas[[1]][s, 2],
#                              rownames(pcas[[1]])[s],
#                              pos = 1)
#                 }
#             })
#             output$pca2 <- renderPlot({
#               par(mar = c(4, 4, 0, 0))
#               par(oma = c(1, 0, 0, 0))
#               plot(pcas[[2]],
#                    col = getUnknowncol(),
#                    pch = 21, cex = 1,
#                    xlim = ranges$x,
#                    ylim = ranges$y)
#               usr <<- par("usr")
#               for (i in 1:length(input$markers)) 
#                 points(pcaMrkSel2()[[i]], pch = 16, cex = 1.4, col = myCols()[i])
#               ## FIXME this does not work when brushed/subset of points selected
#               s <- feats[input$fDataTable_rows_selected]
#               if (length(s)) {
#                 points(pcas[[2]][s, , drop = FALSE],
#                        pch = 19, cex = 2, col = "#00000060")
#                 if (input$checkbox)
#                   text(pcas[[2]][s, 1], pcas[[2]][s, 2],
#                        rownames(pcas[[2]])[s],
#                        pos = 1)
#               }
#             })
#             ## Protein profile
#             output$profile1 <- renderPlot({
#                 par(mar = c(5.1, 4.1, 1, 1))
#                 ylim <- range(profs[[1]])
#                 n <- nrow(profs[[1]])
#                 m <- ncol(profs[[1]])
#                 fracs <- sampleNames(object[[1]])
#                 plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
#                      type = "n", xaxt = "n", xlab = "")
#                 axis(1, at = 1:m, labels = fracs, las = 2)
#                 title(xlab = "Fractions", line = 5.5)
#                 matlines(t(profs[[1]][feats, ]),
#                         col = getUnknowncol(),
#                         lty = 1,
#                         type = "l")
#                 ## for (i in 1:length(input$markers)) 
#                 ##     matlines(t(profMrkSel()[[i]]),
#                 ##              col = myCols()[i],
#                 ##              lty = 1,
#                 ##              lwd = 1.5)
#                 s <- feats[input$fDataTable_rows_selected]
#                 if (length(s))
#                     matlines(t(profs[[1]][s, , drop = FALSE]),
#                              col = "black",
#                              lty = 1,
#                              lwd = 2)
#             })
#             output$profile2 <- renderPlot({
#               par(mar = c(5.1, 4.1, 1, 1))
#               ylim <- range(profs[[2]])
#               n <- nrow(profs[[2]])
#               m <- ncol(profs[[2]])
#               fracs <- sampleNames(object[[2]])
#               plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
#                    type = "n", xaxt = "n", xlab = "")
#               axis(1, at = 1:m, labels = fracs, las = 2)
#               title(xlab = "Fractions", line = 5.5)
#               matlines(t(profs[[2]][feats, ]),
#                        col = getUnknowncol(),
#                        lty = 1,
#                        type = "l")
#               s <- feats[input$fDataTable_rows_selected]
#               if (length(s))
#                 matlines(t(profs[[2]][s, , drop = FALSE]),
#                          col = "black",
#                          lty = 1,
#                          lwd = 2)
#             })
#             
#             ## Feature data table
#             output$fDataTable <- DT::renderDataTable({
#                 if (is.null(input$pcaBrush)) {
#                     i <- try(pcas[[1]][, 1] >= usr[1] & pcas[[1]][, 1] <= usr[2])
#                     j <- try(pcas[[1]][, 2] >= usr[3] & pcas[[1]][, 2] <= usr[4])
#                 } else {
#                     i <- pcas[[1]][, 1] >= input$pcaBrush$xmin & pcas[[1]][, 1] <= input$pcaBrush$xmax
#                     j <- pcas[[1]][, 2] >= input$pcaBrush$ymin & pcas[[1]][, 2] <= input$pcaBrush$ymax
#                 }
#                 feats <<- which(i & j)
#                 DT::datatable(fData(object[[1]])[i & j, ],
#                               rownames = TRUE,
#                               options = list(searchHighlight = TRUE))
#                               ## filter = 'top')
#             })
#             
#             ## When a double-click happens, check if there's a brush on the plot.
#             ## If so, zoom to the brush bounds; if not, reset the zoom.
#             observeEvent(input$pcaDblclick, {
#                              brush <- input$pcaBrush
#                              if (!is.null(brush)) {
#                                  ranges$x <- c(brush$xmin, brush$xmax)
#                                  ranges$y <- c(brush$ymin, brush$ymax)
#                              } else {
#                                  ranges$x <- NULL
#                                  ranges$y <- NULL
#                              }                                         
#                          })
#             ## Output legend
#             output$legend <- renderPlot({
#                 par(mar = c(0, 0, 0, 0))
#                 par(oma = c(0, 0, 0, 0))
#                 plot(0, type = "n",
#                      xaxt = "n", yaxt = "n",
#                      xlab = "", ylab = "",
#                      bty = "n")
#                 legend("topleft",
#                        input$markers,
#                        col = myCols(),
#                        ncol = 1, bty = "n",
#                        pch = 16,
#                        cex = legend.cex)
#             })
#         }
#     app <- list(ui = ui, server = server)
#     runApp(app)
# }
# 
# 
# ## feats
# ##  features to display on PCA plot
# ##  profiles to diplay on matplot
# ##  features to show in DT::datatable
# 
# ## feats[input$fDataTable_rows_selected]
# ##  features to highlight
# ##  feature selected in DT::datatable
