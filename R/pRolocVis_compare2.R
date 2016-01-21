pRolocVis_compare2 <- function(object, fcol1, fcol2,
                               #foi,
                               fig.height = "600px",
                               fig.width = "100%",
                               legend.width = "200%",
                               legend.cex = 1,
                               remap = TRUE,
                               nchar = 40,
                               all = TRUE,
                               # method,
                               fDataInd1, fDataInd2, 
                               ...) {
  
  
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
  
  
  ## Check MSnSetList and take intersection
  if (!inherits(object, "MSnSetList"))
    stop("The input must be of class MSnSetList")
  message("Subsetting MSnSetList to their common feature names")
  object <- commonFeatureNames(object)
  
  
  ## fcol checks
  if (missing(fcol1) | missing(fcol2)) {
    if (missing(fcol1) & missing(fcol2)) {
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
  fcol <- c(fcol1, fcol2)
  tf <- !sapply(1:length(fcol), function(x) isMrkMat(object[[x]], fcol[x]))
  pmarkers <- vector("list", length = length(object))
  for (i in 1:length(tf)) {
    if (tf[i]) {
      ## Make a mrk vec mat, then extract mat
      mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
      tmpObj <- mrkVecToMat(object[[i]], fcol[i], mfcol = mName)
      pmarkers[[i]] <- fData(tmpObj)[, mName]
    } else {
      pmarkers[[i]] <- fData(object[[i]])[, fcol[i]]
    }
  }
  
  ## Setting features to be displayed
#   if (!missing(foi)) {
#     if (inherits(foi, "FeaturesOfInterest"))
#       foi <- FoICollection(list(foi))
#     foimarkers <- as(foi, "matrix")
#     if (exists("pmarkers", inherits = FALSE)) {
#       pmarkers <- lapply(pmarkers, function(z) 
#         merge(z, foimarkers, by = 0, all.x = TRUE))
#       for (i in 1:length(pmarkers)) {
#         rownames(pmarkers[[i]]) <- pmarkers[[i]][, "Row.names"]
#         pmarkers[[i]] <- pmarkers[[i]][featureNames(object[[i]]), -1]     
#       } 
#     } else pmarkers <- foimarkers
#   }
#   
  
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
  myclasses <- unique(unlist(sapply(pmarkers, colnames)))
  names(cols) <- myclasses
  
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
  if (!all & max(sapply(pmarkers, ncol)) > 15)
    pmsel <- 1    
  
  
  ## Data to be displayed (allow other methods than PCA, matrix not supported as
  ## would need a list of matrices, we can add later if needed)
#   if (missing(method)) {
#     pcas <- lapply(object, plot2D, fcol = NULL, plot = FALSE)
#   } else {
#     if (pRoloc:::.validpRolocVisMethod(method)) {
#       if (class(method) == "matrix") 
#         stop("Visualisation method 'matrix' is not supported for the compare app")
#       else
#         pcas <- lapply(object, plot2D, fcol = NULL, plot = FALSE, method = method)
#     } else {
#       stop("Invalid visualisation method")
#     }
#   }
  
  ## Get data for profiles (need to do this here before changing MSnSet with remap
  ## as exprs data gets lost with remap)
  profs <- lapply(object, exprs)
  
  
  ## Remap data to same PC space
  
  if (remap) {
    message("Remapping data to the same PC space")
    object <- pRoloc:::remap(object)
    pcas <- lapply(object, function(z) exprs(z)[, dims])
    plotmeth <- "none"
  } else {
    pcas <- lapply(object, plot2D, fcol = NULL, plot = FALSE)
    plotmeth <- "PCA"
  }

  ## Create column of unknowns (needed later for plot2D in server)
  newName <- paste0(format(Sys.time(), "%a%b%d%H%M%S%Y"), "unknowns")
  object <- lapply(object, function(x) {fData(x)[, newName] = "unknown"; x})

  
  ## all features are displayed on start
  feats <- toSel <- 1:nrow(object[[1]])
  names(feats) <- featureNames(object[[1]])
  idxDT <- numeric()
  namesIdxDT <- character()
  
  ## Build shiny app
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectizeInput("markers", "Labels",
                       choices = myclasses,
                       multiple = TRUE,
                       selected = myclasses[pmsel]),
        sliderInput("trans", "Transparancy",
                    min = 0,  max = 1, value = 0.5),
        checkboxInput("checkbox", label = "Show labels", value = TRUE),
        br(),
        # actionButton("resetButton", "Zoom/reset plot"),
        # br(),
        actionButton("clear", "Clear selection"),
        br(),
        # radioButtons("switchDT", "Datatable view:",
        #                        c("Object 1" = "dt1",
        #                          "Object 2" = "dt2")),
        width = 2),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("PCA", id = "pcaPanel",
                             fluidRow(
                               column(4, 
                                      plotOutput("pca1",
                                                 height = fig.height,
                                                 width = fig.width,
                                                 dblclick = "dblClick1"),
                                      # brush = brushOpts(
                                      # id = "pcaBrush",
                                      # resetOnNew = TRUE)),
                                      offset = 0),
                               column(4, 
                                      plotOutput("pca2",
                                                 height = fig.height,
                                                 width = fig.width,
                                                 dblclick = "dblClick2"),
                                      # brush = brushOpts(
                                      # id = "pcaBrush",
                                      # resetOnNew = TRUE)),
                                      offset = 0),
                               column(3, 
                                      plotOutput("legend1",
                                                 height = fig.height,
                                                 width = legend.width))
                             )
                    ),
                    tabPanel("Profiles", id = "profilesPanel",
                             fluidRow(
                               column(4,
                                      plotOutput("profile1",
                                                 height = "400px",
                                                 width = "120%"),
                                      offset = 0),
                               column(4,
                                      plotOutput("profile2",
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
                             column(length(fDataInd1), 
                                    DT::dataTableOutput("fDataTable"))))
        ))
      )
    )
  
  
  
  
  server <-
    function(input, output, session) {
      ranges1 <- reactiveValues(x = NULL, y = NULL)
      ranges2 <- reactiveValues(x = NULL, y = NULL)
      
      ## Get brush bounds for zoom
#       brushBounds1 <- reactiveValues(i =  try(pcas[[1]][, 1] >= min(pcas[[1]][, 1]) & 
#                                                pcas[[1]][, 1] <= max(pcas[[1]][, 1])),
#                                     j = try(pcas[[1]][, 2] >= min(pcas[[1]][, 2]) & 
#                                               pcas[[1]][, 2] <= max(pcas[[1]][, 2])))
#       brushBounds2 <- reactiveValues(i =  try(pcas[[2]][, 1] >= min(pcas[[2]][, 1]) & 
#                                                 pcas[[2]][, 1] <= max(pcas[[2]][, 1])),
#                                      j = try(pcas[[2]][, 2] >= min(pcas[[2]][, 2]) & 
#                                                pcas[[2]][, 2] <= max(pcas[[2]][, 2])))
      resetLabels <- reactiveValues(logical = FALSE)
      
      
      
      ## Get coords for proteins according to selectized marker class(es)
      ## NB: need two reactive objects here as markers in object[[1]] do not
      ## necessarily have the same indices as markers in object[[2]] (would like
      ## to allow different markers between different datasets)
      mrkSel1 <- reactive({
        ind <- match(input$markers, colnames(pmarkers[[1]]))
        lapply(ind,
               function(z) which(pmarkers[[1]][, z] == 1))
      })
      mrkSel2 <- reactive({
        ind <- match(input$markers, colnames(pmarkers[[2]]))
        lapply(ind,
               function(z) which(pmarkers[[2]][, z] == 1))
      })
      
      
      
      ## Update colour transparacy according to slider input
      ## Note: will this break if markers *classes* are not common 
      ## between datasets
      myCols <- reactive({
        scales::alpha(cols,
                      input$trans)[sapply(input$markers, function(z) 
                        which(names(cols) == z))]
      })
      
      
      
      ## PCA plot 1
      output$pca1 <- renderPlot({
        par(mar = c(4, 4, 0, 0))
        par(oma = c(1, 0, 0, 0))

        plot2D(object[[1]], method = plotmeth,
               col = rep(getUnknowncol(), nrow(object[[1]])),
               pch = 21, cex = 1,
               xlim = ranges1$x,
               ylim = ranges1$y,
               fcol = newName)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) {
            if (length(mrkSel1()[[i]]) > 0)
              points(pcas[[1]][mrkSel1()[[i]], ], pch = 16, 
                     cex = 1.4, col = myCols()[i])
          }
        } 
        ## highlight point on plot by selecting item in table
        idxDT <<- feats[input$fDataTable_rows_selected]
        
        if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
        
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          highlightOnPlot(pcas[[1]], namesIdxDT, cex = 1.3)
          if (input$checkbox) 
            highlightOnPlot(pcas[[1]], namesIdxDT, labels = TRUE, pos = 3)
        }
        resetLabels$logical <- FALSE
      })
      
      
      
      ## PCA plot 2
      output$pca2 <- renderPlot({
        par(mar = c(4, 4, 0, 0))
        par(oma = c(1, 0, 0, 0))
        
        plot2D(object[[2]], method = plotmeth,
               col = rep(getUnknowncol(), nrow(object[[2]])),
               pch = 21, cex = 1,
               xlim = ranges2$x,
               ylim = ranges2$y,
               fcol = newName)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) {
            if (length(mrkSel2()[[i]]) > 0)
              points(pcas[[2]][mrkSel2()[[i]], ], pch = 16, 
                     cex = 1.4, col = myCols()[i])
          }
        } 
        ## highlight point on plot by selecting item in table
        idxDT <<- feats[input$fDataTable_rows_selected]
        
        if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
        
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          highlightOnPlot(pcas[[2]], namesIdxDT, cex = 1.3)
          if (input$checkbox) 
            highlightOnPlot(pcas[[2]], namesIdxDT, labels = TRUE, pos = 3)
        }
        resetLabels$logical <- FALSE
      })
      
      
      
      ## Protein profile
      output$profile1 <- renderPlot({
        par(mar = c(8, 3, 1, 1))
        par(oma = c(0, 0, 0, 0))
        ylim <- range(profs[[1]])
        n <- nrow(profs[[1]])
        m <- ncol(profs[[1]])
        fracs <- sampleNames(object[[1]])
        plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
             type = "n", xaxt = "n", xlab = "")
        axis(1, at = 1:m, labels = fracs, las = 2)
        title(xlab = "Fractions", line = 5.5)
        matlines(t(profs[[1]][feats, ]),
                 col = getUnknowncol(),
                 lty = 1,
                 type = "l")
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) { 
            if (length(mrkSel1()[[i]]) > 0)
              matlines(t(profs[[1]][mrkSel1()[[i]], ]),
                       col = myCols()[i],
                       lty = 1,
                       lwd = 1.5) 
          }
        }
        ## If an item is clicked in the table highlight profile
        idxDT <<- feats[input$fDataTable_rows_selected]
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          matlines(t(profs[[1]][namesIdxDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 2)
        }
      }) 
        
        
        ## Protein profile
        output$profile2 <- renderPlot({
          par(mar = c(8, 3, 1, 1))
          par(oma = c(0, 0, 0, 0))
          ylim <- range(profs[[2]])
          n <- nrow(profs[[2]])
          m <- ncol(profs[[2]])
          fracs <- sampleNames(object[[2]])
          plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity", 
               type = "n", xaxt = "n", xlab = "")
          axis(1, at = 1:m, labels = fracs, las = 2)
          title(xlab = "Fractions", line = 5.5)
          matlines(t(profs[[2]][feats, ]),
                   col = getUnknowncol(),
                   lty = 1,
                   type = "l")
          if (!is.null(input$markers)) {
            for (i in 1:length(input$markers)) { 
              if (length(mrkSel2()[[i]]) > 0)
                matlines(t(profs[[2]][mrkSel2()[[i]], ]),
                         col = myCols()[i],
                         lty = 1,
                         lwd = 1.5)
            }
          }
        ## If an item is clicked in the table highlight profile
        idxDT <<- feats[input$fDataTable_rows_selected]
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          matlines(t(profs[[2]][namesIdxDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 2)
        }
      })             
      
      
        
      ## Feature data table
      output$fDataTable <- DT::renderDataTable({
        
#         feats <<- which(brushBounds$i & brushBounds$j)
        ## Double clicking to identify protein
        if (!is.null(input$dblClick1)) {
          dist <- apply(pcas[[1]], 1, function(z) sqrt((input$dblClick1$x - z[1])^2 
                                                  + (input$dblClick1$y - z[2])^2))
          idxPlot <- which(dist == min(dist))
          if (idxPlot %in% idxDT) {                          ## 1--is it already clicked?
            setsel <- setdiff(names(idxDT), names(idxPlot))  ## Yes, remove it from table
            idxDT <<- idxDT[setsel]
          } else {                                           ## 2--new click?
            idxDT <<- c(idxDT, idxPlot)                      ## Yes, highlight it to table
          }
        }
        if (!is.null(input$dblClick2)) {
          dist <- apply(pcas[[2]], 1, function(z) sqrt((input$dblClick2$x - z[1])^2 
                                                       + (input$dblClick2$y - z[2])^2))
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
                         featureNames(object[[1]]))
#                        featureNames(object)[brushBounds$i & brushBounds$j])
        if (resetLabels$logical) toSel <- numeric()         ## reset labels
        
        ## Display data table (with clicked proteins highlighted)
#         DT::datatable(data = fData(object)[brushBounds$i & brushBounds$j, fDataInd], 
        DT::datatable(data = fData(object[[1]])[, fDataInd1], 
                      rownames = TRUE,
                      selection = list(mode = 'multiple', selected = toSel))
      })
      
      
#       ## When a the reset button is clicked check to see is there is a brush on
#       ## the plot, if yes zoom, if not reset the plot.
#       observeEvent(input$resetButton, {
#         brush <- input$pcaBrush
#         if (!is.null(brush)) {
#           ranges$x <- c(brush$xmin, brush$xmax)
#           ranges$y <- c(brush$ymin, brush$ymax)
#           brushBounds$i <- pcas[[1]][, 1] >= brush$xmin & pcas[[1]][, 1] <= brush$xmax
#           brushBounds$j <- pcas[[1]][, 2] >= brush$ymin & pcas[[1]][, 2] <= brush$ymax
#         } else {
#           ranges$x <- NULL
#           ranges$y <- NULL
#           brushBounds$i <- try(pcas[[1]][, 1] >= min(pcas[[1]][, 1]) 
#                                & pcas[[1]][, 1] <= max(pcas[[1]][, 1]))
#           brushBounds$j <- try(pcas[[1]][, 2] >= min(pcas[[1]][, 2]) 
#                                & pcas[[1]][, 2] <= max(pcas[[1]][, 2]))
#         }
#       })
      
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

  