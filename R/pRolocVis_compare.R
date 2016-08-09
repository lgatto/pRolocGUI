##' @param fcol1 If using the \code{compare} app this is the feature meta-data 
##' label (fData column name) for the first dataset in the \code{MSnSetList}. 
##' Default is \code{markers}.
##' @param fcol2 If using the \code{compare} app this is the feature meta-data 
##' label (fData column name) for the second dataset in the \code{MSnSetList}. 
##' Default is \code{markers}. 
##' @param remap A \code{logical} indicating whether the second dataset in the
##' \code{MSnSetList} should be remapped to the first dataset.names(cols) <- myclasses Default is 
##' @return For \code{compare} and \code{main} a \code{character} vector of the 
##' \code{featureNames} of the proteins selected is invisibly returned.
##' @rdname pRolocVis-apps
pRolocVis_compare <- function(object, fcol1, fcol2,
                              foi,
                              fig.height = "600px",
                              fig.width = "100%",
                              legend.width = "200%",
                              legend.cex = 1,
                              remap = TRUE,
                              nchar = 40,
                              all = TRUE,
                              ...) {
                               
  ## Return featureNames of proteins selected
  on.exit(return(invisible(idDT)))
  
  
  ## Check MSnSetList and take intersection
  if (!inherits(object, "MSnSetList"))
    stop("The input must be of class MSnSetList")
  message("Subsetting MSnSetList to their common feature names")
  object <- commonFeatureNames(object)
  
  
  ## Check if method specified
  dotargs <- pairlist(...)
  if (any(names(dotargs) == "method"))
    stop("Argument 'method' is already defined internally and can not be used with the compare application")
  
  if (any(names(dotargs) == "fcol"))
    stop("Please specify fcol1 and fcol2 for each MSnSet respectively, see ?pRolocVis for more details")
  
  
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

    for (i in seq_along(object))
        object@x[[i]] <- .makeMatsVecs(object@x[[i]])

  ## Define data columns
  origFvarLab1 <- fvarLabels(object[[1]])
  origFvarLab2 <- fvarLabels(object[[2]])
  if (length(origFvarLab1) > 4) {
    .ind <- which(origFvarLab1 == fcol1)
    .fvarL <- origFvarLab1[-.ind]
    selDT1 <- c(.fvarL[1:3], fcol1)
  } else {
    selDT1 <- origFvarLab1[1:length(origFvarLab1)]
  }
  if (length(origFvarLab2) > 4) {
    .ind <- which(origFvarLab2 == fcol2)
    .fvarL <- origFvarLab2[-.ind]
    selDT2 <- c(.fvarL[1:3], fcol2)
  } else {
    selDT2 <- origFvarLab2[1:length(origFvarLab2)]
  }  
  
  
  ## Make fcol matrix of markers if it's not already
  fcol <- c(fcol1, fcol2)
  tf <- sapply(1:length(fcol), function(x) isMrkVec(object[[x]], fcol[x]))
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
  if (!missing(foi)) {
    if (inherits(foi, "FeaturesOfInterest") | inherits(foi, "FoICollection")) {
      if (inherits(foi, "FeaturesOfInterest"))
        foi <- FoICollection(list(foi))
      foimarkers <- as(foi, "matrix")
      if (exists("pmarkers", inherits = FALSE)) {
        pmarkers <- lapply(pmarkers, function(z) merge(z, foimarkers,
                                                       by = 0, all.x = TRUE))
        rownames(pmarkers[[1]]) <- pmarkers[[1]][, "Row.names"]
        rownames(pmarkers[[2]]) <- pmarkers[[1]][, "Row.names"]
        pmarkers[[1]] <- pmarkers[[1]][featureNames(object[[1]]), -1]
        pmarkers[[2]] <- pmarkers[[2]][featureNames(object[[2]]), -1]            
      } else pmarkers <- list(foimarkers, foimarkers)
    } else {
      message("foi is not a valid FeaturesOfInterest or FoICollection object")
    }
  }     ## NB: pmarkers[[1]] and pmarkers[[2]] contains the same num of rows/proteins
  sumpm <- lapply(pmarkers, function(z) apply(z, 2, sum, na.rm = TRUE))
  if (any(sumpm[[1]] == 0)) {
    message(paste("foi object", names(which(sumpm[[1]] == 0)), "does not match any featuresNames that are common in both datasets, removing foi"))
    pmarkers[[1]] <- pmarkers[[1]][, -which(sumpm[[1]] == 0)]
    pmarkers[[2]] <- pmarkers[[2]][, -which(sumpm[[2]] == 0)]
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
    n <- ncol(pmarkers[[ind]]) / length(cols)
    cols <- rep(cols, n + 1)
  }
  myclasses <- unique(unlist(lapply(pmarkers, colnames)))
  cols <- cols[1:length(myclasses)]
  names(cols) <- myclasses

  ## Shorten markers names if too long
  for (i in 1:length(object)) {
    cn <- sapply(colnames(pmarkers[[i]]),
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
    colnames(pmarkers[[i]]) <- cn
  }
  
  
  ## Display all classes unless user specifies not to
  pmsel <- TRUE
  if (!all | max(sapply(pmarkers, ncol)) > 15)
    pmsel <- 1    
  
  
  ## Get data for profiles (need to do this here before changing MSnSet with remap
  ## as exprs data gets lost with remap)
  profs <- lapply(object@x, exprs)
  
  ## Remap data to same PC space
  if (remap) {
    message("Remapping data to the same PC space")
    object <- pRoloc:::remap(object)
    plotmeth <- "none"
  } else {
    plotmeth <- "PCA"
  }
  pcas <- lapply(object@x, plot2D, fcol = NULL, 
                 plot = FALSE, method = "none", ...)
  
  
  ## Create column of unknowns (needed later for plot2D in server)
  newName <- paste0(format(Sys.time(), "%a%b%d%H%M%S%Y"), "unknowns")
  object <- lapply(object@x, function(x) {fData(x)[, newName] = "unknown"; x})
  
  
  ## all features are displayed on start
  toSel <- 1:nrow(object[[1]])
  feats <- featureNames(object[[1]])
  idDT <- character()

  
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
        actionButton("resetButton", "Zoom/reset plot"),
        br(),
        actionButton("clear", "Clear selection"),
        br(),
        width = 2),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("PCA", id = "pcaPanel",
                             fluidRow(
                               column(5, 
                                      plotOutput("pca1",
                                                 height = fig.height,
                                                 width = fig.width,
                                                 dblclick = "dblClick1",
                                                 brush = brushOpts(
                                                   id = "pcaBrush1",
                                                   resetOnNew = TRUE)),
                                      offset = 0),
                               column(5, 
                                      plotOutput("pca2",
                                                 height = fig.height,
                                                 width = fig.width,
                                                 dblclick = "dblClick2",
                                                 brush = brushOpts(
                                                   id = "pcaBrush2",
                                                   resetOnNew = TRUE)),
                                      offset = 0),
                               column(2, 
                                      plotOutput("legend1",
                                                 height = fig.height,
                                                 width = legend.width))
                             )
                    ),
                    tabPanel("Profiles", id = "profilesPanel",
                             fluidRow(
                               column(5,
                                      plotOutput("profile1",
                                                 height = "400px",
                                                 width = "110%"),
                                      offset = 0),
                               column(5,
                                      plotOutput("profile2",
                                                 height = "400px",
                                                 width = "110%"),
                                      offset = 0),
                               
                               column(2, 
                                      plotOutput("legend2",
                                                 width = "100%"),
                                      offset = 0)
                             )
                    ),
                    tabPanel("Table Selection", id = "tableSelPanel",
                             fluidRow(
                               column(5,
                                      checkboxGroupInput("selTab1", 
                                                         "Columns to display for data 1",
                                                         choices = origFvarLab1,
                                                         selected = selDT1)
                                      ),
                               column(5,
                                      checkboxGroupInput("selTab2",
                                                         "Columns to display for data 2",
                                                         choices = origFvarLab2,
                                                         selected = selDT2)
                                      
                                      )
                             ),
                             tags$head(tags$style("#selTab1{color: darkblue;}"))
                    ),
                    ## feature data table is always visible
                    fluidRow(
                      column(12,
                             column(length(c(selDT1, selDT2)),
                                    DT::dataTableOutput("fDataTable"))))
        ))
    )
  )
  
  
  
  
  server <-
    function(input, output, session) {
      ranges <- reactiveValues(x = c(min(c(pcas[[1]][, 1], pcas[[2]][, 1])), 
                                     max(c(pcas[[1]][, 1], pcas[[2]][, 1]))),
                               y = c(min(c(pcas[[1]][, 2], pcas[[2]][, 2])), 
                                     max(c(pcas[[1]][, 2], pcas[[2]][, 2]))))
      
      
      ## Capture brushed proteins for zoom
      brushedProts1 <- reactiveValues(i =  try(pcas[[1]][, 1] >= min(pcas[[1]][, 1]) & 
                                                 pcas[[1]][, 1] <= max(pcas[[1]][, 1])),
                                      j = try(pcas[[1]][, 2] >= min(pcas[[1]][, 2]) & 
                                                pcas[[1]][, 2] <= max(pcas[[1]][, 2])))
      brushedProts2 <- reactiveValues(i =  try(pcas[[2]][, 1] >= min(pcas[[2]][, 1]) & 
                                                 pcas[[2]][, 1] <= max(pcas[[2]][, 1])),
                                      j = try(pcas[[2]][, 2] >= min(pcas[[2]][, 2]) & 
                                                pcas[[2]][, 2] <= max(pcas[[2]][, 2])))
      
      resetLabels <- reactiveValues(logical = FALSE)
    
      
      
      ## Get coords for proteins according to selectized marker class(es)
      ## NB: need two reactive objects here as markers in object[[1]] do not
      ## necessarily have the same indices as markers in object[[2]] (would like
      ## to allow different markers between different datasets)
      mrkSel1 <- reactive({
        # browser()
        ind <- match(input$markers, colnames(pmarkers[[1]]))
        .mrkSel1 <- vector("list", length(input$markers))
        for (i in seq(length(input$markers))) {
          if (is.na(ind[i])) {
            .mrkSel1[[i]] <- NA
          } else {
            .mrkSel1[[i]] <- which(pmarkers[[1]][, ind[i]] == 1)
          }
        }
        .mrkSel1
      })

        
      mrkSel2 <- reactive({
        ind <- match(input$markers, colnames(pmarkers[[2]]))
        .mrkSel2 <- vector("list", length(input$markers))
        for (i in seq(length(input$markers))) {
          if (is.na(ind[i])) {
            .mrkSel2[[i]] <- NA
          } else {
            .mrkSel2[[i]] <- which(pmarkers[[2]][, ind[i]] == 1)
          }
        }
        .mrkSel2
      })
      
      
      ## Update colour transparacy according to slider input
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
               xlim = ranges$x,
               ylim = ranges$y,
               fcol = newName, 
               ...)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) {
            if (!is.na(mrkSel1()[[i]][1]))
              points(pcas[[1]][mrkSel1()[[i]], ], pch = 16, 
                     cex = 1.4, col = myCols()[i])
          }
        } 
        ## highlight point on plot by selecting item in table
        idDT <<- feats[input$fDataTable_rows_selected]
        if (resetLabels$logical) idDT <<- character()  ## If TRUE clear labels
        if (length(idDT)) {
          highlightOnPlot(pcas[[1]], idDT, cex = 1.3)
          if (input$checkbox) 
            highlightOnPlot(pcas[[1]], idDT, labels = TRUE, pos = 3)
        }
      })
      
      
      
      ## PCA plot 2
      output$pca2 <- renderPlot({
        par(mar = c(4, 4, 0, 0))
        par(oma = c(1, 0, 0, 0))
        plot2D(object[[2]], method = plotmeth,
               col = rep(getUnknowncol(), nrow(object[[2]])),
               pch = 21, cex = 1,
               xlim = ranges$x,
               ylim = ranges$y,
               fcol = newName,
               ...)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) {
            if (!is.na(mrkSel2()[[i]][1]))
              points(pcas[[2]][mrkSel2()[[i]], ], pch = 16, 
                     cex = 1.4, col = myCols()[i])
          }
        } 
        ## highlight point on plot by selecting item in table
        idDT <<- feats[input$fDataTable_rows_selected]
        if (resetLabels$logical) idDT <<- character()  ## If TRUE labels are cleared
        if (length(idDT)) {
          highlightOnPlot(pcas[[2]], idDT, cex = 1.3)
          if (input$checkbox) 
            highlightOnPlot(pcas[[2]], idDT, labels = TRUE, pos = 3)
        }
        resetLabels$logical <<- FALSE
      })
      
      
      
      ## Protein profile plot 1
      output$profile1 <- renderPlot({
        par(mar = c(8, 2, 1, 1))
        par(oma = c(1, 0, 0, 1))
        ylim <- range(profs[[1]])
        n <- nrow(profs[[1]])
        m <- ncol(profs[[1]])
        fracs <- colnames(profs[[1]])
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
            if (!is.na(mrkSel1()[[i]][1]))
              matlines(t(profs[[1]][mrkSel1()[[i]], ]),
                       col = myCols()[i],
                       lty = 1,
                       lwd = 1.5) 
          }
        }
        ## If an item is clicked in the table highlight profile
        idDT <<- feats[input$fDataTable_rows_selected]
        if (length(idDT)) {
          matlines(t(profs[[1]][idDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 2)
        }
      })
      
      
      ## Protein profile plot 2
      output$profile2 <- renderPlot({
        par(mar = c(8, 3, 1, 1))
        par(oma = c(1, 0, 0, 0))
        ylim <- range(profs[[2]])
        n <- nrow(profs[[2]])
        m <- ncol(profs[[2]])
        fracs <- colnames(profs[[2]])
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
            if (!is.na(mrkSel2()[[i]][1]))
              matlines(t(profs[[2]][mrkSel2()[[i]], ]),
                       col = myCols()[i],
                       lty = 1,
                       lwd = 1.5)
          }
        }
        ## If an item is clicked in the table highlight profile
        idDT <<- feats[input$fDataTable_rows_selected]
        if (length(idDT)) {
          matlines(t(profs[[2]][idDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 2)
        }
      })             
      
      
      
      ## Feature data table
      output$fDataTable <- DT::renderDataTable({
        feats <<- unique(c(names(which(brushedProts1$i & brushedProts1$j)),
                           names(which(brushedProts2$i & brushedProts2$j))))
        ## Double clicking to identify protein
        if (!is.null(input$dblClick1)) {
          dist <- apply(pcas[[1]], 1, function(z) sqrt((input$dblClick1$x - z[1])^2 
                                                       + (input$dblClick1$y - z[2])^2))
          idPlot <- names(which(dist == min(dist)))
          if (idPlot %in% idDT) {                          ## 1--is it already clicked?
            idDT <<- setdiff(idDT, idPlot)                 ## Yes, remove it from table
          } else {                                         ## 2--new click?
            idDT <<- c(idDT, idPlot)                       ## Yes, highlight it to table
          }
        }
        if (!is.null(input$dblClick2)) {
          dist <- apply(pcas[[2]], 1, function(z) sqrt((input$dblClick2$x - z[1])^2 
                                                       + (input$dblClick2$y - z[2])^2))
          idPlot <- names(which(dist == min(dist)))
          if (idPlot %in% idDT) {                          ## 1--is it already clicked?
            idDT <<- setdiff(idDT, idPlot)                 ## Yes, remove it from table
          } else {                                         ## 2--new click?
            idDT <<- c(idDT, idPlot)                       ## Yes, highlight it to table
          }
        } 
        toSel <- match(idDT, feats)                        ## selection to highlight in DT
        if (resetLabels$logical) toSel <- numeric()        ## reset labels
        .dt1 <- fData(object[[1]])[feats, input$selTab1, drop = FALSE]
        .dt2 <- fData(object[[2]])[feats, input$selTab2, drop = FALSE]
        colnames(.dt1) <- paste0('<span style="color:',   
                                 rep("darkblue", ncol(.dt1)), '">', 
                                 colnames(.dt1), '</span>')
        dataDT <- cbind(.dt1, .dt2)
        DT::datatable(data = dataDT, 
                      rownames = TRUE,
                      selection = list(mode = 'multiple', selected = toSel),
                      escape = FALSE) %>%     ## NB: `escape = FALSE` required for colname coloring
          formatStyle(columns = colnames(.dt1), color = c("darkblue")) 
      })
      
      
      ## When a the reset button is clicked check to see is there is a brush on
      ## the plot, if yes zoom, if not reset the plot.
      observeEvent(input$resetButton, {
        .brush1 <- input$pcaBrush1
        .brush2 <- input$pcaBrush2
        brush <- list(.brush1, .brush2)
        tf <- !sapply(brush, is.null)
        if (any(tf)) { 
          tf <- which(tf)
          brush <- brush[[tf]] 
          bminx <- brush$xmin
          bmaxx <- brush$xmax
          bminy <- brush$ymin
          bmaxy <- brush$ymax
        } else {   ## reset the plot
          bminx <- min(c(pcas[[1]][, 1], pcas[[2]][, 1]))
          bmaxx <- max(c(pcas[[1]][, 1], pcas[[2]][, 1]))
          bminy <- min(c(pcas[[1]][, 2], pcas[[2]][, 2]))
          bmaxy <- max(c(pcas[[1]][, 2], pcas[[2]][, 2]))
        }
        ranges$x <- c(bminx, bmaxx)
        ranges$y <- c(bminy, bmaxy)
        brushedProts1$i <- try(pcas[[1]][, 1] >= bminx 
                               & pcas[[1]][, 1] <= bmaxx)
        brushedProts1$j <- try(pcas[[1]][, 2] >= bminy 
                               & pcas[[1]][, 2] <= bmaxy)
        brushedProts2$i <- try(pcas[[2]][, 1] >= bminx 
                               & pcas[[2]][, 1] <= bmaxx)
        brushedProts2$j <- try(pcas[[2]][, 2] >= bminy 
                               & pcas[[2]][, 2] <= bmaxy)
      })
      
      
      ## When clear selection is pressed labels and reset selection 
      observeEvent(input$clear, {
        resetLabels$logical <<- TRUE
      })
      
      
      ## Output legend for pca
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
      
      ## Output legend for profiles
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
