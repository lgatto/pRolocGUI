## DOUBLE CLICKING TO HIGHLIGHT on/off on PCA plot, or selection via table

## Shiny: spinning loading wheel on top of plot while plot is recalculating
## https://gist.github.com/daattali/edd7c20cd09f484b7f32

## Very slow with bigger data (fusion data), server side table flaky
## and warnings about size

## References
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://gallery.shinyapps.io/095-plot-interaction-advanced/
## https://gallery.shinyapps.io/105-plot-interaction-zoom/
## https://gallery.shinyapps.io/106-plot-interaction-exclude/
## https://github.com/rstudio/shiny-examples
## http://shiny.rstudio.com/articles/selecting-rows-of-data.html
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html

##' @rdname pRolocVis-apps
##' @param foi A \code{\link{FeaturesOfInterest}} or
##'     \code{\link{FoICollection}} object.
##' @param fig.height Height of the figure. Default is \code{"600px"}.
##' @param fig.width Width of the figure. Default is \code{"100px"}.
##' @param nchar Maximum number of characters of the markers class
##'     names, before their names are truncated. Default is 10.
##' @param all If \code{TRUE} all clusters are displayed on startup,
##'     if the total number of clusters is less than including 15. If
##'     \code{FALSE} or otherwise, only the first cluster in the list
##'     is displayed.
##' @param method A \code{character} describe how to transform the
##'     data or what to plot. One of \code{"PCA"} (default) or 
##'     \code{"t-SNE"}. Other transformations can be used but the
##'     data first needs to be transformed and passed as a matrix
##'     see \code{methargs} and example herein  
##' @param methargs A \code{list} of arguments to be passed when
##'     \code{method} is called. If missing, the data will be scaled
##'     and centred prior to PCA and t-SNE (i.e. \code{Rtsne}'s
##'     arguments \code{pca_center} and \code{pca_scale} are set to
##'     \code{TRUE}). If \code{method = "none"} and \code{object} is a
##'     \code{matrix}, then the first and only argument of
##'     \code{methargs} must be an \code{MSnSet} with matching
##'     features with \code{object}.
##' @return For the \code{pca}, \code{compare} and \code{aggregate} apps 
##'     a \code{character} vector of \code{featureNames} names of the
##'     \code{object} loaded that have been selected in the app
##'     upon application closure.
pRolocVis_pca <- function(object,
                          fcol = "markers",
                          foi,
                          fig.height = "700px",
                          fig.width = "700px",
                          nchar = 40,
                          all = TRUE,
                          method = c("PCA", "t-SNE", "none"),
                          methargs) {
  ## Return featureNames of proteins selected
  idDT <- character()
  on.exit({
    setUnknowncol(NULL)
    return(invisible(idDT))
  })
  
  ## Check MSnSet or matrix
  if (inherits(object, "MSnSet")) {
    if (missing(method)) {method = "PCA"}
    if (method == "PCA") {
      object_coords <- plot2D(object, method = "PCA", plot = FALSE)
    } 
    else if (method == "t-SNE") {
      object_coords <- plot2D(object, method = "t-SNE", plot = FALSE)
    } 
    else if (method == "none") {
      object_coords <- exprs(object)[, c(1, 2)]
    }
    if (!(any(method == c("PCA", "t-SNE", "none")))) 
      stop(paste("Currently the only supported internal methods are PCA and t-SNE.",
           "To use another visualisation please pass as a 2D matrix and set",
           "method == \"none\" as per documentation for plot2D in pRoloc"))
  } 
  else if (is.matrix(object)) {
    object_coords <- object[, 1:2]
    message(paste("Taking first two columns in the data matrix for the 2D plot"))
    if (missing(methargs)) {
      stop(paste("When passing a matrix as the plotting object you must include the",
           "corresponding MSnSet in methargs e.g. methargs = list(your_MSnSet)"))
    }
    object <- methargs[[1]]
    if (!inherits(object, "MSnSet")) 
      stop(paste("When passing a matrix as the plotting object methargs must be",
                 "the corresponding MSnSet e.g. methargs = list(your_MSnSet)"))
    if (nrow(object_coords) != nrow(object)) 
      stop("Number of features in the matrix and MSnSet differ.")
    if (!all.equal(rownames(object_coords), featureNames(object))) 
      warning("Matrix rownames and feature names don't match")
  } 
  else stop("object must be an 'MSnSet' or a 'matrix' (if method == \"none\").")
  
  ## check fcol
  if (!is.null(fcol) && !fcol %in% fvarLabels(object)) {
    warning("No fcol found using fcol = NULL", immediate. = TRUE)
    fcol <- NULL
  }
  if (is.null(fcol)) {
    fcol <- "nullmarkers"
    fData(object)[, fcol] <- rep("unknown", nrow(object))
  }
  
  
  ## Update feature data and convert any columns that are matrices
  ## to vectors as otherwise in the shiny app will display these as
  ## a long vector of 1,0,0,0,0,1,0 etc.
  object <- .makeMatsVecs(object)
  
  ## Define DT columns
  origFvarLab <- fvarLabels(object)
  selDT <- .defineDT(origFvarLab, fcol)
  
  
  ## Check pmarkers, if not a matrix convert to a matrix
  pmarkers <- fData(object)[, fcol]
  .obj_list <- .chkMarkersMat(pmarkers, object, fcol)
  pmarkers <- .obj_list$pm
  object <- .obj_list$obj
  mName <- .obj_list$mN
  
  # if (!inherits(pmarkers, "matrix")) {
  #   mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
  #   if (fcol == "nullmarkers") {
  #     m <- matrix(1, ncol = 1, nrow = nrow(object))
  #     rownames(m) <- featureNames(object)
  #     colnames(m) <- fcol <- mName
  #     fData(object)[, mName] <- pmarkers <- m
  #     colnames(pmarkers) <- "unknown"
  #   } else {
  #     object <- mrkVecToMat(object, fcol, mfcol = mName)
  #     fcol <- mName
  #     pmarkers <- fData(object)[, fcol]
  #   }
  # }
  
  ## Create column of unknowns (needed later for plot2D in server)
  all_points <- paste0(format(Sys.time(), "%a%b%d%H%M%S%Y"), "unknowns")
  fData(object)[, all_points] <- "unknown"
  
  
  ## Setting features to be displayed
  if (!missing(foi)) {
    if (inherits(foi, "FeaturesOfInterest") | inherits(foi, "FoICollection")) {
      if (inherits(foi, "FeaturesOfInterest"))
        foi <- FoICollection(list(foi))
      foimarkers <- as(foi, "matrix")
      if (exists("pmarkers", inherits = FALSE)) {
        pmarkers <- merge(pmarkers, foimarkers,
                          by = 0, all.x = TRUE)
        rownames(pmarkers) <- pmarkers[, "Row.names"]
        pmarkers <- pmarkers[featureNames(object), -1]            
      } else pmarkers <- foimarkers
    } else {
      message("foi is not a valid FeaturesOfInterest or FoICollection object")
    }
  }
  
  ## more checks on the marker matrix
  sumpm <- apply(pmarkers, 2, sum, na.rm = TRUE)
  # if (fcol == "nullmarkers") sumpm <- 1
  if (any(sumpm == 0)) {
    if (fcol == "nullmarkers") {
      pmarkers <- pmarkers[, -which(sumpm == 0), drop = FALSE]
    } else {
      message(paste("foi object", names(which(sumpm == 0)), "does not match any featuresNames in the data, removing this foi"))
      pmarkers <- pmarkers[, -which(sumpm == 0), drop = FALSE]
    }
  }
  if (length(grep("GO:", colnames(pmarkers))) > 0) {
    cn <- pRoloc::flipGoTermId(colnames(pmarkers), names = FALSE, 
                               keepNA = FALSE)
    #     if (all(!is.na(cn))) {
    #       names(cn) <- NULL
    colnames(pmarkers) <- cn
    #     }
  }
  
  ## Marker colours
  cols <- getStockcol()
  if (length(cols) < ncol(pmarkers)) {
    message("Too many features for available colours. Some colours will be duplicated.")
    n <- ncol(pmarkers) %/% length(cols)
    cols <- rep(cols, n + 1)
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
  if (!all | ncol(pmarkers) > 30)
    pmsel <- 1
  
  
  # pcas <- plot2D(object, fcol = NULL, plot = FALSE,
  #                mirrorX = FALSE, mirrorY = FALSE, ...)
  # profs <- exprs(object)
  
  
  ## Settings and objects for app
  fd <- fData(object)
  profs <- exprs(object)
  feats <- toSel <- featureNames(object)
  idxDT <- numeric()
  namesIdxDT <- character()
  myclasses <- colnames(pmarkers)
  
  ## generate CSS for selectizeInput 
  css <- CSS(myclasses, cols[seq(myclasses)])
  
  ## generate UI inputs for colour picker 
  col_ids <-  paste0("col", seq(myclasses))
  colPicker <- function(x) {colourInput(col_ids[x], myclasses[x], 
                                        value = getStockcol()[x])}
  col_input <- lapply(seq(col_ids), colPicker)
  ll <- length(col_input)
  if (ll > 5) {
    n <- 2
    cw <- c("50%", "50%")
    ntv <- round(ll/n)
    num1 <- 1:ntv
    num2 <- (ntv+1):ll
  } else {
    n <- 1
    cw <- c("50%")
  }
  
  
  
  ## Build shiny app ==========================================================
  ## ===== UI =================================================================
  ## ==========================================================================
  
  ui <- tagList(
    fluidPage(
      titlePanel(h2("pRolocVis - explore", align = "right"), 
                 windowTitle = "pRolocVis"),
      # add css code for coloured selectizeInput
      tags$head(tags$head(uiOutput("css"))),     
      sidebarLayout(
        sidebarPanel(
          selectizeInput("markers", "Labels",
                         choices = myclasses,
                         multiple = TRUE,
                         selected = myclasses[pmsel]),
          sliderInput("trans", "Transparancy",
                      min = 0,  max = 1, value = 0.75),
          checkboxInput("checkbox", label = "Show labels", value = TRUE), 
          width = 2),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Spatial Map", id = "mapPanel",
                               fluidRow(
                                 column(9, plotOutput("spatialmap",
                                                      height = fig.height,
                                                      width = fig.width,
                                                      dblclick = "dblClick",
                                                      brush = brushOpts(
                                                        id = "plotBrush",
                                                        resetOnNew = TRUE))),
                                 column(2, 
                                        actionButton("resetButton", "Zoom/reset plot"),
                                        br(),
                                        actionButton("clear", "Clear selection"),
                                        br(),
                                        actionButton("resetColours", "Reset colours"),
                                        br(),
                                        downloadButton("downloadData", "Save selection"),
                                        br(),
                                        downloadButton("saveplot", "Download plot"))
                                )
                               ),
                      tabPanel("Protein Profiles", id = "profilesPanel",
                               plotOutput("profile",
                                          height = "400px",
                                          width = "120%")),
                      tabPanel("Table Selection", id = "tableSelPanel",
                               fluidRow(
                                 column(4,
                                        checkboxGroupInput("selTab", 
                                                           "Data columns to display",
                                                           choices = origFvarLab,
                                                           selected = selDT)))),
                      tabPanel("Colour picker", id = "colPicker",
                               if (ll > 5) {
                                 fluidRow(
                                   column(6, col_input[num1]), 
                                   column(6, col_input[num2]))
                               } else {
                                 fluidRow(
                                   col_input)
                               }, br(), br(), br(), br(), br() ## add whitespace
                                     # this is a list of N colour containers 
                      )),            # for N organelles
          
          ## feature data table is always visible
          fluidRow(
            column(12,
                   column(length(selDT),
                          DT::dataTableOutput("fDataTable"))))
        )
      )
      
    )
    
  )
  
  
  ## == SERVER ================================================================
  ## ==========================================================================
  
  server <-
    function(input, output, session) {
      
      ## --------Set reactive objects--------
      ## set brush bounds for zooming
      ranges <- reactiveValues(x = NULL, y = NULL)
      brushBounds <- reactiveValues(i =  try(object_coords[, 1] >= min(object_coords[, 1]) & 
                                               object_coords[, 1] <= max(object_coords[, 1])),
                                    j = try(object_coords[, 2] >= min(object_coords[, 2]) & 
                                              object_coords[, 2] <= max(object_coords[, 2])))
      resetLabels <- reactiveValues(logical = FALSE)
      
      ## Get coords for proteins according to selectized marker class(es)
      mrkSel <- reactive({
        lapply(input$markers,
               function(z) which(pmarkers[, z] == 1))
      })
      
      
      ## Update colours according to colourpicker input
      cols_user <- reactive({
        sapply(col_ids, function(z) input[[z]])
      })
      
      
      ## Update colour transparacy according to slider input
      myCols <- reactive({
        scales::alpha(cols_user(),
                      input$trans)[sapply(input$markers, function(z)
                        which(myclasses == z))]
      })
      myCols.bg <- reactive({
        # scales::alpha(cols.bg,
        #               NA)[sapply(input$markers, function(z)
        #                   which(myclasses == z))]
        darken(myCols())
      })
      
      
      ## Spatial map plot
      output$spatialmap <- renderPlot({
        par(mar = c(4, 4, 0, 0))
        par(oma = c(1, 0, 0, 0))
        .plot(object_coords, fd = fd, unk = TRUE,
              xlim = ranges$x,
              ylim = ranges$y,
              fcol = all_points)
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers))
            points(object_coords[mrkSel()[[i]], ], pch = 21,
                   cex = 1.4, bg = myCols()[i], col = myCols.bg()[i])
        }
        idxDT <<- feats[input$fDataTable_rows_selected] ## highlight point on plot by selecting item in table
        if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          .highlight(object_coords, fd, namesIdxDT)
          if (input$checkbox)
            .highlight(object_coords, fd, namesIdxDT, labels = TRUE)
        }
        resetLabels$logical <- FALSE
        height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
      })
      
      
      
      ## Protein profile
      output$profile <- renderPlot({
        par(mar = c(8, 3, 1, 1))
        par(oma = c(0, 0, 0, 0))
        ylim <- range(profs)
        n <- nrow(profs)
        m <- ncol(profs)
        fracs <- colnames(profs)
        plot(0, ylim = ylim, xlim = c(1, m), ylab = "Intensity",
             type = "n", xaxt = "n", xlab = "")
        axis(1, at = 1:m, labels = fracs, las = 2)
        title(xlab = "Fractions", line = 5.5)
        matlines(t(profs[feats, ]),
                 col = paste0("grey", 60),
                 lty = 1,
                 type = "l")
        if (!is.null(input$markers)) {
          for (i in 1:length(input$markers)) {
            matlines(t(profs[mrkSel()[[i]], ]),
                     col = myCols()[i],
                     lwd = 1.5)
          }
        }
        ## If an item is clicked in the table highlight profile
        idxDT <<- feats[input$fDataTable_rows_selected]
        namesIdxDT <<- names(idxDT)
        if (length(idxDT)) {
          matlines(t(profs[namesIdxDT, , drop = FALSE]),
                   col = "black",
                   lty = 1,
                   lwd = 3)
        }
      })            
      
      
      
      ## Feature data table
      output$fDataTable <- DT::renderDataTable({
        feats <<- which(brushBounds$i & brushBounds$j)
        ## Double clicking to identify protein
        if (!is.null(input$dblClick)) {
          dist <- apply(object_coords, 1, function(z) sqrt((input$dblClick$x - z[1])^2
                                                           + (input$dblClick$y - z[2])^2))
          idxPlot <- which(dist == min(dist))
          if (idxPlot %in% idxDT) {                            ## 1--is it already clicked?
            setsel <- setdiff(names(idxDT), names(idxPlot))  ## Yes, remove it from table
            idxDT <<- idxDT[setsel]
          } else {                                             ## 2--new click?
            idxDT <<- c(idxDT, idxPlot)                      ## Yes, highlight it to table
          }
        }
        namesIdxDT <<- names(idxDT)
        toSel <- match(namesIdxDT, rownames(fd)[brushBounds$i & brushBounds$j])
        if (resetLabels$logical) toSel <- numeric()
        ## don't display mName - see https://github.com/ComputationalProteomicsUnit/pRolocGUI/issues/52
        dtdata <- fd[, -grep(mName, colnames(fd))]
        dtdata <- dtdata[brushBounds$i & brushBounds$j, input$selTab]
        DT::datatable(data = dtdata,
                      filter = "top",
                      rownames = TRUE,
                      options = list(
                        search = list(regex = TRUE, 
                                      caseInsensitive = FALSE),
                        dom = "l<'search'>rtip",
                        pageLength = 10
                      ),
                      callback = JS(callback),
                      style = "bootstrap4",
                      selection = list(mode = 'multiple', selected = toSel))   
        # selection = list(mode = 'multiple', selected = toSel))  %>% 
        # DT::formatRound(5, 2) %>% 
        # DT::formatStyle(3:6, 'text-align' = 'center')
      }, server = FALSE)
      
      
      ## --------Reset button--------
      ## When a the reset button is clicked check to see is there is a brush on
      ## the plot, if yes zoom, if not reset the plot.
      observeEvent(input$resetButton, {
        brush <- input$plotBrush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          ranges$y <- c(brush$ymin, brush$ymax)
          brushBounds$i <- object_coords[, 1] >= brush$xmin & object_coords[, 1] <= brush$xmax
          brushBounds$j <- object_coords[, 2] >= brush$ymin & object_coords[, 2] <= brush$ymax
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
          brushBounds$i <- try(object_coords[, 1] >= min(object_coords[, 1])
                               & object_coords[, 1] <= max(object_coords[, 1]))
          brushBounds$j <- try(object_coords[, 2] >= min(object_coords[, 2])
                               & object_coords[, 2] <= max(object_coords[, 2]))
        }
      })
      
      ## --------Clear button--------
      ## When clear selection is pressed update clear idxDT above and reset selection
      observeEvent(input$clear, {
        resetLabels$logical <- TRUE
      })
      
      ## --------Save selection button--------
      ## When save button is download save points/proteins selected
      output$downloadData <- downloadHandler(
        file = "features.csv",
        content = function(file) { 
          write.table(namesIdxDT, file = file, quote = FALSE, 
                      row.names = FALSE, col.names = FALSE)
        }
      )
      
      ## --------Save figure button--------
      ## Save figure of map
      output$saveplot <- downloadHandler(
        file = "spatialmap.pdf" , 
        content = function(file) {
          pdf(file = file)
          par(mar = c(4, 4, 0, 0))
          par(oma = c(1, 0, 0, 0))
          .plot(object_coords, fd, unk = TRUE,
                xlim = ranges$x,
                ylim = ranges$y,
                fcol = all_points)
          if (!is.null(input$markers)) {
            for (i in 1:length(input$markers))
              points(object_coords[mrkSel()[[i]], ], pch = 21,
                     cex = 1.4, bg = myCols()[i], col = myCols.bg()[i])
          }
          idxDT <<- feats[input$fDataTable_rows_selected] ## highlight point on plot by selecting item in table
          if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
          namesIdxDT <<- names(idxDT)
          if (length(idxDT)) {
            .highlight(object_coords, fd, namesIdxDT)
            if (input$checkbox)
              .highlight(object_coords, fd, namesIdxDT, labels = TRUE)
          }
          resetLabels$logical <- FALSE
          height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
          dev.off()
        })
      
      ## update CSS colours in selectizeInput
      output$css <- renderUI({
        tags$style(HTML(CSS(myclasses, cols_user())))
      })
      
      
      ## reset colours to stockCols
      observeEvent(input$resetColours, {
        for (i in seq(ncol(pmarkers))) {
          updateColourInput(session, col_ids[i],
                            value = getStockcol()[i])
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
