## remove this when building package
# source("utils.R")
# source("css.R")

## DOUBLE CLICKING TO HIGHLIGHT on/off on PCA plot, or selection via table

## Shiny: spinning loading wheel on top of plot while plot is recalculating
## https://gist.github.com/daattali/edd7c20cd09f484b7f32

## References
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://gallery.shinyapps.io/095-plot-interaction-advanced/
## https://gallery.shinyapps.io/105-plot-interaction-zoom/
## https://gallery.shinyapps.io/106-plot-interaction-exclude/
## https://github.com/rstudio/shiny-examples
## http://shiny.rstudio.com/articles/selecting-rows-of-data.html
## http://shiny.rstudio.com/articles/plot-interaction-advanced.html
## https://rinterface.com/shiny/shinydashboardPlus/#

##' @rdname pRolocVis-apps
##' @param classProfiles A \code{logical} indicating if a tab displaying
##' individual class profile plots should be displayed. Default is \code{FALSE}. 
pRolocVis_explore <- function(object,
                              fcol = "markers",
                              classProfiles = FALSE,
                              fig.height = "700px",
                              # fig.width = "100%",
                              # legend.width = "200%",
                              # legend.cex = 1,
                              nchar = 25,
                              # all = TRUE,
                              ...) {
                          
  
  #####################################################################
  ##################### Initialize app settings  ###################### 
  #####################################################################

  ## Check if MSnSet and if not, check it's a matrix with MSnSet in 
  ## the methargs (as per plot2D)
  myargs <- list(...)

  if (inherits(object, "MSnSet")) {
    object_coords <- plot2D(object, plot = FALSE, fcol = NULL, ...)
  } 
  else if (inherits(object, "matrix")) {
    message(paste("---------------------------------------------------------",
                  "\nWhen passing a matrix as the object please check that the",
                  "\narguments method = 'none' and methargs are also passed",
                  "\nSee ?plot2D and the pRolocGUI vignette for more details.",
                  "\n---------------------------------------------------------"))
    chk <- plot2D(object, plot = FALSE, fcol = NULL, ...)
    object_coords <- object
    object <- myargs$methargs[[1]]
  }
  else stop(paste("Object must be of class MSnSet or matrix"))  
  .xlab <- colnames(object_coords)[1]
  .ylab <- colnames(object_coords)[2]
  


  # if (!inherits(object, "MSnSet") | !is.matrix(object)) {
  #   if (is.matrix(object)) {
  #     if ("method" %in% names(myargs)) {
  #       if (myargs$method == "none") {
  #         
  #         
  #         if ("dims" %in% names(myargs)) {
  #           if (length(myargs$dims) != 2) stop("Only 2 dimensions allowed for 2D plotting, check dims argument")
  #           object_coords <- object[, myargs$dims]
  #         }
  #         else {
  #           if (ncol(object) != 2) stop("Only 2 dimensions allowed for 2D plotting, check dimensions of object")
  #           object_coords <- object
  #         }
  #         if ("methargs" %in% names(myargs)) stop("methargs must be supplied if object is a matrix and method must be 'none'")
  #         object <- myargs$methargs[[1]]
  #         if (!inherits(object, "MSnSet")) 
  #           stop(paste("If method == \"none\", and the object is a 'matrix',", 
  #                      "methargs must be a supplied as list(MSnSet)"))
  #         if (nrow(object_coords) != nrow(object)) 
  #           stop("Number of features in the matrix and feature metadata differ.")
  #         if (!all.equal(rownames(object_coords), featureNames(object))) 
  #           stop(paste("Matrix rownames and feature names don't match"))
  #         
  #       } else stop(paste("If object is a matrix, method must be set to 'none' and methargs must be provided"))
  #     }
  #     else stop("If object is a matrix, method must be set to 'none' and methargs must be provided")
  #     .xlab <- colnames(object_coords)[1]
  #     .ylab <- colnames(object_coords)[2]
  #   } 
  #   else object_coords <- plot2D(object, plot = FALSE, ...)
  # }
  # else stop("object must be an 'MSnSet' or a 'matrix' (if method == \"none\")")
    
   
  ## Check for missing values
  if (anyNA(exprs(object))) {
    chk <- unique(which(is.na(exprs(object)), arr.ind=TRUE)[,1])
  
    object <- object[-chk, ]
    if (nrow(object) != nrow(object_coords)) {
      object_coords <- object_coords[!chk, , drop = FALSE]
    }
    if (all(featureNames(object) != rownames(object_coords))) {
      stop(paste("Row/featureNames in matrix/MSnSet do not match"))
    }
    message(paste(c("--------------------------------------------------",
                    "\nMissing values in dataset",
                    "\n--------------------------------------------------")))
  }

  ## Check fcol is present and if not add a new column called nullmarkers
  if (!is.null(fcol) && !fcol %in% fvarLabels(object)) {
    warning("No fcol found using fcol = NULL", immediate. = TRUE)
    fcol <- NULL
  }
  if (is.null(fcol)) {
    message(paste("fcol = NULL, no annotation column specified, setting fcol name to nullmarkers"))
    setUnknowncol("#BEBEBE")
    fcol <- "nullmarkers"
    m <- matrix(0, ncol = 1, nrow = nrow(object))
    rownames(m) <- featureNames(object)
    colnames(m) <- "0"
    fData(object)[, fcol] <- m
  } else {
    if (!isMrkVec(object, fcol) & !isMrkMat(object, fcol)) 
    stop("Your fcol (markers) are neither vector nor matrix. See ?markers for details.")
  }
  
  ## Shorten markers names if too long
  getMyClasses <- getMarkerClasses(object, fcol = fcol)
  cn <- sapply(getMyClasses,
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
  diffNam1 <- setdiff(getMyClasses, cn)
  diffNam2 <- setdiff(cn, getMyClasses)
  for (i in seq(diffNam1)) {
    object <- fDataToUnknown(object, fcol = fcol, 
                             from = diffNam1[i], 
                             to = diffNam2[i])
  }
  

  ## Update feature data and convert any columns that are matrices
  ## to vectors as otherwise in the shiny app these are displayed as
  ## a long vector of 1,0,0,0,0,1,0 etc.
  .tn <- length(fvarLabels(object))
  chk <- vector(length = .tn)
  for (i in 1:.tn) {
    chk[i] <- is.matrix(fData(object)[, i])
  }
  if (any(chk)) {
    .ind <- which(chk)
    .nams <- fvarLabels(object)[.ind]
    .tmpnams <- paste0(.nams, format(Sys.time(), "%a%b%d%H%M%S%Y"))
    for (i in seq(.nams)) {
      object <- mrkMatToVec(object, mfcol = .nams[i], vfcol = .tmpnams[i])
    }
    fData(object)[, .nams] <- NULL
    fvarLabels(object)[match(.tmpnams, fvarLabels(object))] <- .nams
  }
  
  
  ## Now extract all relevant data
  fd <- fData(object)                             # all featureData
  pd <- pData(object)
  if (ncol(pd) == 0) {
    pd <- data.frame("Information" = "No sample information provided (see pData(MSnSet) and ?pData for examples)")
  }
  pcol <- NULL                                    # replicate information
  profs <- MSnbase::exprs(object)                          # intensities
  mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
  pmarkers_msnset <- mrkVecToMat(object, fcol, mfcol = mName)
  pmarkers <- fData(pmarkers_msnset)[, mName]     # marker matrix    
  
  ## Check pmarkers, if not a matrix convert to a matrix
  if (!inherits(pmarkers, "matrix")) {
    if (fcol == "nullmarkers") {
      pmarkers <- matrix(1, ncol = 1, nrow = nrow(object))
      rownames(pmarkers) <- rownames(profs)
      colnames(pmarkers) <- fcol
    }
    else {
      mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
    object <- mrkVecToMat(object, fcol, mfcol = mName)
    fcol <- mName
    pmarkers <- fData(object)[, fcol]
    }
  }

  ## Define DT columns (select only first 4 columns of fData to display on startup)
  ## initialize other objects for the datatable tracking
  origFvarLab <- colnames(fd)
  selDT <- colnames(fd)[1:4]           
  feats <- toSel <- idxDT <- numeric()
  namesIdxDT <- character()
  
  ## Marker colours
  scheme = "white"
  scheme2 <- "black"
  
  cols <- appStockcol()
  if (length(cols) < ncol(pmarkers)) {
    message("Too many features for available colours. Some colours will be duplicated.")
    n <- ncol(pmarkers) %/% length(cols)
    cols <- rep(cols, n + 1)
  }
  
  ## generate UI inputs for colour picker 
  col_ids <-  paste0("col", seq(colnames(pmarkers)))
  myclasses <- colnames(pmarkers)
  colPicker <- function(x) {colourpicker::colourInput(col_ids[x], myclasses[x], value = appStockcol()[x])}
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

  
  #####################################################################
  ########################### BUILD UI  ############################### 
  #####################################################################
  
  header <- dashboardHeader(title = "pRolocGUI Explore",
                            # enable_rightsidebar = TRUE,
                            controlbarIcon = shiny::icon("gears"))
  
  sidebar <- dashboardSidebar(
    p(strong("Subcellular classes")),
    actionButton(inputId = "selectall", label="Select/clear all",
                 style='padding:4%; font-size:100%; margin:6px 5px 6px 20%') %>%
      helper(icon = "circle-question",
             colour = "grey",
             type = "inline",
             title = "Explore compartments",
             content = c("This sidebar allows you to explore proteins that 
                         belong to pre-defined subcellular classes. To remove 
                         or add the class labels on the spatial map click 
                         the desired button that corresponds to the compartments 
                         name. All class labels can be added back to the plot 
                         (or fully removed) by clicking them individually 
                         or using the \"Select/clear all\" action button."), 
             size = "s"),
    checkboxGroupButtons(
      inputId = "markers",
      label = "",
      choices = colnames(pmarkers),
      selected = colnames(pmarkers),
      direction = "vertical",
      width = "100%",
      size = "xs",
      checkIcon = list(
        yes = icon("ok",
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    ),
    minified = FALSE
    # minified = FALSE,
  )
  
  if (classProfiles) {
    body <- dashboardBody(
      ## trigger resize of plot  when sidebars are clicked
      tags$script('
      $(".navbar-custom-menu").on("click",function(){
        $(window).trigger("resize");
      })'
      ),
      ## update colours in css according to selected colorpicker
      tags$head(uiOutput("css")),
      ## css for styling app
      tags$head(tags$style(HTML(css_hs))),
      useShinyjs(),
      tags$hr(),
      ## main body of the app
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("Spatial Map", value = "mapPanel",
                           br(),
                           plotOutput("pca",
                                      height = fig.height,
                                      dblclick = "dblClick",
                                      brush = brushOpts(
                                        id = "pcaBrush",
                                        resetOnNew = TRUE)) %>%
                             helper(icon = "circle-question",
                                    colour = "grey",
                                    type = "inline",
                                    title = "Interactive data projection",
                                    content = c("This visualisation is an interactive 
                                  projection of the dataset. Each point on the plot 
                                  represents one protein.<br /> <br /> Double click 
                                  points on the plot to identify them (similarly you 
                                  can double click to remove them or alternatively 
                                  use the \"Clear selection\" button in the left 
                                  tab panel to remove all highlighted proteins). 
                                  If you would like to highlight proteins without 
                                  displaying their name/ID untick \"Show Labels\" 
                                  in the left panel.<br /> <br /> Searching: Use 
                                  the search box below the plot to search and find 
                                  your favourite proteins. Batch searching is enabled 
                                  but requires that protein IDs/features/text are 
                                  separated by spaces. Search matches will appear 
                                  in the table below. Click the desired row entry(s) 
                                  in the table and they will be highlighed on the plot.
                                  <br /> <br /> Interactive zooming: Click and brush 
                                  areas of the plot (use your mouse to click and brush 
                                  a rectangular area of the plot) and then click the 
                                  \"Zoom/reset\" button in the bottom left panel. 
                                  <br /> <br /> Exporting: Highlighed proteins can 
                                  be exported to a .csv file by clicking \"Export selected\". 
                                  Highlighted proteins can be removed from the selection 
                                  by clicking \"Clear selection\". <br /> <br /> Rendering 
                                  of images: Use the \"Download plot\" button to 
                                  save a high resolution PDF of the data."), 
                                    size = "s")
                  ),
                  tabPanel("Profiles", value = "profilesPanel1",
                           br(),
                           p(strong("Protein profiles")) %>%
                             helper(icon = "circle-question",
                                    colour = "grey",
                                    type = "inline",
                                    title = "Protein profiles",
                                    content = c("Profile plot displaying the relative 
                                             abundance of each protein in each fraction 
                                             across the gradient employed."), size = "s"),
                           plotOutput("profile1",
                                      height = "550px")),
                  tabPanel("Profiles (by class)", value = "profilesPanel2",
                           br(),
                           plotOutput("profile2",
                                      height = "800px")),
                  tabPanel("Table Selection", id = "tableSelPanel",
                           br(),
                           fluidRow(
                             column(4,
                                    checkboxGroupInput("selTab",
                                                       "Data columns to display",
                                                       choices = origFvarLab,
                                                       # choices = origFvarLab[-grep(mName, origFvarLab)],
                                                       selected = selDT)))),
                  # tabPanel("Table Legends", value = "tbl",
                  #          tableOutput("tbl")),
                  tabPanel("Sample info", value = "sampleInfo",
                           br(), br(),
                           tableOutput("pdata"), br()), 
                  tabPanel("Colour picker", value = "colPicker",
                           br(),
                           fluidRow(
                             if (ll > 5) {
                               splitLayout(cellWidths = c("50%", "50%"),
                                           col_input[num1],
                                           col_input[num2])
                             } else {
                               splitLayout(cellWidths = "50%",
                                           col_input)
                             }, br(), br(), br(), br(), br()  ## add whitespace
                           ), br(), br())   # this is a list of N colour containers for N organelles
      ),      #===end TABS in MP===
      
      ## feature data table is always visible
      DT::dataTableOutput("fDataTable")
      
    )
  } else {
    body <- dashboardBody(
      ## trigger resize of plot  when sidebars are clicked
      tags$script('
      $(".navbar-custom-menu").on("click",function(){
        $(window).trigger("resize");
      })'
      ),
      ## update colours in css according to selected colorpicker
      tags$head(uiOutput("css")),
      ## css for styling app
      tags$head(tags$style(HTML(css_hs))),
      useShinyjs(),
      tags$hr(),
      ## main body of the app
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("Spatial Map", value = "mapPanel",
                           br(),
                           plotOutput("pca",
                                      height = fig.height,
                                      dblclick = "dblClick",
                                      brush = brushOpts(
                                        id = "pcaBrush",
                                        resetOnNew = TRUE)) %>%
                             helper(icon = "circle-question",
                                    colour = "grey",
                                    type = "inline",
                                    title = "Interactive data projection",
                                    content = c("This visualisation is an interactive 
                                  projection of the dataset. Each point on the plot 
                                  represents one protein.<br /> <br /> Double click 
                                  points on the plot to identify them (similarly you 
                                  can double click to remove them or alternatively 
                                  use the \"Clear selection\" button in the left 
                                  tab panel to remove all highlighted proteins). 
                                  If you would like to highlight proteins without 
                                  displaying their name/ID untick \"Show Labels\" 
                                  in the left panel.<br /> <br /> Searching: Use 
                                  the search box below the plot to search and find 
                                  your favourite proteins. Batch searching is enabled 
                                  but requires that protein IDs/features/text are 
                                  separated by spaces. Search matches will appear 
                                  in the table below. Click the desired row entry(s) 
                                  in the table and they will be highlighed on the plot.
                                  <br /> <br /> Interactive zooming: Click and brush 
                                  areas of the plot (use your mouse to click and brush 
                                  a rectangular area of the plot) and then click the 
                                  \"Zoom/reset\" button in the bottom left panel. 
                                  <br /> <br /> Exporting: Highlighed proteins can 
                                  be exported to a .csv file by clicking \"Save selection\". 
                                  Highlighted proteins can be removed from the selection 
                                  by clicking \"Clear selection\". <br /> <br /> Rendering 
                                  of images: Use the \"Download plot\" button to 
                                  save a high resolution PDF of the data."), 
                                    size = "s")
                  ),
                  tabPanel("Profiles", value = "profilesPanel1",
                           br(),
                           br(),
                           p(strong("Protein profiles")) %>%
                             helper(icon = "circle-question",
                                    colour = "grey",
                                    type = "inline",
                                    title = "Protein profiles",
                                    content = c("Profile plot displaying the relative 
                                             abundance of each protein in each fraction 
                                             across the gradient employed."), size = "s"),
                           plotOutput("profile1",
                                      height = "550px")),
                  # tabPanel("Profiles (by class)", value = "profilesPanel2",
                  #          br(),
                  #          plotOutput("profile2",
                  #                     height = "800px")),
                  tabPanel("Table Selection", id = "tableSelPanel",
                           br(),
                           fluidRow(
                             column(4,
                                    checkboxGroupInput("selTab",
                                                       "Data columns to display",
                                                       choices = origFvarLab,
                                                       # choices = origFvarLab[-grep(mName, origFvarLab)],
                                                       selected = selDT)))),
                  # tabPanel("Table Legends", value = "tbl",
                  #          tableOutput("tbl")),
                  tabPanel("Sample info", value = "sampleInfo",
                           br(), br(),
                           tableOutput("pdata"), br()), 
                  tabPanel("Colour picker", value = "colPicker",
                           br(),
                           fluidRow(
                             if (ll > 5) {
                               splitLayout(cellWidths = c("50%", "50%"),
                                           col_input[num1],
                                           col_input[num2])
                             } else {
                               splitLayout(cellWidths = "50%",
                                           col_input)
                             }, br(), br(), br(), br(), br()  ## add whitespace
                           ), br(), br())   # this is a list of N colour containers for N organelles
      ),      #===end TABS in MP===
      
      ## feature data table is always visible
      DT::dataTableOutput("fDataTable")
      
    )
  }
  
  
  # rightsidebar <- .setRightSidebar(background = "light",
  #                              width = 160,
  #                              .items = list(
  #                                p(strong("Map controls")),
  #                                br(),
  #                                p("Transparency"),
  #                                sliderInput("trans", NULL,
  #                                            min = 0,  max = 1, value = 0.75),
  #                                checkboxInput("checkbox", label = "Show labels", value = TRUE),
  #                                br(),
  #                                actionButton("resetButton", "Zoom/reset plot", style='padding:6px; font-size:90%'),
  #                                br(), br(),
  #                                actionButton("clear", "Clear selection", style='padding:6px; font-size:90%'),
  #                                br(), br(),
  #                                actionButton("resetColours", "Reset colours", style='padding:6px; font-size:90%'),
  #                                br(), br(),
  #                                downloadButton("exportSelected", "Save selection", style='padding:6px; font-size:90%'),
  #                                br(), br(),
  #                                downloadButton("saveplot", "Download plot", style='padding:6px; font-size:90%'),
  #                                br())
  # )
  
  controlbar <- dashboardControlbar(
    skin = "light",
    width = 160,
    .list = list(
      p(strong(" Map controls")),
      br(),
      p(" Transparency"),
      sliderInput("trans", NULL,
                  min = 0,  max = 1, value = 0.9),
      checkboxInput("checkbox", label = "Show labels", value = TRUE),
      br(),
      actionButton("resetButton", "Zoom/reset plot", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br(), br(),
      actionButton("clear", "Clear selection", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br(), br(),
      actionButton("resetColours", "Reset colours", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br(), br(),
      downloadButton("exportSelected", "Export selected", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br(), br(),
      downloadButton("exportData", "Export data", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br(), br(),
      downloadButton("saveplot", "Download plot", style='padding:8px; font-size:90%; margin:3px 3px 3px 6px'),
      br())
  )
  
  
  ui <- tags$body(class="skin-blue right-sidebar-mini control-sidebar-open", 
                  dashboardPage(header,
                                sidebar,
                                body,
                                controlbar
                                # sidebar_fullCollapse = TRUE))
                  ))
                                                                                               
  ui <- shinyUI(tagList(ui))
  
  
  #####################################################################
  ############################# SERVER  ############################### 
  #####################################################################
  
  
  server <- function(input, output, session) {
    
    observe_helpers()
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
             function(z) which(pmarkers[, z, drop = FALSE] == 1))
    })
    
    
    ## Update colours according to colourpicker input
    cols_user <- reactive({
      cols_user <- sapply(col_ids, function(z) input[[z]])
      names(cols_user) <-  myclasses
      return(cols_user)
    })
    
    
    ## Update colour transparacy according to slider input
    myCols <- reactive({
      scales::alpha(cols_user(),
                    input$trans)[sapply(input$markers, function(z)
                      which(colnames(pmarkers) == z))]
    })
    myCols.bg <- reactive({
      # scales::alpha(cols.bg,
      #               NA)[sapply(input$markers, function(z)
      #                   which(colnames(pmarkers) == z))]
      darken(darken(darken(darken(myCols()))))
    })
    profCols <- reactive({
      scales::alpha(cols_user(),
                    .4)[sapply(input$markers, function(z)
                      which(colnames(pmarkers) == z))]
    })
    
    

    ## --------PCA plot--------
    ## Generate PCA or MDS plot
    output$pca <- renderPlot({
      par(mar = c(4, 4, 0, 0))
      par(oma = c(1, 0, 0, 0))
      .plot2D_shiny(object_coords, fd, unk = TRUE,
             xlim = ranges$x,
             ylim = ranges$y,
             fcol = fcol)
      if (!is.null(input$markers)) {
        for (i in 1:length(input$markers))
          points(object_coords[mrkSel()[[i]], , drop = FALSE], pch = 21,
                 cex = 2, bg = myCols()[i], col = myCols.bg()[i])
      }
      idxDT <<- feats[input$fDataTable_rows_selected] ## highlight point on plot by selecting item in table
      if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
      namesIdxDT <<- names(idxDT)
      if (length(idxDT)) {
        .highlightOnPlot_shiny(object_coords, namesIdxDT)
        if (input$checkbox)
          .highlightOnPlot_shiny(object_coords, namesIdxDT, labels = TRUE)
      }
      resetLabels$logical <- FALSE
      height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
    })
    
  
    ## profile plot
    output$profile1 <- renderPlot({
      par(mar = c(13, 4, 1, 1), oma = c(0, 0, 0, 0), bg = scheme, 
          col.axis = scheme2, col.main = scheme2, 
          col.lab = scheme2, fg = scheme2)
      ylim <- range(profs)
      n <- nrow(profs)
      m <- ncol(profs)
      fracs <- colnames(profs)
      ## check if there are replicates and if their are, create breaks in the plot
      # if (!is.null(pcol)) {
      #   repInfo <- unique(pd[, pcol])
      #   repNames <- vector("list", length(repInfo))
      #   ## get fraction names by replicate
      #   fracNames <- lapply(repInfo, function(z) colnames(profs)[pd$Experiment == z])
      #   fracInds <- lapply(fracNames, function(z) which(z == colnames(profs)))
      # } else {
      fracInds <- list(seq(colnames(profs)))
      # }
      ## get unknowns
      profs_un <- profs[which(fd[, fcol] == "unknown"), ]
      ## get quantiles for each fraction in unknowns
      quants <- apply(profs_un, MARGIN = 2, function(x) quantile(x, c(0, 1)))  # max and min for unknowns
      bound_low <- quants[1, ]
      bound_high <- quants[2, ]
      ## get quantiles for subcellular classes
      mrkProfs <- lapply(mrkSel(), function(z) profs[z, , drop = FALSE])   # 5% and 95% quantiles for all other classes
      quants <- lapply(mrkProfs, function(z) apply(z, MARGIN = 2, function(x) quantile(x, c(0.05, .95))))
      meanProfs <- lapply(mrkProfs, function(z) apply(z, 2, mean)) 
      
      ## make polygon plots
      plot(0, ylim = ylim, xlim = c(1, m),
           type = "n", xaxt = "n", yaxt = "n", xlab = "", 
           ylab = "Intensities", cex.axis = 1.2,
           cex.lab = 1.2)
      v_x <- axis(1, at = 1:m, labels = fracs, las = 2, cex.axis = 1.2)
      v_y <- axis(2)
      abline(v = v_x, h = v_y, lty = "dotted", col = "lightgray", lwd = 1)
      mtext("Fractions", side=1, line=8, cex = 1.2)
      
      ## update lines on plot according to zoom
      # feats <<- which(brushBounds$i & brushBounds$j)
      namFeats <- names(feats)[which(names(feats) %in% rownames(profs_un))]
      
      ## plot unknowns
      invisible(lapply(fracInds, function(x)     # plot all unknowns as lines here
        matlines(x, t(profs_un[namFeats, x]),
                 col = "grey90", lty = 1, lwd = 1, type = "l")
      ))
      
      ## markers
      for (i in seq(input$markers)) {
        invisible(lapply(fracInds, function(x)     # don't plot all lines
          polygon(c(x, rev(x)), 
                  c(quants[[i]][2, x], rev(quants[[i]][1, x])),
                  col = profCols()[i], border = FALSE)
        ))
        invisible(lapply(fracInds, function(z)     # plot the mean profile
          matlines(z, meanProfs[[i]][z],
                   col = myCols()[i],
                   lty = 1, lwd = 1,
                   type = "l")))
      }
      ## If an item is clicked in the table highlight profile
      idxDT <<- feats[input$fDataTable_rows_selected]
      namesIdxDT <<- names(idxDT)
      if (length(idxDT)) {
        invisible(lapply(fracInds, function(z)     # don't plot all lines
          matlines(z, t(profs[namesIdxDT, z, drop = FALSE]),
                   col = "black",   # would like to colour by location here need names vector of colours
                   lty = 5, lwd = 2,
                   type = "l")))
      }
    })

    ## Class specific/faceted plots
    if (classProfiles) {
      output$profile2 <- renderPlot({
        plotFacetProfiles(profs, fcol, fd, pd, col = cols_user())
      }) 
    }
    
    
    ## --------Display/update data table--------
    ## Feature data table
    output$fDataTable <- DT::renderDataTable({
      feats <<- which(brushBounds$i & brushBounds$j)
      ## Double clicking to identify protein
      if (!is.null(input$dblClick)) {
        l2_dist <- apply(object_coords, 1, function(z) sqrt((input$dblClick$x - z[1])^2
                                                         + (input$dblClick$y - z[2])^2))
        idxPlot <- which(l2_dist == min(l2_dist))
        if (idxPlot %in% idxDT) {                            ## 1--is it already clicked?
          setsel <- setdiff(names(idxDT), names(idxPlot))   ## Yes, remove it from table
          idxDT <<- idxDT[setsel]
        } else {                                             ## 2--new click?
          idxDT <<- c(idxDT, idxPlot)                       ## Yes, highlight it to table
        }
      }
      namesIdxDT <<- names(idxDT)
      toSel <- match(namesIdxDT, rownames(fd)[brushBounds$i & brushBounds$j])
      if (resetLabels$logical) toSel <- numeric()
      ## don't display mName - see https://github.com/ComputationalProteomicsUnit/pRolocGUI/issues/52
      # dtdata <- fd[, -grep(mName, colnames(fd))]
      dtdata <- fd[, ] # remove this
      dtdata <- dtdata[brushBounds$i & brushBounds$j, input$selTab]
      DT::datatable(data = dtdata,
                    filter = "top",
                    rownames = TRUE,
                    options = list(
                      search = list(regex = TRUE, 
                                    caseInsensitive = TRUE),
                      dom = "l<'search'>rtip",
                      pageLength = 100
                      # scrollX = TRUE
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
      brush <- input$pcaBrush
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
    
    ## --------Export selected button--------
    ## When save button is download save points/proteins selected
    output$exportSelected <- downloadHandler(
      filename = "features.csv",
      content = function(file) { 
        write.table(cbind(profs[namesIdxDT, , drop = FALSE], 
                          fd[namesIdxDT, , drop = FALSE]), 
                    file = file, quote = FALSE, 
                    col.names = NA, row.names = TRUE, 
                    sep = "\t")
      }
    )
    
    ## --------Export data button--------
    ## When save button is download whole dataset
    output$exportData <- downloadHandler(
      filename = "fulldataset.csv",
      content = function(file) { 
        write.table(cbind(profs, fd), 
                  file = file, quote = FALSE, 
                  row.names = TRUE, col.names = NA,
                  sep = "\t")
      }
    )
    
    ## --------Save figure button--------
    ## Save figure of PCA
    output$saveplot <- downloadHandler(
      filename = function(){"plot.pdf"}, 
      content = function(file) {
        if (input$tabs == "mapPanel") {
          pdf(file = file)
          par(mar = c(4, 4, 0, 0))
          par(oma = c(1, 0, 0, 0))
          .plot2D_shiny(object_coords, fd, unk = TRUE,
                 xlim = ranges$x,
                 ylim = ranges$y,
                 fcol = fcol)
          if (!is.null(input$markers)) {
            for (i in 1:length(input$markers))
              points(object_coords[mrkSel()[[i]], , drop = FALSE], pch = 21,
                     cex = 1.4, bg = myCols()[i], col = myCols.bg()[i])
          }
          idxDT <<- feats[input$fDataTable_rows_selected] ## highlight point on plot by selecting item in table
          if (resetLabels$logical) idxDT <<- numeric()  ## If TRUE labels are cleared
          namesIdxDT <<- names(idxDT)
          if (length(idxDT)) {
            .highlightOnPlot_shiny(object_coords, fd, namesIdxDT)
            if (input$checkbox)
              .highlightOnPlot_shiny(object_coords, fd, namesIdxDT, labels = TRUE, cex = 1)
          }
          resetLabels$logical <- FALSE
          height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
          dev.off()  
        } 
        else if (input$tabs == "profilesPanel1") {
          if (ncol(profs) < 15) {
            w <- 7
          } else {
            w <- 10
          }
          pdf(file = file, width = w)
          par(mar = c(13, 4, 1, 1), oma = c(0, 0, 0, 0), bg = scheme, 
              col.axis = scheme2, col.main = scheme2, 
              col.lab = scheme2, fg = scheme2)
          ylim <- range(profs)
          n <- nrow(profs)
          m <- ncol(profs)
          fracs <- colnames(profs)
          ## check if there are replicates and if their are, create breaks in the plot
          if (!is.null(pcol)) {
            repInfo <- unique(pd[, pcol])
            repNames <- vector("list", length(repInfo))
            ## get fraction names by replicate
            fracNames <- lapply(repInfo, function(z) colnames(profs)[pd$Experiment == z])
            fracInds <- lapply(fracNames, function(z) which(z == colnames(profs)))
          } else {
            fracInds <- list(seq(colnames(profs)))
          }
          ## get unknowns
          profs_un <- profs[which(fd[, fcol] == "unknown"), ]
          ## get quantiles for each fraction in unknowns
          quants <- apply(profs_un, MARGIN = 2, function(x) quantile(x, c(0, 1)))  # max and min for unknowns
          bound_low <- quants[1, ]
          bound_high <- quants[2, ]
          ## get quantiles for subcellular classes
          mrkProfs <- lapply(mrkSel(), function(z) profs[z, ])   # 5% and 95% quantiles for all other classes
          quants <- lapply(mrkProfs, function(z) apply(z, MARGIN = 2, function(x) quantile(x, c(0.05, .95))))
          meanProfs <- lapply(mrkProfs, function(z) apply(z, 2, mean)) 
          
          ## make polygon plots
          plot(0, ylim = ylim, xlim = c(1, m),
               type = "n", xaxt = "n", yaxt = "n", xlab = "", 
               ylab = "Normalised intensities", cex.axis = 1.2,
               cex.lab = 1.2)
          v_x <- axis(1, at = 1:m, labels = fracs, las = 2, cex.axis = 1.2)
          v_y <- axis(2)
          abline(v = v_x, h = v_y, lty = "dotted", col = "lightgray", lwd = 1)
          mtext("Fractions", side=1, line=8, cex = 1.2)
          
          ## update lines on plot according to zoom
          feats <<- which(brushBounds$i & brushBounds$j)
          namFeats <- names(feats)[which(names(feats) %in% rownames(profs_un))]
          
          ## plot unknowns
          invisible(lapply(fracInds, function(x)     # plot all unknowns as lines here
            matlines(x, t(profs_un[namFeats, x, drop = FALSE]),
                     col = "grey90", lty = 1, lwd = 1, type = "l")
          ))
          for (i in seq(input$markers)) {
            invisible(lapply(fracInds, function(x)     # don't plot all lines
              polygon(c(x, rev(x)), 
                      c(quants[[i]][2, x], rev(quants[[i]][1, x])),
                      col = profCols()[i], border = FALSE)
            ))
            invisible(lapply(fracInds, function(z)     # plot the mean profile
              matlines(z, meanProfs[[i]][z],
                       col = myCols()[i],
                       lty = 1, lwd = 1,
                       type = "l")))
          }
          ## If an item is clicked in the table highlight profile
          idxDT <<- feats[input$fDataTable_rows_selected]
          namesIdxDT <<- names(idxDT)
          if (length(idxDT)) {
            invisible(lapply(fracInds, function(z)     # don't plot all lines
              matlines(z, t(profs[namesIdxDT, z, drop = FALSE]),
                       col = "black",   # would like to colour by location here need names vector of colours
                       lty = 5, lwd = 2,
                       type = "l")))
          }
          dev.off()
        } 
        else if (input$tabs == "profilesPanel2") {
          if (ncol(profs) < 15) {
            w <- 10
            h <- 10
          } else {
            w <- round(ncol(profs)/1.5)
            h <- ncol(profs)/2
          }
          profByClass <- plotFacetProfiles(profs, fcol, fd, pd, col = cols_user())
          ggsave(filename = file, plot = profByClass, device = "pdf", width = w, height = h) 
        } 
        else {
          pdf(file = file)
          plot(0,type='n',axes=FALSE,ann=FALSE)
          mtext("No plot selected")
          dev.off()
        }
      })
    
    ## update colours in selectizeInput
    output$css <- renderUI({
      tags$style(HTML(css_colours(cols_user())))
    })
    
    
    ## reset colours to stockCols
    observeEvent(input$resetColours, {
      for (i in seq(ncol(pmarkers))) {
        colourpicker::updateColourInput(session, col_ids[i],
                                        value = appStockcol()[i])
      }
    })
    
    ## updatecheckbox
    observe({
      if (input$selectall > 0) {
        if (input$selectall%%2 == 0){
          updateCheckboxGroupButtons(
            session = session,
            inputId = "markers",
            label = "",
            choices = colnames(pmarkers),
            selected = colnames(pmarkers),
            # direction = "vertical",
            # individual = TRUE,
            # justified = TRUE,
            # width = "100%",
            size = "xs",
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          )
          
        } else {
          updateCheckboxGroupButtons(
            session = session,
            inputId = "markers",
            label = "",
            choices = colnames(pmarkers),
            selected = c(),
            # direction = "vertical",
            # individual = TRUE,
            # justified = TRUE,
            # width = "100%",
            size = "xs",
            checkIcon = list(
              yes = icon("ok",
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          )
          
        }}
    })
    
    ## table legends
    # output$tbl <- renderTable(pd)
    
    ## table legends
    output$pdata <- renderTable(pd)
    
    ## control sidebars
    # observe({
    #   if (input$tabs == "loadingpage") {
    #     addClass(selector = "body", class = "sidebar-collapse")
    #     removeClass(selector = "body", class = "control-sidebar-open")
    #   } else {
    #     removeClass(selector = "body", class = "sidebar-collapse")
    #     addClass(selector = "body", class = "control-sidebar-open")
    #   }
    # })
    # observeEvent(input$openright, {addClass(selector = "body", class = "control-sidebar-open")})
    
  } # end of server
  
  app <- list(ui = ui, server = server)
  runApp(app)
}
  
