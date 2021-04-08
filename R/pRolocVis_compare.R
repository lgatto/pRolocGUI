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
pRolocVis_compare <- function(object,
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


  myargs <- list(...)
  
  ## Check no more than 2 datasets are loaded
  if (length(object) != 2) stop(paste("object must be of length 2 (use the 'explore' app for 1 object)"))

  ## Check if object is an MSnSetList and if not, check it's a list of matrices 
  ## with MSnSets in the methargs (as per plot2D)
  if (inherits(object, "MSnSetList")) {
    object_coords <- lapply(object@x, function(x) plot2D(x, plot = FALSE, 
                                                         fcol = NULL, ...))
  }
  else if (inherits(object, "list")) {
    object_coords <- list()
    if (all(sapply(object, is.matrix))) {
      message(paste("---------------------------------------------------------",
                    "\nWhen passing a list of matrices as the object please check",
                    "\nthe arguments method = 'none' and metharg are also passed",
                    "\nSee ?plot2D and the pRolocGUI vignette for more details.",
                    "\n---------------------------------------------------------"))
      ### --------------------TEST THIS-------------------
      for (i in seq(object)) {
        .methargs <- myargs$methargs[[1]]  ## this should be a MSnSetList
        if (is.null(.methargs)) stop(paste("Missing methargs, please pass MSnSets as a MSnSetList, see ?pRolocVis"))
        if (!inherits(.methargs, "MSnSetList")) stop(paste("methargs must be a MSnSetList, see ?pRolocVis"))
        if (length(.methargs) != 2) stop(paste("methargs must be a MSnSetList of length 2, see ?pRolocVis"))
        # if (!all(sapply(.methargs, inherits, "MSnSet"))) stop(paste("methargs must be a list of MSnSets of length 2, see ?pRolocVis"))
        if (is.null(myargs$method)) stop(paste("method must be set to method = 'none' if a matrix is passed"))
        if (myargs$method != "none") stop(paste("method must be set to method = 'none' if a matrix is passed"))
        chk <- plot2D(object[[i]], plot = FALSE, fcol = NULL,
                      method = myargs$method, methargs = list(.methargs[[i]]))
        object_coords[[i]] <- object[[i]]
        object[[i]] <- myargs$methargs[[1]][[i]]
      }
    }
    else stop(paste("Object must be either a MSnSetList or a list of two matrices"))
  }
  else stop(paste("Object must be of class MSnSet or matrix"))  

  ## keep only intersection between datasets
  message(paste("Subsetting data and keeping the"))
  object <- commonFeatureNames(object)
  .cmnNam <- featureNames(object[[1]])
  object_coords <- lapply(object_coords, function(x) {
    x <- x[.cmnNam, ]
    return(x)
    })
  
  
  ## Check fcol is valid and if not add a new column called nullmarkers
  ## check fcol is not set, user must define fcol1 and fcol2 explicitly
  if (length(fcol) == 1) {
    fcol <- rep(fcol, 2)
    message(paste0(c("-----------------------------------------------",
                   "\nIf length(fcol) == 1 pRolocVis will assume that",
                   "\nthe same fcol is to be used for both datasets",
                   "\nsetting fcol = c(", fcol[1], ",", fcol[2],")",
                   "\n-----------------------------------------------")))
  }
  for (i in seq(fcol)) {
    if (!is.null(fcol[i]) && !fcol[i] %in% fvarLabels(object[[i]])) {
      stop("No fcol found, please specify fcol", immediate. = TRUE)
      # fcol[i] <- NULL
    }
    if (is.null(fcol[i])) {
      message(paste("fcol = NULL, no annotation column specified, setting fcol name to nullmarkers"))
      setUnknowncol("#BEBEBE")
      m <- matrix(0, ncol = 1, nrow = nrow(object[[i]]))
      rownames(m) <- featureNames(object[[i]])
      colnames(m) <- "0"
      object <- lapply(object, function(x) {
        fData(x)[, "nullmarkers"] <- 0
        return(x)
      })
      fcol <- rep("nullmarkers", 2)
    } else {
      if (!isMrkVec(object[[i]], fcol[i]) & !isMrkMat(object[[i]], fcol[i])) 
        stop("Your fcol (markers) are neither vector nor matrix. See ?markers for details.")
    }
  }
  
  ## Shorten markers names if too long
  for (i in seq_along(object)) {
    origCl <- getMarkerClasses(object[[i]])
    cn <- sapply(origCl,
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
    from <- setdiff(origCl, cn)
    to <- setdiff(cn, origCl)
    stopifnot(length(to) == length(from))
    x <- object[[i]]
    stopifnot(fcol[i] %in% fvarLabels(x))
    fvar <- fData(x)[[fcol[i]]]
    for (j in seq_along(from)) {
      fvar <- sub(from[j], to[j], fvar)
    }
    fData(x)[[fcol[i]]] <- fvar
    object@x[[i]] <- x
  }
  

  ## Update feature data and convert any columns that are matrices
  ## to vectors as otherwise in the shiny app these are displayed as
  ## a long vector of 1,0,0,0,0,1,0 etc
  for (i in seq(object)) {
    .tn <- length(fvarLabels(object[[i]]))
    chk <- vector(length = .tn)
    for (j in 1:.tn) {
      chk[j] <- is.matrix(fData(object[[i]])[, j])
    }
    if (any(chk)) {
      .ind <- which(chk)
      .nams <- fvarLabels(object[[i]])[.ind]
      .tmpnams <- paste0(.nams, format(Sys.time(), "%a%b%d%H%M%S%Y"))
      for (j in seq(.nams)) {
        object[[i]] <- mrkMatToVec(object[[i]], mfcol = .nams[j], vfcol = .tmpnams[j])
      }
      fData(object[[i]])[, .nams] <- NULL
      fvarLabels(object[[i]])[match(.tmpnams, fvarLabels(object[[i]]))] <- .nams
    }
  }
  
  
  ## Now extract all relevant data
  fd <- lapply(object, fData)                     # all featureData
  pd <- lapply(object, pData)
  pcol <- NULL                                    # replicate information
  profs <- lapply(object, MSnbase::exprs)                  # intensities
  mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
  pmarkers_msnset <- list()
  for (i in seq(object)) pmarkers_msnset[[i]] <- mrkVecToMat(object[[i]], fcol[i], mfcol = mName)
  pmarkers <- lapply(pmarkers_msnset, fData)     # marker matrix    
  pmarkers <- lapply(pmarkers, function(x) x[, mName])
  
  
  ## Check pmarkers, if not a matrix convert to a matrix
  for (i in seq(object)) {
    if (!inherits(pmarkers[[i]], "matrix")) {
      mName <- paste0("Markers", format(Sys.time(), "%a%b%d%H%M%S%Y"))
      object <- mrkVecToMat(object[[i]], fcol[i], mfcol = mName)
      fcol[i] <- mName
      pmarkers[[i]] <- fData(object[[i]])[, fcol[i]]
    }
  }
  
  
  ## Define DT columns (select only first 4 columns of fData to display on startup)
  ## initialize other objects for the datatable tracking
  origFvarLab <- lapply(fd, colnames)
  selDT <- lapply(origFvarLab, function(x) x[1:4])          
  feats <- rownames(fd[[1]])
  toSel <- 1:nrow(fd[[1]])
  idxDT <- character()
  myclasses <- unique(unlist(lapply(pmarkers, colnames)))
  
  ## generate UI inputs for colour picker 
  scheme = "white"
  scheme2 <- "black"
  cols <- appStockcol()
  if (length(cols) < max(sapply(pmarkers, ncol))) {
    message("Too many features for available colours. Some colours will be duplicated.")
    ind <- which.max(sapply(pmarkers, ncol))
    n <- ncol(pmarkers[[ind]]) / length(cols)
    cols <- rep(cols, n + 1)
  }
 
  cols <- cols[1:length(myclasses)]
  names(cols) <- myclasses
  col_ids <-  paste0("col", seq(myclasses))
  colPicker <- function(x) {colourpicker::colourInput(col_ids[x], myclasses[x], 
                                        value = appStockcol()[x])}
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
  
  header <- dashboardHeader(title = "pRolocGUI Compare",
                                # enable_rightsidebar = TRUE,
                            controlbarIcon = shiny::icon("gears"))
  
  sidebar <- dashboardSidebar(
    p(strong("Subcellular classes")),
    actionButton(inputId = "selectall", label="Select/clear all",
                 style='padding:4%; font-size:100%; margin:6px 5px 6px 20%') %>%
      helper(colour = "grey",
             type = "inline",
             buttonLabel = "classes",
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
      choices = myclasses,
      selected = myclasses,
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
                           fluidRow(
                             column(5, br(),
                                    plotOutput("pca1",
                                               height = fig.height,
                                               dblclick = "dblClick1",
                                               brush = brushOpts(
                                                 id = "plotBrush1",
                                                 resetOnNew = TRUE))),
                             column(5, br(),
                                    plotOutput("pca2",
                                               height = fig.height,
                                               dblclick = "dblClick2",
                                               brush = brushOpts(
                                                 id = "plotBrush2",
                                                 resetOnNew = TRUE))) %>%
                               helper(colour = "grey",
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
                                      size = "s"))
                  ),
                  tabPanel("Profiles", value = "profilesPanel1",
                           br(),
                           p(strong("Dataset 1")),
                           plotOutput("profile1",
                                      height = "400px",
                                      width = "100%") %>%
                             helper(colour = "grey",
                                    type = "inline",
                                    title = "Protein profiles",
                                    content = "Profile plot displaying the relative 
                                             abundance of each protein in each fraction 
                                             across the gradient employed.", size = "s"),
                           p(strong("Dataset 2")),
                           plotOutput("profile2",
                                      height = "400px",
                                      width = "100%")
                  ),
                  tabPanel("Profiles (by class)", value = "profilesPanel2",
                           fluidRow(
                             column(5, br(),
                                    p(strong("Dataset 1")),
                                    br(),
                                    plotOutput("classProfiles1",
                                               height = "1200px")),
                             column(5, br(),
                                    p(strong("Dataset 2")),
                                    br(),
                                    plotOutput("classProfiles2",
                                               height = "1200px"))
                           )),
                  tabPanel("Table Selection", value = "tableSelPanel",
                           fluidRow(
                             column(5, br(),
                                    checkboxGroupInput("selTab1", 
                                                       "Columns to display for data 1",
                                                       choices = origFvarLab[[1]],
                                                       selected = selDT[[1]])
                             ),
                             column(5, br(),
                                    checkboxGroupInput("selTab2",
                                                       "Columns to display for data 2",
                                                       choices = origFvarLab[[2]],
                                                       selected = selDT[[2]])
                                    
                             )
                           )),
                  tabPanel("Sample info", value = "sampleInfo",
                           br(),
                           # fluidRow(
                           #   column(5, 
                           p(strong("Sample data for data 1")), br(),
                           tableOutput("pdata1"),
                           p(strong("Sample data for data 2")), br(),
                           tableOutput("pdata2"),
                           br(),br(),
                  ),
                  
                  tabPanel("Colour picker", value = "colPicker",
                           fluidRow(br(),
                                    if (ll > 5) {
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  col_input[num1],
                                                  col_input[num2])
                                    } else {
                                      splitLayout(cellWidths = "50%",
                                                  col_input)
                                    }, br(), br(), br(), br(), br()  ## add whitespace
                           ))   # this is a list of N colour containers for N organelles
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
                           fluidRow(
                             column(5, br(),
                                    plotOutput("pca1",
                                               height = fig.height,
                                               dblclick = "dblClick1",
                                               brush = brushOpts(
                                                 id = "plotBrush1",
                                                 resetOnNew = TRUE))),
                             column(5, br(),
                                    plotOutput("pca2",
                                               height = fig.height,
                                               dblclick = "dblClick2",
                                               brush = brushOpts(
                                                 id = "plotBrush2",
                                                 resetOnNew = TRUE))) %>%
                               helper(colour = "grey",
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
                                      size = "s"))
                  ),
                  tabPanel("Profiles", value = "profilesPanel1",
                           br(),
                           p(strong("Dataset 1")),
                           plotOutput("profile1",
                                      height = "400px",
                                      width = "100%") %>%
                             helper(colour = "grey",
                                    type = "inline",
                                    title = "Protein profiles",
                                    content = "Profile plot displaying the relative 
                                             abundance of each protein in each fraction 
                                             across the gradient employed.", size = "s"),
                           p(strong("Dataset 2")),
                           plotOutput("profile2",
                                      height = "400px",
                                      width = "100%")
                  ),
                  # tabPanel("Profiles (by class)", value = "profilesPanel2",
                  #          fluidRow(
                  #            column(5, br(),
                  #                   p(strong("Dataset 1")),
                  #                   br(),
                  #                   plotOutput("classProfiles1",
                  #                              height = "1200px")),
                  #            column(5, br(),
                  #                   p(strong("Dataset 2")),
                  #                   br(),
                  #                   plotOutput("classProfiles2",
                  #                              height = "1200px"))
                  #          )),
                  tabPanel("Table Selection", value = "tableSelPanel",
                           fluidRow(
                             column(5, br(),
                                    checkboxGroupInput("selTab1", 
                                                       "Columns to display for data 1",
                                                       choices = origFvarLab[[1]],
                                                       selected = selDT[[1]])
                             ),
                             column(5, br(),
                                    checkboxGroupInput("selTab2",
                                                       "Columns to display for data 2",
                                                       choices = origFvarLab[[2]],
                                                       selected = selDT[[2]])
                                    
                             )
                           )),
                  tabPanel("Sample info", value = "sampleInfo",
                           br(),
                           # fluidRow(
                           #   column(5, 
                           p(strong("Sample data for data 1")), br(),
                           tableOutput("pdata1"),
                           p(strong("Sample data for data 2")), br(),
                           tableOutput("pdata2"),
                           br(),br(),
                  ),
                  
                  tabPanel("Colour picker", value = "colPicker",
                           fluidRow(br(),
                                    if (ll > 5) {
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  col_input[num1],
                                                  col_input[num2])
                                    } else {
                                      splitLayout(cellWidths = "50%",
                                                  col_input)
                                    }, br(), br(), br(), br(), br()  ## add whitespace
                           ))   # this is a list of N colour containers for N organelles
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
  #                                downloadButton("downloadData", "Save selection", style='padding:6px; font-size:90%'),
  #                                br(), br(),
  #                                downloadButton("saveplot", "Download plot", style='padding:6px; font-size:90%'),
  #                                br())
  # )
  controlbar <- dashboardControlbar(skin = "light",
                                   width = 160,
                                   .items = list(
                                     p(strong("Map controls")),
                                     br(),
                                     p("Transparency"),
                                     sliderInput("trans", NULL,
                                                 min = 0,  max = 1, value = 0.75),
                                     checkboxInput("checkbox", label = "Show labels", value = TRUE),
                                     br(),
                                     actionButton("resetButton", "Zoom/reset plot", style='padding:6px; font-size:90%'),
                                     br(), br(),
                                     actionButton("clear", "Clear selection", style='padding:6px; font-size:90%'),
                                     br(), br(),
                                     actionButton("resetColours", "Reset colours", style='padding:6px; font-size:90%'),
                                     br(), br(),
                                     downloadButton("downloadData", "Save selection", style='padding:6px; font-size:90%'),
                                     br(), br(),
                                     downloadButton("saveplot", "Download plot", style='padding:6px; font-size:90%'),
                                     br())
  )
  
  ui <- tags$body(class="skin-blue right-sidebar-mini control-sidebar-open", 
                  dashboardPage(header,
                                sidebar,
                                body,
                                controlbar
                                # sidebar_fullCollapse = TRUE
                                ))
  
  ui <- shinyUI(tagList(ui))
  
  
  #####################################################################
  ############################# SERVER  ############################### 
  #####################################################################
  
  
  server <- function(input, output, session) {
    
    observe_helpers()
    ## --------Set reactive objects--------
    ## define range for plots
    ranges <- reactiveValues(x = c(min(c(object_coords[[1]][, 1], 
                                         object_coords[[2]][, 1])), 
                                   max(c(object_coords[[1]][, 1], 
                                         object_coords[[2]][, 1]))),
                             y = c(min(c(object_coords[[1]][, 2], 
                                         object_coords[[2]][, 2])), 
                                   max(c(object_coords[[1]][, 2], 
                                         object_coords[[2]][, 2]))))
    
    
    ## Capture brushed proteins for zoom
    brushBounds1 <- reactiveValues(i =  try(object_coords[[1]][, 1] >= 
                                               min(object_coords[[1]][, 1]) & 
                                               object_coords[[1]][, 1] <= 
                                               max(object_coords[[1]][, 1])),
                                    j = try(object_coords[[1]][, 2] >= 
                                              min(object_coords[[1]][, 2]) & 
                                              object_coords[[1]][, 2] <= 
                                              max(object_coords[[1]][, 2])))
    brushBounds2 <- reactiveValues(i =  try(object_coords[[2]][, 1] >= 
                                               min(object_coords[[2]][, 1]) & 
                                               object_coords[[2]][, 1] <= 
                                               max(object_coords[[2]][, 1])),
                                    j = try(object_coords[[2]][, 2] >= 
                                              min(object_coords[[2]][, 2]) & 
                                              object_coords[[2]][, 2] <= 
                                              max(object_coords[[2]][, 2])))
    
    resetLabels <- reactiveValues(logical = FALSE)
    
    ## Get coords for proteins according to selectized marker class(es)
    ## NB: need two reactive objects here as markers in object[[1]] do not
    ## necessarily have the same indices as markers in object[[2]] (would 
    ## like to allow different markers between different datasets)
    mrkSel1 <- reactive({
      # lapply(input$markers,
      #       function(z) which(pmarkers[[1]][, z] == 1))
      ind <- match(input$markers, colnames(pmarkers[[1]]))
      .mrkSel1 <- list()
      if (length(ind) > 0) {
        for (i in seq(length(input$markers))) {
          if (is.na(ind[i])) {
            .mrkSel1[[i]] <- NA
          } else {
            .mrkSel1[[i]] <- which(pmarkers[[1]][, ind[i]] == 1)
          }
        }
      }
      # print(input$markers)
      # print(paste0("---------"))
      return(.mrkSel1)
    })
  
    mrkSel2 <- reactive({
      # lapply(input$markers,
      #       function(z) which(pmarkers[[2]][, z] == 1))
      ind <- match(input$markers, colnames(pmarkers[[2]]))
      .mrkSel2 <- list()
      if (length(ind) > 0) {
        for (i in seq(length(input$markers))) {
          if (is.na(ind[i])) {
            .mrkSel2[[i]] <- NA
          } else {
            .mrkSel2[[i]] <- which(pmarkers[[2]][, ind[i]] == 1)
          }
        }
      }
      .mrkSel2
    })
    
    
    ## Update colours according to colourpicker input
    cols_user <- reactive({
      cols_user <- sapply(col_ids, function(z) input[[z]])
      names(cols_user) <- myclasses
      # print(paste0("cols_user"))
      # print(cols_user)
      # print(paste0("---------"))
      return(cols_user)
    })
    
    
    ## Update colour transparacy according to slider input
    myCols <- reactive({
      myCols <- scales::alpha(cols_user(),
                    input$trans)[sapply(input$markers, function(z)
                      which(myclasses == z))]
      # print(paste0("myCols"))
      # print(myCols)
      # print(paste0("---------"))
      return(myCols)
    })
    
    myCols.bg <- reactive({
      darken(myCols())
    })
    
    profCols <- reactive({
      scales::alpha(cols_user(),
                    .4)[sapply(input$markers, function(z)
                      which(myclasses == z))]
    })
    
    

    ## ========================PCA plot========================
    ## ========================================================
    ## Generate PCA or MDS plot
    
    plotMap <- function(indData = 1, indMrk = mrkSel1()) {
      par(mar = c(4, 4, 0, 0))
      par(oma = c(1, 0, 0, 0))
      .plot2D_shiny(object_coords[[indData]], fd[[indData]], unk = TRUE,
                  xlim = ranges$x,
                  ylim = ranges$y,
                  fcol = fcol[indData])
      if (!is.null(input$markers)) {
        for (i in 1:length(input$markers)) {
          if (!is.na(indMrk[[i]][1]))
            points(object_coords[[indData]][mrkSel2()[[i]], ], pch = 16,
                   cex = 1.4, bg = myCols()[i], col = myCols.bg()[i])
        }
      }
      idxDT <<- feats[input$fDataTable_rows_selected] ## highlight point on plot by selecting item in table
      if (resetLabels$logical) idxDT <<- character()  ## If TRUE labels are cleared
      # namesIdxDT <<- names(idxDT)
      if (length(idxDT)) {
        .highlightOnPlot_shiny(object_coords[[indData]], idxDT)
        if (input$checkbox)
          .highlightOnPlot_shiny(object_coords[[indData]], idxDT, labels = TRUE)
      }
    }
    
    output$pca1 <- renderPlot({
      plotMap(indData = 1, indMrk = mrkSel1())
      height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
    })
    
    output$pca2 <- renderPlot({
      plotMap(indData = 2, indMrk = mrkSel2())
      resetLabels$logical <<- FALSE
      height <- reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)) # fix ratio 1:1
    })
    
  
    ## =====================PROFILES plot========================
    ## ==========================================================
    plotProfiles <- function(indData = 1, indMrk = mrkSel1()) {
      # browser()
      par(mar = c(13, 4, 1, 1), oma = c(0, 0, 0, 0), bg = scheme,
            col.axis = scheme2, col.main = scheme2,
            col.lab = scheme2, fg = scheme2)
      ylim <- range(profs[[indData]])
      n <- nrow(profs[[indData]])
      m <- ncol(profs[[indData]])
      fracs <- colnames(profs[[indData]])
      ## check if there are replicates and if their are, create breaks in the plot
      # if (!is.null(pcol)) {
      #   repInfo <- unique(pd[, pcol])
      #   repNames <- vector("list", length(repInfo))
      #   ## get fraction names by replicate
      #   fracNames <- lapply(repInfo, function(z) colnames(profs)[pd$Experiment == z])
      #   fracInds <- lapply(fracNames, function(z) which(z == colnames(profs)))
      # } else {
      fracInds <- list(seq(colnames(profs[[indData]])))
      # }
      ## get unknowns
      profs_un <- profs[[indData]][which(fd[[indData]][, fcol[indData]] == "unknown"), ]
      ## get quantiles for each fraction in unknowns
      quants <- apply(profs_un, MARGIN = 2, function(x) 
        quantile(x, c(0, 1), na.rm = TRUE))  # max and min for unknowns
      bound_low <- quants[1, ]
      bound_high <- quants[2, ]
      ## get quantiles for subcellular classes
      mrkProfs <- lapply(indMrk, function(z) profs[[indData]][z, , drop = FALSE])   # 5% and 95% quantiles for all other classes
      quants <- lapply(mrkProfs, function(z) apply(z, MARGIN = 2, function(x) 
        quantile(x, c(0.05, .95), na.rm = TRUE)))
      meanProfs <- lapply(mrkProfs, function(z) apply(z, 2, mean, na.rm = TRUE))
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
      # namFeats <- names(feats)[which(names(feats) %in% rownames(profs_un))]
      zoomedProts <-  which(brushBounds1$i & brushBounds1$j)
      namFeats <- names(zoomedProts)[which(names(zoomedProts) %in% rownames(profs_un))]
      ## plot unknowns
      invisible(lapply(fracInds, function(x)     # plot all unknowns as lines here
        matlines(x, t(profs_un[namFeats, x, drop = FALSE]),
                 col = "grey90", lty = 1, lwd = 1, type = "l")
      ))
      ## markers
      if (!is.null(input$markers)) {
        for (i in 1:length(input$markers)) {
          if (!is.na(indMrk[[i]][1])) {
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
        }
      }
      ## If an item is clicked in the table highlight profile
      idxDT <<- feats[input$fDataTable_rows_selected]
      # namesIdxDT <<- names(idxDT)
      if (length(idxDT) > 0) {
        invisible(lapply(fracInds, function(z)     # don't plot all lines
          matlines(z, t(profs[[indData]][idxDT, z, drop = FALSE]),
                   col = "black",   # would like to colour by location here need names vector of colours
                   lty = 5, lwd = 2,
                   type = "l")))
      }
    } ## ----------- end of function----------------

    output$profile1 <- renderPlot({
      plotProfiles(1, mrkSel1())
    })

    output$profile2 <- renderPlot({
      plotProfiles(2, mrkSel2())
    })
    
    ## =====================FACET profiles plot========================
    ## ================================================================
    if (classProfiles) {
      output$classProfiles1 <- renderPlot({
        plotFacetProfiles(profs[[1]], fcol[1], fd[[1]], 
                          pd[[1]], col = cols_user(), ncol = 1)
      })
      
      output$classProfiles2 <- renderPlot({
        plotFacetProfiles(profs[[2]], fcol[2], fd[[2]], 
                          pd[[2]], col = cols_user(), ncol = 1)
      })
    }
    
    
    
    ## ========================DATA TABLE========================
    ## ==========================================================
    output$fDataTable <- DT::renderDataTable({
      
      feats <<- unique(c(names(which(brushBounds1$i & brushBounds1$j)),
                         names(which(brushBounds2$i & brushBounds2$j))))
    ## Double clicking to identify protein
      if (!is.null(input$dblClick1)) {
        l2_dist <- apply(object_coords[[1]], 1, function(z) sqrt((input$dblClick1$x - z[1])^2 
                                                              + (input$dblClick1$y - z[2])^2))
        idxPlot <- names(which(l2_dist == min(l2_dist)))
        if (idxPlot %in% idxDT) {                          ## 1--is it already clicked?
          # setsel <- setdiff(names(idxDT), names(idxPlot))                 ## Yes, remove it from table
          idxDT <<- setdiff(idxDT, idxPlot)
        } else {                                         ## 2--new click?
          idxDT <<- c(idxDT, idxPlot)                    ## Yes, highlight it to table
        }
      }
      if (!is.null(input$dblClick2)) {
        l2_dist <- apply(object_coords[[2]], 1, function(z) sqrt((input$dblClick2$x - z[1])^2 
                                                              + (input$dblClick2$y - z[2])^2))
        idxPlot <- names(which(l2_dist == min(l2_dist)))
        if (idxPlot %in% idxDT) {                          ## 1--is it already clicked?
          # setsel <- setdiff(names(idxDT), names(idxPlot)) 
          idxDT <<- setdiff(idxDT, idxPlot)                        ## Yes, remove it from table
        } else {                                         ## 2--new click?
          idxDT <<- c(idxDT, idxPlot)                       ## Yes, highlight it to table
        }
      } 
      toSel <- match(idxDT, feats)                        ## selection to highlight in DT
      if (resetLabels$logical) toSel <- numeric()        ## reset labels
      .dt1 <- fd[[1]][feats, input$selTab1, drop = FALSE]
      .dt2 <- fd[[2]][feats, input$selTab2, drop = FALSE]
      colnames(.dt2) <- paste0('<span style="color:',   
                               rep("darkblue", ncol(.dt1)), '">', 
                               colnames(.dt2), '</span>')
      dtdata <- cbind(.dt1, .dt2)
      DT::datatable(data = dtdata,
                    filter = "top",
                    rownames = TRUE,
                    selection = list(mode = 'multiple', selected = toSel),
                    options = list(
                      search = list(regex = TRUE, 
                                    caseInsensitive = TRUE),
                      dom = "l<'search'>rtip",
                      pageLength = 100,
                      scrollX = TRUE
                    ),
                    callback = JS(callback),
                    style = "bootstrap4",
                    escape = FALSE)  %>%     ## NB: `escape = FALSE` required for colname coloring
        formatStyle(columns = colnames(.dt2), color = c("darkblue")) 
    }, server = FALSE)
    
    
    ## =====================ZOOM/RESET plot========================
    ## ============================================================
    ## When a the reset button is clicked check to see is there is a brush on
    ## the plot, if yes zoom, if not reset the plot.
    observeEvent(input$resetButton, {
      .brush1 <- input$plotBrush1
      .brush2 <- input$plotBrush2
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
        bminx <- min(c(object_coords[[1]][, 1], object_coords[[2]][, 1]))
        bmaxx <- max(c(object_coords[[1]][, 1], object_coords[[2]][, 1]))
        bminy <- min(c(object_coords[[1]][, 2], object_coords[[2]][, 2]))
        bmaxy <- max(c(object_coords[[1]][, 2], object_coords[[2]][, 2]))
      }
      ranges$x <- c(bminx, bmaxx)
      ranges$y <- c(bminy, bmaxy)
      brushBounds1$i <- try(object_coords[[1]][, 1] >= bminx 
                             & object_coords[[1]][, 1] <= bmaxx)
      brushBounds1$j <- try(object_coords[[1]][, 2] >= bminy 
                             & object_coords[[1]][, 2] <= bmaxy)
      brushBounds2$i <- try(object_coords[[2]][, 1] >= bminx 
                             & object_coords[[2]][, 1] <= bmaxx)
      brushBounds2$j <- try(object_coords[[2]][, 2] >= bminy 
                             & object_coords[[2]][, 2] <= bmaxy)
    })
    
    
    ## =====================CLEAR LABELS=========================
    ## ==========================================================
    ## When clear selection is pressed update clear idxDT above 
    ## and reset selection
    observeEvent(input$clear, {
      resetLabels$logical <<- TRUE
    })
    
    ## =====================DOWNLOAD DATA=========================
    ## ===========================================================
    ## When save button is download save points/proteins selected
    output$downloadData <- downloadHandler(
      filename = "features.csv",
      content = function(file) { 
        write.table(idxDT, file = file, quote = FALSE, 
                    row.names = FALSE, col.names = FALSE)
      }
    )
    
    
    ### =====================SAVE FIGURES=========================
    ## ===========================================================
    ## Save figure of PCA
    output$saveplot <- downloadHandler(
      filename = function(){"plot.pdf"}, 
      content = function(file) {
        if (input$tabs == "mapPanel") {
          pdf(file = file, width = 13, height = 6)
          par(mfrow = c(1, 2)) 
          plotMap(indData = 1, indMrk = mrkSel1())
          plotMap(indData = 2, indMrk = mrkSel2())  
          dev.off()  
        } 
        else if (input$tabs == "profilesPanel1") {
          pdf(file = file, width = 13, height = 6)
          par(mfrow = c(1, 2)) 
          plotProfiles(indData = 1, indMrk = mrkSel1())
          plotProfiles(indData = 2, indMrk = mrkSel2())  
          dev.off()
        } 
        else if (input$tabs == "profilesPanel2") {
          # if (ncol(profs) < 15) {
          #   w <- 10
          #   h <- 10
          # } else {
          #   w <- round(ncol(profs)/1.5)
          #   h <- ncol(profs)/2
          # }
          profByClass1 <- plotFacetProfiles(profs[[1]], fcol[1], 
                                            fd[[1]], pd[[1]], 
                                            col = cols_user(), ncol = 1)
          profByClass2 <- plotFacetProfiles(profs[[2]], fcol[2], 
                                            fd[[2]], pd[[2]], 
                                            col = cols_user(), ncol = 1)
          ggsave(filename = file, plot = profByClass1, device = "pdf", width = 12, height = 5) 
          ggsave(filename = file, plot = profByClass2, device = "pdf", width = 12, height = 5) 
        } 
        # 
        # else if (input$tabs == "profilesPanel2") {
        #   if (ncol(profs) < 15) {
        #     w <- 10
        #     h <- 10
        #   } else {
        #     w <- round(ncol(profs)/1.5)
        #     h <- ncol(profs)/2
        #   }
        #   mycol <- c(cols_user(), "grey")
        #   profByClass <- plotFacetProfiles(df = calcData[[1]], col = mycol, reps = FALSE)
        #   ggsave(filename = file, plot = profByClass, device = "pdf", width = w, height = h) 
        # } 
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
            choices = myclasses,
            selected = myclasses,
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
            choices = myclasses,
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
    output$pdata1 <- renderTable(pd[[1]])
    output$pdata2 <- renderTable(pd[[2]])
    
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
  
