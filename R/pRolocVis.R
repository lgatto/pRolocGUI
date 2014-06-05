#'@name pRolocVis
#'@title pRolocVis
#'@export
#'@author Thomas Naake <tn299@@cam.ac.uk>
#'@usage pRolocVis(object = NULL)
#'@param object Pass a MSnSet to pRolocVis directly. Default \code{NULL} will
#'enable possibility to upload MSnSet in pRolovVis. 
#'@description A function to start a shiny session with one MSnSet data set. 
#'Run \code{pRolocVis()} to start the shiny application and choose between
#'three example MSnSet originating from \code{pRolocdata} or upload your
#'own MSnSet. Choosing between the tabs allows to display PCA plots,
#'protein profiles, the underlying data and upload abilities for past
#'search results.
#'@examples \dontrun{pRolocVis(object = NULL)}
#'
#'@export
pRolocVis <- function(object = NULL) {    
    ## global
    ## load MSnSets
    data(andy2011, package = "pRolocdata")
    data(tan2009r1, package = "pRolocdata")
    data(dunkley2006, package = "pRolocdata")
    ## increase upload limit to 20 MB
    options(shiny.maxRequestSize = 20*1024^2)
    
    ## is there a pRolocGUI_SearchResults? is so load it.
    ## possibly FeaturesOfInterest -> FoICollection
    ## else empty FoICollection
    if (exists("pRolocGUI_SearchResults", .GlobalEnv)) {
        sr <- get("pRolocGUI_SearchResults", .GlobalEnv)
        if (inherits(sr, "FoICollection"))
            sr <- sr
        else {
            if (inherits(sr, "FeaturesOfInterest")) { 
                coll <- FoICollection()
                sr <- addFeaturesOfInterest(sr, coll)
            } else
                sr <- NULL
        }
    } else {
        sr <- NULL
    }
        
    app <- list(  
        ui = 
            bootstrapPage( 
                fluidPage(
                    responsive = TRUE,
                    ## Application title
                    .pRn1_setTitlePanel(),
                    ## Sidebar Panel
                    sidebarPanel(
                        .pR_tags(), 
                        .pR_condDisplaySelection(),
                        .pR_condTabData(),
                        .pR_condTabPCA(),
                        .pR_condTabProteinProfiles(),
                        .pR_condTabQuantitation(),
                        .pR_condTabfData(),
                        .pR_condTabpData(),
                        .pR_condTabSearch(),
                        width = 3
                    ),
                    ## Main Panel
                    mainPanel(
                        tabsetPanel(
                            .pR_tabPanelData(),
                            .pR_tabPanelPCA(),
                            .pR_tabPanelProteinProfiles(),
                            .pR_tabPanelQuantitation(),
                            .pR_tabPanelfData(),
                            .pR_tabPanelpData(),
                            .pR_tabPanelSearch(),
                            id = "tab1"
                        ),
                        width = 9
                    )
                )
            ),
        
        server = function(input, output) {   
            
            ## START: links to vignette ## 
            vignette <- system.file("doc/pRolocVis.html", package="pRolocGUI")  
                
            if (nchar(vignette))
                addResourcePath(prefix = "doc", 
                    directoryPath = system.file("doc",
                    package = "pRolocGUI"))
                
            ## Links to vignette ##
            output$linkDisplay <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#display",
                      "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
                
            output$linkData <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPCA <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisPCA",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPP <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisPP",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkExprs <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisExprs",
                        "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
            
            output$linkfData <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisfData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkpData <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVispData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkSearch <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisSearch",
                        "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
        
            ## END: Links to vignette ## 
            
            
            
            ## TAB: DATA/UPLOAD ##
            
            ## upload function for own data, access to data path implemented 
            ## by index "datapath", 
            ## see ?shiny::fileInput for further details
            output$Data1 <- renderUI({
                ## choose Data source, a drop down list of
                ## andy2011, dunkley2006, tan2009r1 (all example
                ## data) or use your own data by selecting load
                ## data
                selectInput("data",
                            "Choose MSnSet data source:",
                            choices = c("andy2011", 
                                "dunkley2006",
                                "tan2009r1", 
                                "own data"),
                            selected=ifelse(is.null(object), 
                                "andy2011", 
                                "own data"))                                
            })
            
            output$Data2 <- renderUI({
                if (is.null(object))
                    fileInput("owndata", 
                            "Select your own MSnSet file",
                            ## accept=c('.rda', 'data/rda', 
                            ## '.RData', 'data/RData'),
                            multiple = FALSE)
                else {
                    if (inherits(object, "MSnSet"))
                        helpText("object is an MSnSet")
                    else 
                        helpText("object corrupt, 
                            MSnSet 'andy2011' will be used")
                }
                
            })
            
            output$Data3 <- renderUI({
                if (is.null(object))
                    textOutput("warningowndataUI")
            })

            .dIownData <- reactive({
                if (!length(as.character(input$owndata["datapath"])))
                    od <- andy2011
                else {
                    ## check if MSnSet has ending .rda or .RData and if 
                    ## it is MSnSet
                    if (file_ext(input$owndata["name"]) %in% c("rda","RData")) {
                        if (inherits(
                            get(load(as.character(input$owndata["datapath"]))), 
                            "MSnSet"))
                            od <- get(load(
                                as.character(input$owndata["datapath"])
                            ))
                        else
                            od <- andy2011
                    } else
                        od <- andy2011
                }
            })
            
            ## .dI
            .dI <- reactive({
                if (is.null(object)) {
                    if (!is.null(input$data))
                        switch(input$data,
                            "andy2011" = andy2011,
                            "dunkley2006" = dunkley2006,
                            "tan2009r1" = tan2009r1,
                            "own data" = .dIownData()
                        ) 
                } else {
                    if (inherits(object, "MSnSet"))
                        object
                    else 
                        andy2011
                }
                
            })
            
            output$warningowndataUI <- renderText({
                if (input$data == "own data") {
                    if (identical(.dI(), andy2011))
                        return("noMSnSet selected, 
                                MSnSet 'andy2011' will be used")
                    else
                        return()
                } else
                    return()
            })  
            ## END: UPLOAD ##
            
            
            ## START OF SEARCH IMPLEMENTATION ##
            
            ## check boxes by clicking on plots PCA and plotDist
            dSelect <- reactiveValues(PCA = NULL, plotDist = NULL, text = NULL)
            
            observe({
                dSelect$PCA <- .selPCA(
                    dSelect$PCA, input$PCAclick, .protPCA$mult
                )
                dSelect$plotDist <- .selPlotDist(
                    dSelect$plotDist, input$plotDistclick, .protPlotDist$mult
                )
                dSelect$text <- .selText(
                    dSelect$text, input$saveText, input$resetMult, 
                    .protText$mult
                )  
            })
            
            output$checkBoxUI <- renderUI({
                checkboxGroupInput("chooseIdenSearch", 
                                label = "",
                                choices = c("PCA" = "mousePCA",
                                    "protein profiles" = "mousePlotDist",
                                    "saved searches" = "savedSearches",
                                    "query" = "text"),
                                selected = c(
                                    dSelect$PCA, dSelect$plotDist, dSelect$text)
                )
            })
            
            ## reactive expressions for general search
            ## reactive expression to forward indices to 
            ## plot2D, plotDist and tabs quantitation
            ## and feature meta-data
            .searchInd <- reactive(
                .sI(input$chooseIdenSearch, input$tagSelectList, .protText$mult, 
                    .protPCA$mult, .protPlotDist$mult, .whichNamesFOI())
            )
            
            ## Clear multiple points on click
            observe({
                if (input$resetMult > 0) {
                    .protPCA$mult <- NULL
                    .protPlotDist$mult <- NULL
                    .protText$mult <- NULL
                    dSelect$PCA <- NULL
                    dSelect$plotDist <- NULL
                    dSelect$text <- NULL
                }
            })
            
            ## text-based search: protein und fvarLabels
            output$searchUI <- renderUI({
                if (!is.null(.dI()))
                    selectInput("search", "", 
                                choices = c("protein", fvarLabels(.dI()))
                    )
            })
            
            output$searchResultsUI <- renderUI(
                if(!is.null(input$search)) {
                    if (length(.searchResultsText()))
                        selectInput("sRTextInput", label = "",
                                    choices = .searchResultsText())
                    else 
                        return("not found")
                }
            )
            
            ## reactive expressions for text based search
            ## levels to search
            .searchResultsText <- reactive({
                subset(
                    (
                        if(input$search != "protein")
                            names(table(fData(.dI())[input$search]))
                        else
                            row.names(.dI())
                    ), 
                    grepl(input$level.search,
                          if(input$search != "protein")
                              names(table(fData(.dI())[input$search]))
                          else
                              row.names(.dI())
                          )
                )
            })
            
            ## vector with reactive values
            .protText <- reactiveValues(mult=NULL)
            
            ## observe indices and concatenate to protText$mult
            observe({
                sRText <- isolate(input$sRTextInput)
                if (!is.null(input$search)) {
                    if (input$search == "protein")
                        newInd <- which(rownames(andy2011) == sRText)
                    else 
                        newInd <- which(fData(.dI())[input$search] == sRText)
                    if (!is.null(newInd)) {
                        if (input$saveText > 0 && length(newInd > 0))
                            isolate({
                                input$saveText
                                .protText$mult <- 
                                    isolate(c(.protText$mult, newInd))
                            })
                    }
                }
            })
            ## END OF SEARCHING IMPLEMENTATION ##  
                        

            ## TAB: PCA PLOT ##  
                        
            ## point size
            .fcex <- reactive({
                ## check for numericcolums in fData(.dI()) 
                colNum <- which(sapply(fData(.dI()), is.numeric))
                ## write indices in vector colNum
                colNum <- as.vector(colNum)
                if(length(colNum))
                    ## return fvarLabels of numeric colums 
                    fvarLabels(.dI())[colNum]
            })
            
            ## values of PCA, dims is dependent on user input,
            ## so is xlim and ylim
            .valuesPCA <- reactive({
                if (!is.null(.dI()) && !is.null(input$PCAn1))
                    plot2D(.dI(), fcol=NULL,
                        dims=c(as.numeric(input$PCAn1),
                        as.numeric(input$PCAn2)), 
                        plot=FALSE)
            })
            
            ## render colour selectInput accordingly to fvarLabels()
            output$fcoloursOutput <- renderUI({ 
                if (!is.null(.dI()))
                    selectInput("fcolours", "colour", c("none",fvarLabels(.dI())),
                                selected="none")
            })
            
            output$fsymboltypeOutput <- renderUI({ 
                if (!is.null(input$fcolours) && 
                        input$fcolours %in% fvarLabels(.dI())) 
                    selectInput("fsymboltype", "symbol type", 
                                c("none", fvarLabels(.dI())),
                                selected="none")
            })
            
            output$fcexOutput <- renderUI({
                ## initially !length(input$fcolours)
                ## to avoid an error message we have an outer if statement
                ## only show when there are numeric columns in fData (.fcex())
                if (length(input$fcolours) && length(.fcex())) {
                    if (input$fcolours != "none")
                        selectInput("fcex", "point size", c("1", .fcex()),
                                    selected = "1")
                    else
                        return()
                }
            })
            
            ## zoom function: parameters for x- and y-range for PCA plot
            output$xrangeUI <- renderUI({
                if(!is.null(.valuesPCA()))
                    ## get max and min values of first principal component
                    ## create a range slider
                    sliderInput("xrange", "zoom x-axis", 
                                min = min(.valuesPCA()[, 1]) - 1,
                                max = max(.valuesPCA()[, 1]) + 1,
                                value = c(min(.valuesPCA()[, 1]), 
                                    max(.valuesPCA()[, 1]))
                    )
            })  
            
            output$yrangeUI <- renderUI({
                if(!is.null(.valuesPCA()))
                    ## get max and min values of second principal component
                    ## create a range slider
                    sliderInput("yrange", "zoom y-axis",
                                min = min(.valuesPCA()[, 2]) - 1, 
                                max = max(.valuesPCA()[, 2]) + 1,
                                value = c(min(.valuesPCA()[, 2]), 
                                    max(.valuesPCA()[, 2]))
                    )
            })
            
            ## compute number of principal components to look for 
            ## and change UI accordingly
            output$PCAn1UI <- renderUI({
                if (!is.null(.dI()))
                    selectInput("PCAn1", "PC along x axis",
                                selected = 1,
                                choices = c(1:ncol(exprs(.dI())))
                    )
            })
            
            output$PCAn2UI <- renderUI({
                if (!is.null(.dI()))
                    selectInput("PCAn2", "PC along y axis",
                                selected = 2,
                                choices = c(1:ncol(exprs(.dI())))
                    )
            })
            
            output$PCALegendUI <- renderUI({
                if (length(input$fcolours))
                    if (input$fcolours %in% fvarLabels(.dI()))
                        ## tick box: add legend
                        checkboxInput("legendyes", "legend", 
                                value = FALSE)
            })
            
            output$PCALegendposUI <- renderUI({
                if (length(input$fcolours))
                    if (input$fcolours %in% fvarLabels(.dI()))
                        ## drop down menu for position of legend
                        selectInput("legendpos", "position of legend",
                                    choices = c("bottomright", "bottom",
                                        "bottomleft","left", "topleft", "top",
                                        "topright", "right","center"), 
                                    selected="bottomright")
            })
            
            ## Generate PCA plot, use fcolours for colours and add legend
            ## function (appearance and position dependent of user input)
            output$PCA <- renderPlot(
                if (!is.null(.dI()))
                    .plotPCA(data = .dI(), 
                        fcolours = input$fcolours, 
                        fcex = input$fcex,
                        xrange = input$xrange,
                        yrange = input$yrange,
                        sb = input$fsymboltype,
                        PCAn1 = input$PCAn1,
                        PCAn2 = input$PCAn2,
                        legend = input$legendyes, 
                        legendpos = input$legendpos,
                        sI = .searchInd(),
                        cIS = input$chooseIdenSearch
                    )
                )
            
            
            ## for Plot/Download button (needs a reactive expression)
            .PCAPlotReac <- reactive(
                .plotPCA(data = .dI(), 
                        fcolours = input$fcolours, 
                        fcex = input$fcex,
                        xrange = input$xrange,
                        yrange = input$yrange,
                        sb = input$fsymboltype,
                        PCAn1 = input$PCAn1,
                        PCAn2 = input$PCAn2,
                        legend = input$legendyes, 
                        legendpos = input$legendpos,
                        sI = .searchInd(),
                        cIS = input$chooseIdenSearch
                )
            )
            
            
            ## Download Handler for PCA plot
            output$plotPCADownload <- downloadHandler(
                filename = function() {
                    paste(input$data, "-" , Sys.Date(), '.jpg', sep='')
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCAPlotReac()
                    dev.off()
                }
            )
            
            ## reactive expressions for search based on cursor input for PCA
            minDist2dProtPCA <- reactive(
                ## will be empty initially
                if (!is.null(input$PCAclick) && !is.null(.valuesPCA())) {
                    ## compute 2D distances from click input to each component 
                    ## of the PCA plot, input$PCAclick$x and input$PCAclick$y
                    ## is user input (index will be returned)
                    .minDistPCA(inputx = input$PCAclick$x, 
                                inputy = input$PCAclick$y,
                                valuesx = .valuesPCA()[,1],
                                valuesy = .valuesPCA()[,2])
                }
            )
            
            minDist2dProtPCAHover <- reactive(
                if (!is.null(input$PCAhover) && !is.null(.valuesPCA())) {
                    .minDistPCA(inputx = input$PCAhover$x, 
                                inputy = input$PCAhover$y,
                                valuesx = .valuesPCA()[,1], 
                                valuesy = .valuesPCA()[,2])
                }
            )
            
            output$hoverProtPCA <- renderText(
                featureNames(.dI())[minDist2dProtPCAHover()]
            )
            
            ## Multiple points list
            ## Create a list-like object with reactive values
            .protPCA <- reactiveValues(mult=NULL)
            ## observe and concatenate new indices to .protPCA$mult
            observe({
                ## will be empty initially
                if(!is.null(input$PCAclick)) {
                    isolate({
                        .protPCA$mult <- c(.protPCA$mult, minDist2dProtPCA())
                        ## remove indices when indices are clicked another time
                        if (length(which(as.vector(table(.protPCA$mult)) > 1))) 
                            .protPCA$mult <- 
                                .protPCA$mult[
                                    -which(.protPCA$mult 
                                        == names(which(table(.protPCA$mult) > 1)))
                                              ]
                    })   
                }
            }) 
            ## END: PCA PLOT ##

            ## TAB: PLOTDIST ##
            
            ## reactive expressions for plotDist
            
            ## Index of element in list where parameters are stored
            .nCol <- reactive({
                if (is.null(input$numberPlotDist) || 
                        input$quantityPlotDist == "1")
                    1
                else
                    as.numeric(input$numberPlotDist)
            })
            
            ## list where parameters for plot are stored
            ## create a list with reactive values
            .listParams <- reactiveValues(
                levPlotDist = NULL, 
                levPlotDistOrg = NULL
            )
            
            ## write paramters to list for plotDist at index of .nCol()
            observe({
                if (!is.null(input$organelleAll) &&
                        !is.null(input$fNamesplDist)) {
                    .listParams$levPlotDist[.nCol()] <- 
                        input$fNamesplDist
                    .listParams$levPlotDistOrg[.nCol()] <- 
                        input$organelleAll
                }
            })
            
            ## calculate protein nearest to user input
            .minDistProtPlotDist <- reactive(
                if (!is.null(input$plotDistclick)) { 
                    if (input$plotDistclick$x < (nrow(pData(.dI())) + .3) &&
                        input$plotDistclick$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                        .minDistPlotDist(data = .dI(), 
                                marker = .listParams$levPlotDist[1],
                                org = .listParams$levPlotDistOrg[1],
                                inputx = input$plotDistclick$x,
                                inputy = input$plotDistclick$y
                    )
                }
            )
                        
            .minDistProtPlotDistHover <- reactive({
                if (!is.null(input$plotDisthover$x)) {
                    if (input$plotDisthover$x < (nrow(pData(.dI())) + .3) &&
                        input$plotDisthover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1") 
                        .minDistPlotDist(data = .dI(),
                                marker = .listParams$levPlotDist[1],
                                org = .listParams$levPlotDistOrg[1],
                                inputx = input$plotDisthover$x,
                                inputy = input$plotDisthover$y
                        )
                }
            })
            
            output$hoverProtPlotDist <- renderText(
                featureNames(.dI())[.minDistProtPlotDistHover()]
            )
            
            ## Multiple points list
            ## Create a list-like object with reactive values
            .protPlotDist <- reactiveValues(mult=NULL)
            
            ## observe and add new points to prot.plotDist$mult
            observe({
                ## will be empty initially
                if(!is.null(input$plotDistclick)) {
                    isolate({
                        .protPlotDist$mult <-
                            c(.protPlotDist$mult, .minDistProtPlotDist())
                        ## remove indices when indices are double-clicked
                        if (length(which((as.vector(table(.protPlotDist$mult)) > 1))))
                            .protPlotDist$mult <- 
                                .protPlotDist$mult[-which(.protPlotDist$mult
                                    == names(which(table(.protPlotDist$mult) > 1)))]
                    })
                }
            }) 
            
            ## for Plot/Download button (needs a reactive expression)
            .plotDistReac <- reactive(
                .plotPlotDist(data = .dI(), 
                            levPlotDist = .listParams$levPlotDist,
                            levPlotDistOrg = .listParams$levPlotDistOrg,
                            quantity = input$quantityPlotDist,
                            sI = .searchInd()
                )
            )
            
            ## organelle for all name
            .organelleAllName <- reactive(
                if (!is.null(input$fNamesplDist))
                    if (input$fNamesplDist != "all")
                        names(table(fData(.dI())[input$fNamesplDist]))
                    else
                        "all"
            )
            
            output$allOrganellesUI <- renderUI(
                if(!is.null(.dI()))
                    selectInput("fNamesplDist",
                                "feature(s) in",
                                choices = c("all", fvarLabels(.dI())) 
                    )
            )
            
            output$organelleAllUI <- renderUI(
                if(!is.null(.organelleAllName()) &&
                       !is.null(input$fNamesplDist))
                    selectInput("organelleAll",
                                "assigned to",
                                choices = .organelleAllName()
                    )
            )
            
            output$numberPlotDistUI <- renderUI({
                if (!as.numeric(input$quantityPlotDist) == 1) {
                    sliderInput("numberPlotDist",
                                "Selected plot",
                                min = 1,
                                max = as.numeric(input$quantityPlotDist), 
                                value = 1,
                                step = 1)
                }
            })
            
            output$plotdist <- renderPlot(
                ## if(!is.null(.plotPlotDist()))
                .plotPlotDist(data = .dI(), 
                        levPlotDist = .listParams$levPlotDist,
                        levPlotDistOrg = .listParams$levPlotDistOrg,
                        quantity = input$quantityPlotDist,
                        sI = .searchInd()
                )
            )
            
            
            output$plotDistDownload <- downloadHandler(
                filename = function() {
                    paste(input$data, "-", "plotDist", "-", 
                          Sys.Date(), '.jpg', sep='')
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    print(.plotDistReac())
                    dev.off()
                }
            )
            ## END: PLOTDIST ##
            
            
            
            ## TAB: QUANTITATION DATA ##
            ## Generate the quantitation data
            output$exprsRadioUI <- renderUI({
                radioButtons("exprsRadio","Features",
                        choices = list("all or"="all", 
                            "selected"="selected"),
                        selected = ifelse(length(.searchInd()), 
                            "selected", "all")
                )
            })
            
            output$MSnExprs <- renderDataTable({
                if (input$exprsRadio == "all")
                    as.data.frame(
                        ## cbind to display data properly
                        cbind(
                            " " = row.names(exprs(.dI())),
                            exprs(.dI())
                        )
                    )
                else
                    as.data.frame(
                        ## cbind to display data properly
                        cbind(
                            " " = row.names(exprs(.dI()[.searchInd()])),
                            exprs(.dI()[.searchInd()])
                        )
                    )
            })
            ## END: QUANTITATION DATA ##
            
            
            
            ## TAB: FEATURE META-DATA ##
            ## Generate the feature meta-data
            output$fDataRadioUI <- renderUI({
                radioButtons("fDataRadio","Features",
                        choices=list("all or" = "all", 
                            "selected" = "selected"),
                        selected = ifelse(length(.searchInd()), 
                            "selected", "all")
                )
            })
            
            output$MSnfData <- renderDataTable({
                if (input$fDataRadio == "all")
                    as.data.frame(
                        ## cbind to display data properly
                        cbind(
                            " " = row.names(fData(.dI())),
                            fData(.dI())
                        )
                    )
                else
                    as.data.frame(
                        ## cbind to display data properly
                        cbind(
                            " " = row.names(fData(.dI()[.searchInd()])),
                            fData(.dI()[.searchInd()])
                        )
                    )
            })
            ## END: FEATURE META-DATA ##
            
            
            
            ## TAB: SAMPLE META-DATA ##
            ## Generate the sample meta-data
            output$MSnpData <- renderDataTable(
                as.data.frame(
                    ## cbind to display data properly
                    cbind(
                        " " = row.names(pData(.dI())),
                        pData(.dI())
                    )
                )
            )
            ## END: SAMPLE META-DATA ##
            
            
            
            ## TAB: SEARCH ##
            observe({
                if (length(.pR_SR$foi) > 0)
                    on.exit(assign("pRolocGUI_SearchResults",
                            .pR_SR$foi, .GlobalEnv)
                    )
            })
            
            .pR_SR <- reactiveValues(foi = sr)
            
            if ((is.null(sr))) {
                .pR_SR$foi <- FoICollection()
            }
            
            observe({
                newFOI <- .newfoi$ind
                if (input$saveLists2SR > 0
                    && !is.null(input$saveLists2SR)
                        && length(.searchInd()) > 0) {
                    isolate(
                        if (!(input$savedSearchText %in% 
                            description(.pR_SR$foi))) {
                            .pR_SR$foi <- addFeaturesOfInterest(
                                            newFOI, .pR_SR$foi)
                        }
                    )
                }
            })
            
            ## text field to assign name to search results
            ## display information about selected FoI
            output$infoSavedSearch <- renderText({
                if (!is.null(.pR_SR$foi) && length(.pR_SR$foi) != 0) {
                    showFOI <- .showFOI(.pR_SR$foi, .dI(), .whichN())
                    paste0(showFOI, sep = "\n", collapse = "")
                } else
                    return("pRolocGUI_SearchResults not found in workspace")
            })
            
            .tagsList <- reactive({description(.pR_SR$foi)})
            
            .whichN <- reactive({
                which(
                    input$tagSelectList ==
                        description(.pR_SR$foi)
                 )[1]
            })
            
            .whichNamesFOI <- reactive({
                which(
                    match(
                        rownames(.dI()),
                        .fnamesFOI(.pR_SR$foi)[[.whichN()]]
                    ) != "NA"
                )
            })
            
            ## select Input for the tag names of the list
            output$tagsListSearchResultUI <- renderUI(
                if(length(.pR_SR$foi) != 0)
                    selectInput("tagSelectList",
                            "Select search result",
                            choices = .tagsList()
                    )
            )
            
            output$savedSearchTextUI <- renderUI(
                textInput("savedSearchText",
                            "Description",
                            value="new search result"
                )
            )
            
            ## action button to save new FoIs
            output$saveLists2SRUI <- renderUI({
                ## actionButton will only appear when 
                ## there is a description and features are selected
                if (nchar(input$savedSearchText) != 0 && 
                    length(.searchInd()) != 0) {
                    if(!(input$savedSearchText %in%
                        description(.pR_SR$foi)) ||
                            length(.pR_SR$foi) == 0)
                        actionButton("saveLists2SR",
                                "Create new features of interest")
                else
                    return("name already exists, choose another name")
                }
            })
                        
            .newfoi <- reactiveValues(ind = NULL)
            
            observe({
                input$saveLists2SR
                sI <- isolate(.searchInd())
                searchText <- isolate(input$savedSearchText)
                dataInput <- isolate(.dI())
                if (!is.null(searchText)
                    && !is.null(dataInput)
                        && !is.null(sI))
                    .newfoi$ind <- FeaturesOfInterest(
                        description = searchText,
                        fnames = featureNames(dataInput)[sI],
                        object = dataInput)
            })
            ### END: SEARCH ###
        
        } ## end server function        
    ) ## end list 
    runApp(app)
} ## end function

