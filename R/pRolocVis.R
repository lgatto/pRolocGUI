#'@name pRolocVis
#'@title pRolocVis
#'@export
#'@author Thomas Naake <tn299@@cam.ac.uk>
#'@usage pRolocVis(object = NULL)
#'@param object Pass a MSnSet to pRolocVis directly. Default \code{NULL} will
#'enable possibility to upload MSnSet in pRolocVis. 
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
    
    ## pRolocGUI_SearchResults
    sr <- .createSR()
        
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
                        .pRn1_condTabPCA(),
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
                            .pRn1_tabPanelPCA(),
                            .pRn1_tabPanelProteinProfiles(),
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
            output$linkDisplayUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#display",
                      "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
                
            output$linkDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPCAUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisPCA",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPPUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisPP",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkExprsUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisExprs",
                        "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
            
            output$linkfDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVisfData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkpDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocVis.html#tabspRolocVispData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkSearchUI <- renderUI({
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
            output$Data1UI <- renderUI({
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
            
            output$Data2UI <- renderUI({
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
            
            output$Data3UI <- renderUI({
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
                if (!is.null(input$data))
                        switch(input$data,
                            "andy2011" = list(andy2011),
                            "dunkley2006" = list(dunkley2006),
                            "tan2009r1" = list(tan2009r1),
                            "own data" = list(
                                    if (is.null(object)) {
                                        .dIownData()
                                    } else {
                                        if (inherits(object, "MSnSet"))
                                            object
                                        else
                                            andy2011
                                    }
                                )
                        )    
            })

            output$warningowndataUI <- renderText({
                    if (input$data == "own data") {
                        if (identical(.dI()[[1]], andy2011))
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
                dSelect$PCA <- .selClick(
                    dSelect$PCA, input$PCAclick, .prot$PCA, TRUE
                )
                dSelect$plotDist <- .selClick(
                    dSelect$plotDist, input$plotDistclick, 
                    .prot$plotDist, FALSE
                )
                dSelect$text <- .selText(
                    dSelect$text, input$saveText, input$resetMult, 
                    .prot$text
                )  
            })
     
            output$checkBoxUI <- renderUI(
                .checkBoxdSelect(dSelect$PCA, dSelect$plotDist, dSelect$text)
            )
                    
            ## reactive expressions for general search
            ## reactive expression to forward indices to 
            ## plot2D, plotDist and tabs quantitation
            ## and feature meta-data
            .searchInd <- reactive(
                    .sI(input$chooseIdenSearch, input$tagSelectList, .prot$text, 
                        .prot$PCA, .prot$plotDist, 
                        .whichFOI(.dI(), .pR_SR$foi, .whichN()))
            )
            
            ## reset button
            output$resetMultUI <- renderUI(.reset(.searchInd()))
            
            ## Clear multiple points on click
            observe({
                if (!is.null(input$resetMult))
                    if (input$resetMult > 0) {
                        .prot$PCA <- NULL
                        .prot$plotDist <- NULL
                        .prot$text <- NULL
                        dSelect$PCA <- NULL
                        dSelect$plotDist <- NULL
                        dSelect$text <- NULL
                    }
            })
            
            ## text-based search: protein und fvarLabels
            output$searchUI <- renderUI(.selVarText(.dI()))
                        
            output$searchResultsUI <- renderUI(
                .selResText(input$search, .searchResultsText())
            )
            
            ## reactive expressions for text based search
            ## levels to search
            .searchResultsText <- reactive(
                    .sRsubset(.dI(), input$search, input$levelSearch)
            )
            
            output$saveTextUI <- renderUI(
                if (!is.null(.dI()) && !is.null(input$search))
                    if (!.checkFeatText(.dI(), 
                            .prot$text, input$sRTextInput, input$search))
                        actionButton("saveText", "Submit selection")
            )
            
            
            ## vector with reactive values
            .prot <- reactiveValues(PCA = NULL, plotDist = NULL, text = NULL)
            
            ## observe indices and concatenate to .prot$PCA, .prot$plotDist
            ## and .prot$text
            observe({
                    .prot$PCA <- .obsProtClick(
                        .prot$PCA, minDist2dProtPCA(), input$PCAclick)
            })
            
            observe({
                .prot$plotDist <- .obsProtClick(.prot$plotDist, 
                        .minDistProtPlotDist(), input$plotDistclick)
            })
            
            observe({ 
                .prot$text <- .obsProtText(.dI(), .prot$text, input$saveText, 
                        isolate(input$sRTextInput), input$search)
            })

            ## END OF SEARCHING IMPLEMENTATION ##  
                        

            
            ## TAB: PCA PLOT ##  
            
            ## values of PCA, dims is dependent on user input,
            ## so is xlim and ylim
            .valuesPCA <- reactive(.vPCA(.dI(), input$PCAn1, input$PCAn2))
            
            ## render colour selectInput accordingly to fvarLabels()
            output$fcoloursUI <- renderUI(.colourPCA(.dI())) 
                
            ## render symboltype selectInput accordingly to fvarLabels
            output$fsymboltypeUI <- renderUI(.symbolPCA(.dI(), input$fcolours))
            
            ## render point size selectInput accordingly to .fcex
            output$fcexUI <- renderUI(.fcexPCA(.dI(), input$fcolours))
            
            ## zoom function: parameters for x- and y-range for PCA plot
            output$xrangeUI <- renderUI(.rangePCA(.valuesPCA(), 1, "xrange"))
                    
            output$yrangeUI <- renderUI(.rangePCA(.valuesPCA(), 2, "yrange"))
            
            ## compute number of principal components to look for 
            ## and change UI accordingly
            output$PCAn1UI <- renderUI(.PC(.dI(), "x", 1))
        
            output$PCAn2UI <- renderUI(.PC(.dI(), "y", 2))
            
            ## legend 
            output$PCALegendUI <- renderUI(.legendPCA(.dI(), input$fcolours))
            
            output$PCALegendposUI <- renderUI(
                .legendPosPCA(.dI(), input$fcolours)
            )
                
            ## Generate PCA plot, use fcolours for colours and add legend
            ## function (appearance and position dependent of user input)
            output$PCAUI <- renderPlot(
                    .plotPCA(obj = .dI(), 
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
                        cIS = input$chooseIdenSearch,
                        ind = "object1"
                    )
                )
            
            
            ## for Plot/Download button (needs a reactive expression)
            .PCAPlotReac <- reactive(
                    .plotPCA(obj = .dI(), 
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
                        cIS = input$chooseIdenSearch,
                        ind = "object1"
                )
            )
            
            ## Download Handler for PCA plot
            output$plotPCADownload <- downloadHandler(
                filename = function() {
                    paste(input$data, "-PCA-", Sys.Date(), ".jpg", sep="")
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCAPlotReac()
                    dev.off()}
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
                                valuesy = .valuesPCA()[,2],
                                name = FALSE)[1]
                }
            )
            
            minDist2dProtPCAHover <- reactive(
                if (!is.null(input$PCAhover) && !is.null(.valuesPCA())) {
                    .minDistPCA(inputx = input$PCAhover$x, 
                                inputy = input$PCAhover$y,
                                valuesx = .valuesPCA()[,1], 
                                valuesy = .valuesPCA()[,2],
                                name = TRUE)[1]
                }
            )
            
            ## display name of 2D-nearest protein in PCA plot
            output$hoverProtPCAUI <- renderText(minDist2dProtPCAHover())
            ## END: PCA PLOT ##

            ## TAB: PLOTDIST ##            
            ## Index of element in list where parameters are stored
            .nCol <- reactive(.nC(input$numberPlotDist, input$quantityPlotDist))
                
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
                if (length(.dI()) != 0 && !is.null(input$plotDistclick)) { 
                    if (input$plotDistclick$x < (nrow(pData(.dI()[[1]])) + .3) &&
                        input$plotDistclick$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                        .minDistPlotDist(obj = .dI(), 
                                marker = .listParams$levPlotDist[1],
                                org = .listParams$levPlotDistOrg[1],
                                inputx = input$plotDistclick$x,
                                inputy = input$plotDistclick$y,
                                name = FALSE)[1]
                }
            )
                        
            .minDistProtPlotDistHover <- reactive({
                if (length(.dI()) != 0 && !is.null(input$plotDisthover$x)) {
                    if (input$plotDisthover$x < (nrow(pData(.dI()[[1]])) + .3) &&
                        input$plotDisthover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1") 
                        .minDistPlotDist(obj = .dI(),
                                marker = .listParams$levPlotDist[1],
                                org = .listParams$levPlotDistOrg[1],
                                inputx = input$plotDisthover$x,
                                inputy = input$plotDisthover$y,
                                name = TRUE)[1]
                }
            })
            
            output$hoverProtPlotDistUI <- renderText(
                    .minDistProtPlotDistHover()
            )
            
            ## for Plot/Download button (needs a reactive expression)
            .plotDistReac <- reactive(
                    .plotPlotDist(obj = .dI(), 
                        levPlotDist = .listParams$levPlotDist,
                        levPlotDistOrg = .listParams$levPlotDistOrg,
                        quantity = input$quantityPlotDist,
                        sI = .searchInd()
                )
            )
            
            ## levels for plotDist to choose to plot
            .organelleAllName <- reactive(
                    .orgName(.dI(), input$fNamesplDist))             
            
            ## select fvarLabels or "all" for all features UI    
            output$allOrganellesUI <- renderUI(
                    .featuresPlotDist(.dI()))
            
            ## UI for feature levels in fvarLabels or "all"
            output$organelleAllUI <- renderUI(
                .flevelPlotDist(.organelleAllName(), input$fNamesplDist)
            )
            
            ## UI for quanity of plots to plot
            output$quantityPlotDistUI <- renderUI(.quantPlotDist(1:8, 1))
            
            ## UI for number of plots to plot            
            output$numberPlotDistUI <- renderUI(
                if (!is.null(input$quantityPlotDist))
                    .numPlotDist(input$quantityPlotDist)
            )
            
            ## renderPlot plotDist and assign parameters
            output$plotDistUI <- renderPlot(.plotDistReac()
#                     .plotPlotDist(obj = .dI(), 
#                         levPlotDist = .listParams$levPlotDist,
#                         levPlotDistOrg = .listParams$levPlotDistOrg,
#                         quantity = input$quantityPlotDist,
#                         sI = .searchInd()
               # )
            )
            
            
            output$plotDistDownload <- downloadHandler(
                filename = function() {
                    paste(input$data, "-plotDist-", Sys.Date(), ".jpg", sep="")
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .plotDistReac()
                    dev.off()}
                    
            )
            ## END: PLOTDIST ##
            
            
            
            ## TAB: QUANTITATION DATA ##
            ## Generate the quantitation data
            output$exprsRadioUI <- renderUI(.radioButton(.searchInd(), TRUE))

            output$MSnExprsUI <- renderDataTable(
                    .dTable(.dI(), "quant", input$exprsRadio, .searchInd())
            )
            ## END: QUANTITATION DATA ##
            
            
            
            ## TAB: FEATURE META-DATA ##
            ## Generate the feature meta-data
            output$fDataRadioUI <- renderUI(.radioButton(.searchInd(), FALSE))
            
            output$MSnfDataUI <- renderDataTable(
                    .dTable(.dI(), "fD", input$fDataRadio, .searchInd())
            )
            ## END: FEATURE META-DATA ##
            
            
            
            ## TAB: SAMPLE META-DATA ##
            ## Generate the sample meta-data
            output$MSnpDataUI <- renderDataTable(
                    .dTable(.dI(), "pD"))
            ## END: SAMPLE META-DATA ##
            
            
            
            ## TAB: SEARCH ##
            ## create object pRolocGUI_SearchResults in .GlobalEnv on exit
            observe({
                if (length(.pR_SR$foi) > 0)
                    on.exit(assign("pRolocGUI_SearchResults",
                            .pR_SR$foi, .GlobalEnv)
                    )
            })
            
            ## create reactiveValues for FoIColection
            .pR_SR <- reactiveValues(foi = sr)
            
            if ((is.null(sr))) {
                .pR_SR$foi <- FoICollection()
            }
            
            ## create reactiveValues for new features of Interest
            .newfoi <- reactiveValues(ind = NULL)
            
            observe({
                    .newfoi$ind <- .obsNewFoI(
                        .dI(), .searchInd(), 
                        input$savedSearchText, input$saveLists2SR
                    )
                    .pR_SR$foi <- .obsSavedSearch(
                        .pR_SR$foi, .newfoi$ind, .searchInd(), 
                        input$saveLists2SR, input$savedSearchText
                    )
            }) 
            
            ## text field to assign name to search results
            ## display information about selected FoI
            .whichN <- reactive(.whichTag(input$tagSelectList, .pR_SR$foi))
            
            output$infoSavedSearchUI <- renderText({
                if (length(.dI()) != 0 && !is.null(.pR_SR$foi) && length(.pR_SR$foi) != 0) {
                    showFOI <- .showFOI(.pR_SR$foi, .dI(), .whichN())
                    paste0(showFOI, sep = "\n", collapse = "")
                } else
                    return("pRolocGUI_SearchResults not found in workspace")
            })
            
            ## select Input for the tag names of the list
            output$tagsListSearchUI <- renderUI(.tagListSearch(.pR_SR$foi))
            
            ## text input to enter description 
            output$savedSearchTextUI <- renderUI(.textDescription())
                
            ## action button to save new FoIs
            output$saveLists2SRUI <- renderUI(
                .buttonSearch(.pR_SR$foi, .searchInd(), input$savedSearchText)
            )
            ### END: SEARCH ###
        
        } ## end server function        
    ) ## end list 
    runApp(app)
} ## end function

