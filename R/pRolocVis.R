#'@name pRolocVis/pRolocComp
#'@aliases pRolocVis
#'@aliases pRolocComp
#'@title pRolocVis/pRolocComp
#'@author Thomas Naake <naake@@stud.uni-heidelberg.de>
#'@usage pRolocVis(object)
#'@usage pRolocComp(object)
#'@param object an object of class \code{MSnSet} or a list of \code{MSnSet}s 
#'(pRolocVis), a list of length 2 of \code{MSnSet}s (pRolocComp).
#'@description \code{pRolocVis} and \code{pRolocComp} launch shiny sessions 
#'to interactively analyse and visualise proteomics data. 
#'
#'@details \code{pRolocVis} is a function to start a shiny session with 
#'one MSnSet data set or a list of \code{MSnSet}s. \code{pRolocComp} launches
#'with a list of two \code{MSnSet}s. 
#'
#'The functions allow to explore and analyse interactively spatial proteomics 
#'data, especially LOPIT and PCP experiments. Both functions offer high 
#'interactivity for exploring Principle Component Analysis (PCA) plots, 
#'protein profile plots and quantatative and qualitative meta-data. 
#'Additionally, \code{pRolocVis} and \code{pRolocComp} support import/export 
#'abilities for past and new search results using the 
#'\code{FeaturesOfInterest}/\code{FoICollection} infrastructure defined in the 
#'\code{MSnbase} package. 
#'
#'\code{pRolocVis} enables to analyse one \code{MSnSet} at a time, while
#'\code{pRolocComp} analyses and compares two \code{MSnSet}s. 
#'\code{pRolocComp} is especially meant for analyses of data which looks 
#'at the change of proteins in protein localisation.
#'
#'To load the vignette for the functions \code{pRolocVis} and \code{pRolocGUI}
#'enter \code{vignette("pRolocGUI")} in the console. The vignette will give more 
#'information on how to use the shiny applications.
#'
#'@examples \dontrun{
#'
#'## load MSnSet data sets from the pRolocdata package
#'data(andy2011, package = "pRolocdata")
#'data(tan2009r1, package = "pRolocdata")
#'data(tan2009r2, package = "pRolocdata")
#'data(dunkley2006, package = "pRolocdata")
#'
#'## create lists with unnamed and named objects
#'unnamedVis <- list(andy2011, tan2009r1, dunkley2006)
#'namedVis <- list(andy2011 = andy2011, tan2009r1 = tan2009r1, dunkley2006 = dunkley2006)
#'unnamedComp <- list(tan2009r1, tan2009r2)
#'namedComp <- list(tan2009r1 = tan2009r1, tan2009r2 = tan2009r2)
#'
#'## launch application by either assigning a MSnSet, an unnamed or a 
#'## named list to the argument object
#'pRolocVis(object = andy2011)
#'pRolocVis(object = unnamedVis)
#'pRolocVis(object = namedVis)
#'
#'pRolocComp(object = unnamedComp)
#'pRolocComp(object = namedComp)
#'}
#'@return An object \code{pRolocGUI_SearchResults} of class \code{FoICollection}
#'when the object existed already or when a new \code{FoICollection} was
#'created during a session. 
#'@export
pRolocVis <- function(object) {    

    ## global
    if (is.list(object)) {
        if (!listOf(object, "MSnSet"))
            stop("object not list of MSnSets")
    } else {
        name <- MSnbase:::getVariableName(match.call(), "object")
        if (!inherits(object, "MSnSet"))
            stop("object not of class MSnSet")
    }
    
    ## increase upload limit to 20 MB
    options(shiny.maxRequestSize = 20*1024^2)
    
    ## pRolocGUI_SearchResults
    sr <- .createSR()
        
    app <- list(  
        ui = 
            bootstrapPage( 
                fluidRow(
                    #responsive = TRUE,
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
                        width = 2
                    ),
                    ## Main Panel
                    mainPanel(
                        tabsetPanel(
                            .pRn1_tabPanelPCA(),
                            .pRn1_tabPanelProteinProfiles(),
                            .pR_tabPanelQuantitation(),
                            .pR_tabPanelfData(),
                            .pR_tabPanelpData(),
                            .pR_tabPanelSearch(),
                            .pRn1_tabPanelData(),
                            id = "tab1"
                        )#,
                        #width = 10
                    )
                )
            ),
        
        server = function(input, output) {   
            ## START: links to vignette ## 
            vignette <- system.file("doc/pRolocGUI.html", package="pRolocGUI")  
                
            if (nchar(vignette))
                addResourcePath(prefix = "doc", 
                    directoryPath = system.file("doc", package = "pRolocGUI"))
                
            ## Links to vignette ##
            output$linkDisplayUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#display",
                      "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
                
            output$linkDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIDataVis",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPCAUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIPCA",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
            
            output$linkPPUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIPP",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkExprsUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIExprs",
                        "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
            
            output$linkfDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIfData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkpDataUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUIpData",
                        "?", target="_blank")
                ##class = c("btn", "action-button"))
            })
                
            output$linkSearchUI <- renderUI({
                if (nchar(vignette))
                    a(href="/doc/pRolocGUI.html#tabspRolocGUISearch",
                        "?", target="_blank")
                    ##class = c("btn", "action-button"))
            })
        
            ## END: Links to vignette ## 
            
            ## TAB: DATA/UPLOAD ##
            
            ## choose Data source
            output$Data1UI <- renderUI(
                selectInput("data", "Choose MSnSet data source:",
                                    choices = .namesObj(object, name, upload = TRUE))     
            )

            .dIownData <- reactive({
                if (length(as.character(input$upload["datapath"]))) {
                    ## check if MSnSet has ending .rda or .RData and if 
                    ## it is MSnSet
                    if (file_ext(input$upload["name"]) %in% c("rda","RData") 
                        && inherits(
                            get(load(as.character(input$upload["datapath"]))), 
                                "MSnSet"))
                        od <- get(load(as.character(input$upload["datapath"])))
                 }
            })
            
            ## .dI (data Input)
            .dI <- reactive({
                ## initially when tab data was not loaded
                if (is.null(input$data)) {
                    if (is.list(object))
                        .dI <- list(object[[1]])
                    else
                        .dI <- list(object)
                } else { ## after tab data was loaded
                    .lenObject <- length(.namesObj(object, name, upload = TRUE))
                    .indObject <- which(input$data == .namesObj(object, name, upload=TRUE))
                    ## upload
                    if (.lenObject == .indObject) {
                        if (inherits(.dIownData(), "MSnSet"))
                            .dI <- list(.dIownData())
                        else {
                            if (is.list(object))
                                .dI <- list(object[[1]])
                            else
                                .dI <- list(object)
                        }
                    } else {## object          
                        if (is.list(object))
                            .dI <- list(object[[.indObject]])
                        else
                            .dI <- list(object)
                    }
                    .dI
                } 
            })

            output$warningUploadUI <- renderUI({
                if (!is.null(input$data)) {
                    if (input$data == "upload" 
                        && !length(as.character(input$upload["datapath"])))
                        strong(span("no file selected", style = "color:red"),
                            ", first element of object will be used")
                    else
                        if (input$data == "upload" && 
                            !inherits(.dIownData(), "MSnSet"))
                            strong(span("no valid MSnSet selected", 
                                                        style = "color:red"), 
                                ", first element of object will be used")
                }
            })  
            
            ## reset reactiveValues when the .dI() changes
            observe({
                .dI()
                .prot$text <- NULL
                .prot$PCA <- NULL
                .prot$plotDist <- NULL
            })
            ## END: UPLOAD ##
            
            
            
            ## START OF SEARCH IMPLEMENTATION ##
            
            ## check boxes by clicking on plots PCA and plotDist
            dSelect <- reactiveValues(PCA = NULL, plotDist = NULL, SaSe = NULL, 
                                      text = NULL)
            
             observe({
                dSelect$PCA <- .selClick(
                    dSelect$PCA, input$PCAclick, .prot$PCA, TRUE
                )
                dSelect$plotDist <- .selClick(
                    dSelect$plotDist, input$plotDistclick, 
                    .prot$plotDist, FALSE
                )
                dSelect$SaSe <- ifelse(input$selCB > 0, "savedSearches", NULL)
                dSelect$text <- .selButton(
                    dSelect$text, input$saveText, input$resetMult, 
                    .prot$text
                )  
            })
     
            output$checkBoxUI <- renderUI(
                .checkBoxdSelect(dSelect$PCA, dSelect$plotDist, dSelect$SaSe, dSelect$text)
            )
                    
            ## reactive expression to forward indices to plot2D, plotDist and 
            ## tabs quantitation and feature meta-data
            .searchInd <- reactive(
                    .sI(input$chooseIdenSearch, input$tagSelectList, .prot$text, 
                        .prot$PCA, .prot$plotDist, .indSavedSearch())
                       # .whichFOI(.dI(), .pR_SR$foi, .whichN()))
            )
            
            ## reset button
            output$resetMultUI <- renderUI(.reset(.searchInd()))
            
            ## Clear multiple points on click
            observe({
                if (!is.null(input$resetMult))
                    if (input$resetMult > 0) {
                        .prot$PCA <- .prot$plotDist <- .prot$text <- NULL
                        dSelect$PCA <- dSelect$plotDist <- dSelect$text <- NULL
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
                        
            ## action button to submit features (query)
            output$saveTextUI <- renderUI(
                if (!is.null(.dI()) 
                    && !is.null(input$search) 
                        && length(.searchResultsText()) >= 1)
                    if (!.checkFeatText(.dI(), 
                            .prot$text, input$sRTextInput, input$search))
                        actionButton("saveText", "Submit selection")
            )
            
            ## action button to remove (query)
            output$removeTextUI <- renderUI(
                if (!is.null(input$search))
                    if (.checkFeatText(
                            .dI(), .prot$text, input$sRTextInput, input$search))
                        actionButton("removeText", "Undo selection")
            )
            
            
            ## vector with reactive values
            .prot <- reactiveValues(PCA = NULL, plotDist = NULL, text = NULL)
            
            ## observe indices and concatenate to .prot$PCA, .prot$plotDist
            ## and .prot$text
            observe({
                    .prot$PCA <- .obsProtClick(
                        .prot$PCA, .minDist2dProtPCA(), input$PCAclick)
            })
            
            observe({
                .prot$plotDist <- .obsProtClick(.prot$plotDist, 
                        .minDistProtPlotDist(), input$plotDistclick)
            })
            
            observe({ 
                .prot$text <- .obsProtText(.dI(), .prot$text, input$saveText, 
                        isolate(input$sRTextInput), input$search)
            })
            
            observe({
                if (!is.null(input$removeText))
                    isolate(.prot$text <- .removeFeat(
                        .prot$text, .obsProtText(
                            .dI(), .prot$text, input$saveText, 
                            isolate(input$sRTextInput), input$search, 
                            add = FALSE), 
                        input$removeText))
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
                        ind = "object1",
                        listSaSe = .indSavedSearchlist()
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
                        ind = "object1",
                        listSaSe = .indSavedSearchlist()
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
            .minDist2dProtPCA <- reactive(
                ## will be empty initially
                if (!is.null(input$PCAclick) && !is.null(.valuesPCA())) {
                    ## compute 2D distances from click input to each component 
                    ## of the PCA plot, input$PCAclick$x and input$PCAclick$y
                    ## is user input (index will be returned)
                    .minDistPCA(inputx = input$PCAclick$x, 
                                inputy = input$PCAclick$y,
                                valuesx = .valuesPCA()[,1],
                                valuesy = .valuesPCA()[,2],
                                name = FALSE)
                }
            )
            
            .minDist2dProtPCAHover <- reactive(
                if (!is.null(input$PCAhover) && !is.null(.valuesPCA())) {
                    .minDistPCA(inputx = input$PCAhover$x, 
                                inputy = input$PCAhover$y,
                                valuesx = .valuesPCA()[,1], 
                                valuesy = .valuesPCA()[,2],
                                name = FALSE)
                }
            )
            
            ## display name of 2D-nearest protein in PCA plot
            output$hoverProtPCAUI <- renderTable(
                if (!is.null(.minDist2dProtPCAHover()))
                    fData(.dI()[[1]])[.minDist2dProtPCAHover(), ]
            )
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
                    if (!is.null(input$quantityPlotDist) && 
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
                    if (!is.null(input$quantityPlotDist) && 
                            input$quantityPlotDist == "1") 
                        .minDistPlotDist(obj = .dI(),
                                marker = .listParams$levPlotDist[1],
                                org = .listParams$levPlotDistOrg[1],
                                inputx = input$plotDisthover$x,
                                inputy = input$plotDisthover$y,
                                name = FALSE)[1]
                }
            })
            
            ## display name of 2D-nearest protein in plotDist plot
            output$hoverProtPlotDistUI <- renderTable(
                if (!is.null(.minDistProtPlotDistHover()))
                    fData(.dI()[[1]])[.minDistProtPlotDistHover(),]
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
            
            ## for Plot/Download button (needs a reactive expression)
            .plotDistReac <- reactive(
                .plotPlotDist(obj = .dI(), 
                    levPlotDist = .listParams$levPlotDist,
                    levPlotDistOrg = .listParams$levPlotDistOrg,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd()
                )
            )
            
            ## renderPlot plotDist and assign parameters
            output$plotDistUI <- renderPlot(
                .plotPlotDist(obj = .dI(), 
                    levPlotDist = .listParams$levPlotDist,
                    levPlotDistOrg = .listParams$levPlotDistOrg,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd()
               )
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
            
            selected <- reactiveValues(SaSe = NULL)
            observe(selected$SaSe <- input$selCB)
            
            output$multSaSe <- renderUI({
                if (length(.pR_SR$foi) > 0) 
                    selectInput(inputId = "selCB", label = "display", 
                        choices = description(.pR_SR$foi), multiple = TRUE,
                        selected = selected$SaSe)
            })
            
            .indSavedSearchlist <- reactive({
                .indSR <- na.omit(match(input$selCB, description(.pR_SR$foi)))
                .protNames <- lapply(foi(.pR_SR$foi)[.indSR], foi)
                lapply(.protNames, match, featureNames(object))
            })
            
            .indSavedSearch <- reactive({
                .protInd <- unique(unlist(.indSavedSearchlist()))
                .protInd[!is.na(.protInd)]
            })
            
            output$infoSavedSearchUI <- renderText({
                if (length(.dI()) != 0 && !is.null(.pR_SR$foi) && 
                        length(.pR_SR$foi) != 0 && !is.null(.whichN())) {
                    showFOI <- .showFOI(.pR_SR$foi, .dI(), .whichN(), FALSE)
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

