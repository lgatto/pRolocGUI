#' @export 
pRolocComp <- function(object = list(tan2009r1 = tan2009r1, tan2009r2 = tan2009r2)) {
    
    if (!listOf(object, "MSnSet"))
        stop("object not list of MSnSets")
    if (length(object) != 2)
        stop("attempt to upload list of length unequal to 2")
    
    ## increase upload limit to 20 MB
    options(shiny.maxRequestSize = 20*1024^2)
    
    ## pRolocGUI_SearchResults
    sr <- .createSR()
    
    app <- list(
        ui = bootstrapPage(
                fluidRow( # was fluidPage
                    ## Application title
                    .pRn2_setTitlePanel(),
                    ## Sidebar Panel
                    sidebarPanel(
                        .pR_tags(),
                        .pRn2_tags(),
                        .pR_condDisplaySelection(),
                        .pRn2_selObj(),
                        .pRn2_condTabPCA(),
                        .pR_condTabProteinProfiles(),
                        .pR_condTabQuantitation(),
                        .pR_condTabfData(),
                        .pR_condTabpData(),
                        .pR_condTabSearch(),
                        .pR_condTabComp(),
                        width = 2
                        ),
                    ## Main Panel
                    mainPanel(
                        tabsetPanel(
                            .pRn2_tabPanelPCA(),
                            .pRn2_tabPanelProteinProfiles(),
                            .pR_tabPanelQuantitation(),
                            .pR_tabPanelfData(),
                            .pR_tabPanelpData(),
                            .pR_tabPanelSearch(),
                            .pR_tabPanelComp(),
                            id = "tab1" 
                        )#,
                       # width = 9
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
            
            
            
            ## create reactive Values to store object1 and object2 or a subset
            ## of these two objects with common features    
            data <- reactiveValues(obj = object)
            observe({
                if (!is.null(input$commonFeat)) {
                    if (input$commonFeat == "common & unique")
                        data$obj <- object
                    else {
                        inter <- intersect(rownames(object[[1]]), rownames(object[[2]]))
                    if (input$commonFeat == "unique")
                        data$obj <- list(
                            object[[1]][setdiff(rownames(object[[1]]), inter)],
                            object[[2]][setdiff(rownames(object[[2]]), inter)])
                    if (input$commonFeat == "common")                        
                        data$obj <- list(
                            object[[1]][rownames(object[[1]]) %in% inter],
                            object[[2]][rownames(object[[2]]) %in% inter])
                    }
                }
            })  
            
            ## START OF SEARCH IMPLEMENTATION ##
            
            ## check boxes by clicking on plots PCA and plotDist
            dSelect <- reactiveValues(PCA = NULL, plotDist = NULL, text = NULL, data = NULL)
            
            observe({
                dSelect$PCA <- .selClick(dSelect$PCA, input$PCA1click, 
                        .prot$PCA, TRUE, input$PCA2click)
            })

            
            observe({
                dSelect$plotDist <- .selClick(
                    dSelect$plotDist, input$plotDist1click, 
                    .prot$plotDist, FALSE, input$plotDist2click)
            })
            
            observe({
                dSelect$text <- .selText(
                    dSelect$text, input$saveText, input$resetMult, 
                    .prot$text
                )  
            })
            
            observe({
                dSelect$data <- NULL
            })
            
            output$checkBoxUI <- renderUI(
                .checkBoxdSelect(dSelect$PCA, dSelect$plotDist, dSelect$text, dSelect$data, TRUE)
            )
            
            ## reactive expression which contains all feature names for the 
            ## checkbox is selected
            .unionFeat <- reactive(
                .sI(cIS = input$chooseIdenSearch, 
                    tagSelectList = input$tagSelectList, 
                    protText = .prot$text, protPCA = .prot$PCA, 
                    protPlotDist = .prot$plotDist,  
                    protSearch = .fnamesFOI(.pR_SR$foi)[[.whichN()]]
                )
            )
            
            ## reactive expressions for general search
            ## indices are computed to forward to
            ## plot2D, plotDist and tabs quantitation
            ## and feature meta-data
            .searchInd1 <- reactive(.computeInd(data$obj, .unionFeat(), "object1"))
            .searchInd2 <- reactive(.computeInd(data$obj, .unionFeat(), "object2"))

            ## Clear multiple points on click
            observe({
                if (!is.null(input$resetMult))
                    if (input$resetMult > 0) {
                        .prot$PCA1 <- NULL
                        .prot$PCA2 <- NULL
                        .prot$PCA <- NULL
                        .prot$plotDist1 <- NULL
                        .prot$plotDist2 <- NULL
                        .prot$plotDist <- NULL
                        .prot$text <- NULL
                        .prot$data <- NULL
                        dSelect$PCA <- NULL
                        dSelect$plotDist <- NULL
                        dSelect$text <- NULL
                    }
            })
            
            ## action button to submit
            output$saveTextUI <- renderUI(
                if (!is.null(input$search))
                    if (!.checkFeatText(data$obj, .prot$text, input$sRTextInput, 
                                input$search, sel$Obj, name = TRUE))
                        actionButton("saveText", "Submit selection")
            )
            
            ## reset button
            output$resetMultUI <- renderUI(.reset(.searchInd1(), .searchInd2()))
            
            ## text-based search: protein und fvarLabels
            output$searchUI <- renderUI(.selVarText(data$obj, sel$Obj))
            
            output$searchResultsUI <- renderUI(
                    .selResText(input$search, .searchResultsText())
            )
            
            ## reactive expressions for text based search
            ## levels to search
            .searchResultsText <- reactive(
                .sRsubset(data$obj, input$search, input$levelSearch, sel$Obj)
            )
            
            ## vector with reactive values
            .prot <- reactiveValues(PCA1 = NULL, PCA2 = NULL, PCA = NULL, 
                        plotDist1 = NULL, plotDist2 = NULL, plotDist = NULL, 
                        text = NULL, data = NULL)
            
            ## observe indices and concatenate to .prot$PCA, .prot$plotDist
            ## and .prot$text
            observe(.prot$PCA1 <- .obsProtClick(
                        .prot$PCA1, minDist2dProt1PCA(), input$PCA1click))
            
            observe(.prot$PCA2 <- .obsProtClick(
                        .prot$PCA2, minDist2dProt2PCA(), input$PCA2click))
            
            observe(.prot$PCA <- c(.prot$PCA1, .prot$PCA2))
            
            observe(.prot$plotDist1 <- .obsProtClick(
                        .prot$plotDist1, .minPlotDist1(), input$plotDist1click))
            
            observe(.prot$plotDist2 <- .obsProtClick(
                        .prot$plotDist2, .minPlotDist2(), input$plotDist2click))
            
            observe(.prot$plotDist <- c(.prot$plotDist1, .prot$plotDist2))
            
            observe({
                if (!is.null(input$saveText)) {
                    .prot$text <- .obsProtText(
                        data$obj, .prot$text, input$saveText, 
                        isolate(input$sRTextInput), isolate(input$search), 
                        isolate(sel$Obj), names = TRUE)
                }
            })
            
            observe({
                .prot$Data <- NULL
            })
        
            ## END OF SEARCHING IMPLEMENTATION ##  
    
            
            
            ## START: TAB PCA ##
            
            ## colours  
            .params <- reactiveValues(
                colours = c("none", "none"), fcex = c(1, 1), 
                symbol = c("none", "none"), 
                PCAn1 = c(1, 1), PCAn2 = c(2, 2),
                xrange1 = c(min(.vPCA(isolate(data$obj), 1, 2, "object1")[, 1]), 
                            max(.vPCA(isolate(data$obj), 1, 2, "object1")[, 1])),
                xrange2 = c(min(.vPCA(isolate(data$obj), 1, 2, "object2")[, 1]), 
                            max(.vPCA(isolate(data$obj), 1, 2, "object2")[, 1])),
                yrange1 = c(min(.vPCA(isolate(data$obj), 1, 2, "object1")[, 2]), 
                            max(.vPCA(isolate(data$obj), 1, 2, "object1")[, 2])),
                yrange2 = c(min(.vPCA(isolate(data$obj), 1, 2, "object2")[, 2]), 
                            max(.vPCA(isolate(data$obj), 1, 2, "object2")[, 2])),
                legend = FALSE
            )
        
            observe({
                if (!is.null(input$fcolours)) {
                    .params$colours[.ind$params] <- input$fcolours}
                if (!is.null(input$fcex)) {
                    .params$fcex[.ind$params] <- input$fcex}
                if (!is.null(input$PCAn1) && !is.null(input$PCAn2)) {
                    .params$PCAn1[.ind$params] <- input$PCAn1
                    .params$PCAn2[.ind$params] <- input$PCAn2 
                }
                if (!is.null(input$fsymboltype)) {
                    .params$symbol[.ind$params] <- input$fsymboltype}
                if (!is.null(input$legendyes)) {
                    .params$legend <- input$legendyes}
                if (!is.null(input$xrange1) && !is.null(input$yrange1)) {
                    .params$xrange1 <- input$xrange1
                    .params$yrange1 <- input$yrange1
                }
                if (!is.null(input$xrange2) && !is.null(input$yrange2)) {
                    .params$xrange2 <- input$xrange2
                    .params$yrange2 <- input$yrange2
                }
            })
        
            ## reactive Values for object selected
            .ind <- reactiveValues(params = 1)
            observe({
                if (sel$Obj == "object1")
                    isolate(.ind$params <- 1)
                else
                    isolate(.ind$params <- 2)
            })
            
            ## values of PCA, dims is dependent on user input,
            ## so is xlim and ylim
            .valuesPCA1 <- reactive(.vPCA(data$obj, .params$PCAn1[1], 
                                    .params$PCAn2[1], "object1"))
       
            .valuesPCA2 <- reactive(.vPCA(data$obj, .params$PCAn1[2], 
                                    .params$PCAn2[2], "object2"))
                       
            ## selectInput for colours
            output$fcoloursOutput <- renderUI({ 
                .colourPCA(data$obj,
                    isolate(.params$colours[.ind$params]), sel$Obj)
            })
        
            ## compute number of principal components to look for 
            ## and change UI accordingly
            output$PCAn1UI <- renderUI(
                .PC(data$obj, "x", isolate(.params$PCAn1)[.ind$params], sel$Obj)
            )
        
            output$PCAn2UI <- renderUI(
                .PC(data$obj, "y", isolate(.params$PCAn2)[.ind$params], sel$Obj)
            )
        
            ## selectInput for point size
            output$fcexOutput <- renderUI(
                .fcexPCA(data$obj, input$fcolours,
                    isolate(.params$fcex)[.ind$params], sel$Obj)
            )
            
            ## selectInput for symboltype
            output$fsymboltypeOutput <- renderUI(
                .symbolPCA(data$obj, input$fcolours, 
                           isolate(.params$symbol)[.ind$params], sel$Obj)
            )
            
            ## zoom function: parameters for x- and y-range for PCA plot
            output$xrange1UI <- renderUI(if (sel$Obj == "object1")
                .rangePCA(.valuesPCA1(), 1, "xrange1"))  
            output$xrange2UI <- renderUI(if (sel$Obj == "object2")
                .rangePCA(.valuesPCA2(), 1, "xrange2"))
            output$yrange1UI <- renderUI(if (sel$Obj == "object1")
                .rangePCA(.valuesPCA1(), 2, "yrange1"))
            output$yrange2UI <- renderUI(if (sel$Obj == "object2")
                .rangePCA(.valuesPCA2(), 2, "yrange2"))
            
            ## legend
            output$PCALegendUI <- renderUI(
                .legendPCA(data$obj, .params$colours[.ind$params], .params$legend, 
                    sel$Obj)
            )
            
            output$PCALegendposUI <- renderUI(
                .legendPosPCA(data$obj, .params$colours[.ind$params], sel$Obj)
            )
            
            ## Plots and reactive expressions for download
            .PCA1 <- reactive(
                .plotPCA(obj = data$obj, 
                    fcolours = .params$colours[1], 
                    fcex = .params$fcex[1],
                    xrange = .params$xrange1,
                    yrange = .params$yrange1,
                    sb = .params$symbol[1],
                    PCAn1 = .params$PCAn1[1],
                    PCAn2 = .params$PCAn2[1],
                    legend = input$legendyes, 
                    legendpos = input$legendpos,
                    sI = .searchInd1(),
                    cIS = input$chooseIdenSearch,
                    ind = "object1")
            )
            
            output$PCA1 <- renderPlot(
                .plotPCA(obj = data$obj, 
                    fcolours = .params$colours[1], 
                    fcex = .params$fcex[1],
                    xrange = .params$xrange1,
                    yrange = .params$yrange1,
                    sb = .params$symbol[1],
                    PCAn1 = .params$PCAn1[1],
                    PCAn2 = .params$PCAn2[1],
                    legend = input$legendyes, 
                    legendpos = input$legendpos,
                    sI = .searchInd1(),
                    cIS = input$chooseIdenSearch,
                    ind = "object1")
            )            
            
            ## display 2D-nearest protein for obj1 in PCA plot
            output$hoverProt1PCA <- renderText(minDist2dProt1PCAHover())
            
            .PCA2 <- reactive(
                .plotPCA(obj = data$obj, 
                    fcolours = .params$colours[2], 
                    fcex = .params$fcex[2],
                    xrange = .params$xrange2,
                    yrange = .params$yrange2,
                    sb = .params$symbol[2],
                    PCAn1 = .params$PCAn1[2],
                    PCAn2 = .params$PCAn2[2],
                    legend = input$legendyes, 
                    legendpos = input$legendpos,
                    sI = .searchInd2(),
                    cIS = input$chooseIdenSearch,
                    ind = "object2")
            )
            
            ## display 2D-nearest protein for obj2 in PCA plot
            output$hoverProt2PCA <- renderText(minDist2dProt2PCAHover())
            
            output$PCA2 <- renderPlot(                
                .plotPCA(obj = data$obj, 
                    fcolours = .params$colours[2], 
                    fcex = .params$fcex[2],
                    xrange = .params$xrange2,
                    yrange = .params$yrange2,
                    sb = .params$symbol[2],
                    PCAn1 = .params$PCAn1[2],
                    PCAn2 = .params$PCAn2[2],
                    legend = input$legendyes, 
                    legendpos = input$legendpos,
                    sI = .searchInd2(),
                    cIS = input$chooseIdenSearch,
                    ind = "object2")
            )
            
            ## Download Handler for PCA plot
            output$plotPCA1Download <- downloadHandler(
                filename = function() {
                    paste('object1-PCA-', Sys.Date(), '.jpg', sep='')
                }, 
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCA1()
                    dev.off()
                }
            )
            
            output$plotPCA2Download <- downloadHandler(
                filename = function() {
                    paste('object2-PCA-', Sys.Date(), '.jpg', sep='')
                }, 
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCA2()
                    dev.off()
                }
            )
            
            ## compute name of 2D-nearest protein for obj1 in PCA plot (click)
            minDist2dProt1PCA <- reactive(
                ## will be empty initially
                if (!is.null(input$PCA1click) && !is.null(.valuesPCA1())) {
                    ## compute 2D distances from click input to each component 
                    ## of the PCA plot, input$PCAclick1$x and input$PCAclick1$y
                    ## is user input (name will be returned)
                    .minDistPCA(inputx = input$PCA1click$x, 
                           inputy = input$PCA1click$y,
                           valuesx = .valuesPCA1()[,1],
                           valuesy = .valuesPCA1()[,2],
                           name = TRUE)
                }
            )
            
            ## compute name of 2D-nearest protein for obj1 in PCA plot (hover)
            minDist2dProt1PCAHover <- reactive(
                if (!is.null(input$PCA1hover) && !is.null(.valuesPCA1())) {
                    .minDistPCA(inputx = input$PCA1hover$x, 
                            inputy = input$PCA1hover$y,
                            valuesx = .valuesPCA1()[,1], 
                            valuesy = .valuesPCA1()[,2],
                            name = TRUE)
                }
            )
                
            ## compute name of 2D-nearest protein for obj2 in PCA plot (click)
            minDist2dProt2PCA <- reactive(
            ## will be empty initially
                if (!is.null(input$PCA2click) && !is.null(.valuesPCA2())) {
                ## compute 2D distances from click input to each component 
                ## of the PCA plot, input$PCAclick2$x and input$PCAclick2$y
                ## is user input (name will be returned)
                    .minDistPCA(inputx = input$PCA2click$x, 
                            inputy = input$PCA2click$y,
                            valuesx = .valuesPCA2()[,1],
                            valuesy = .valuesPCA2()[,2],
                            name = TRUE)
                }
            )
            
            ## compute name of 2D-nearest protein for obj2 in PCA plot (hover)
            minDist2dProt2PCAHover <- reactive(
                if (!is.null(input$PCA2hover) && !is.null(.valuesPCA2())) {
                    .minDistPCA(inputx = input$PCA2hover$x, 
                            inputy = input$PCA2hover$y,
                            valuesx = .valuesPCA2()[,1], 
                            valuesy = .valuesPCA2()[,2],
                            name = TRUE)
                }
            )
            ## END: TAB PCA ## 



            ## START: TAB PLOTDIST ##
            
            ## Index of element in list where parameters are stored
            .nCol <- reactive(.nC(input$numberPlotDist, input$quantityPlotDist))

            ## list where parameters for plot are stored
            ## create a list with reactive values
            .listParams <- reactiveValues(
                                levPlotDist1 = "all", levPlotDist2 = "all",
                                levPlotDistOrg1 = "all", levPlotDistOrg2 = "all"
            )

            ## write paramters to list for plotDist at index of .nCol()
            observe({
                if (!is.null(sel$Obj) && !is.null(input$organelleAll) &&
                        !is.null(input$fNamesplDist)) {
                    if (sel$Obj == "object1") {
                        .listParams$levPlotDist1[.nCol()] <- 
                                input$fNamesplDist
                        .listParams$levPlotDistOrg1[.nCol()] <- 
                                input$organelleAll
                    } else {
                        .listParams$levPlotDist2[.nCol()] <- 
                                input$fNamesplDist
                        .listParams$levPlotDistOrg2[.nCol()] <- 
                                input$organelleAll
                    }
                }
            })

            ## calculate protein nearest to user input (click)
            .minPlotDist1 <- reactive(
                if (length(data$obj) != 0 && !is.null(input$plotDist1click)) { 
                    if (input$plotDist1click$x < (nrow(pData(data$obj[[1]])) + .3) &&
                        input$plotDist1click$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                    .minDistPlotDist(obj = data$obj, 
                            marker = .listParams$levPlotDist1[1],
                            org = .listParams$levPlotDistOrg1[1],
                            inputx = input$plotDist1click$x,
                            inputy = input$plotDist1click$y,
                            ind = "object1",
                            name = TRUE)[1]
                }
            )

            .minPlotDist2 <- reactive(
                if (length(data$obj) != 0 && !is.null(input$plotDist2click)) { 
                    if (input$plotDist2click$x < (nrow(pData(data$obj[[2]])) + .3) &&
                        input$plotDist2click$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                        .minDistPlotDist(obj = data$obj, 
                            marker = .listParams$levPlotDist2[1],
                            org = .listParams$levPlotDistOrg2[1],
                            inputx = input$plotDist2click$x,
                            inputy = input$plotDist2click$y,
                            ind = "object2",
                            name = TRUE)[1]
                }
            )
            
            ## calculate protein nearest to user input (hover) and display name 
            ## in tabPanel
            .minPlotDist1Hover <- reactive({
                if (length(data$obj) != 0 && !is.null(input$plotDist1hover$x)) {
                    if (input$plotDist1hover$x < (nrow(pData(data$obj[[1]])) + .3) &&
                        input$plotDist1hover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1") 
                        .minDistPlotDist(obj = data$obj,
                            marker = .listParams$levPlotDist1[1],
                            org = .listParams$levPlotDistOrg1[1],
                            inputx = input$plotDist1hover$x,
                            inputy = input$plotDist1hover$y,
                            ind = "object1",
                            name = TRUE)[1]
                }
            })

            output$hoverPlotDist1 <- renderText(.minPlotDist1Hover())

            .minPlotDist2Hover <- reactive({
                if (length(data$obj) != 0 && !is.null(input$plotDist2hover$x)) {
                    if (input$plotDist2hover$x < (nrow(pData(data$obj[[2]])) + .3) &&
                        input$plotDist2hover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                            input$quantityPlotDist == "1") 
                    .minDistPlotDist(obj = data$obj,
                            marker = .listParams$levPlotDist2[1],
                            org = .listParams$levPlotDistOrg2[1],
                            inputx = input$plotDist2hover$x,
                            inputy = input$plotDist2hover$y,
                            ind = "object1",
                            name = TRUE)[1]
                }
            })

            output$hoverPlotDist2 <- renderText(.minPlotDist2Hover())

            ## select fvarLabels or "all" for all features UI    
            output$allOrganellesUI <- renderUI(
                .featuresPlotDist(data$obj, sel$Obj))

            ## UI for feature levels in fvarLabels or "all"
            output$organelleAllUI <- renderUI(
                .flevelPlotDist(
                    .orgName(data$obj, input$fNamesplDist, sel$Obj),
                    input$fNamesplDist)
            )

            ## UI for quantity of plots to plot
            output$quantityPlotDistUI <- renderUI(.quantPlotDist(c(1:2), 1))

            ## UI for number of plots to plot            
            output$numberPlotDistUI <- renderUI(
                if (!is.null(input$quantityPlotDist))
                    .numPlotDist(input$quantityPlotDist)
            )

            ## plots and reactive expressions for download button
            .plotDist1 <- reactive(
                .plotPlotDist(obj = data$obj, 
                    levPlotDist = .listParams$levPlotDist1,
                    levPlotDistOrg = .listParams$levPlotDistOrg1,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd1(), ind = "object1")
            )
            
            output$plotDist1UI <- renderPlot(
                .plotPlotDist(obj = data$obj, 
                    levPlotDist = .listParams$levPlotDist1,
                    levPlotDistOrg = .listParams$levPlotDistOrg1,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd1(), ind = "object1")
            )

            
            .plotDist2 <- reactive(
                .plotPlotDist(obj = data$obj, 
                    levPlotDist = .listParams$levPlotDist2,
                    levPlotDistOrg = .listParams$levPlotDistOrg2,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd2(), ind = "object2")    
            )

            output$plotDist2UI <- renderPlot(
                .plotPlotDist(obj = data$obj, 
                    levPlotDist = .listParams$levPlotDist2,
                    levPlotDistOrg = .listParams$levPlotDistOrg2,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd2(), ind = "object2") 
            )

            ## Download Handler for PCA plot
            output$plotDist1Download <- downloadHandler(
                filename = function() {
                    paste("object1-plotDist-", Sys.Date(), ".jpg", sep="")
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .plotDist1()
                    dev.off()}
            )
            
            output$plotDist2Download <- downloadHandler(
                filename = function() {
                    paste('object2-plotDist-', Sys.Date(), '.jpg', sep='')
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .plotDist2()
                    dev.off()}
            )
            
            ## END: TAB PLOTDIST ## 

            
            ## START: TAB QUANTITATION ##
            output$exprsRadioUI <- renderUI(
                if (sel$Obj == "object1")
                    .radioButton(.searchInd1(), TRUE)
                else
                    .radioButton(.searchInd2(), TRUE)
            )
            
            output$MSnExprsUI <- renderDataTable(
                if (sel$Obj == "object1")
                    .dTable(data$obj, "quant", input$exprsRadio, 
                                .searchInd1(), "object1")
                else 
                    .dTable(data$obj, "quant", input$exprsRadio, 
                            .searchInd2(), "object2")                    
            )
            ## END: TAB QUANTITATION ##
            

            ## TAB: FEATURE META-DATA ##
            ## Generate the feature meta-data
            output$fDataRadioUI <- renderUI(
                if (sel$Obj == "object1")
                    .radioButton(.searchInd1(), FALSE)
                else
                    .radioButton(.searchInd2(), FALSE)
            )

            output$MSnfDataUI <- renderDataTable(
                if (sel$Obj == "object1")
                    .dTable(data$obj, "fD", input$fDataRadio, 
                                .searchInd1(), "object1")
                else 
                    .dTable(data$obj, "fD", input$fDataRadio, 
                                .searchInd2(), "object2")
            )
            ## END: FEATURE META-DATA ##

            

            ## TAB: SAMPLE META-DATA ##
            ## Generate the sample meta-data
            output$MSnpDataUI <- renderDataTable(
                .dTable(data$obj, "pD", ind = sel$Obj))
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
                .newfoi$ind <- .obsNewFoI(data$obj, .unionFeat(),            
                                    input$savedSearchText, 
                                    input$saveLists2SR, "object1", FALSE)
                .pR_SR$foi <- .obsSavedSearch(
                            .pR_SR$foi, .newfoi$ind, .unionFeat(), 
                            input$saveLists2SR, input$savedSearchText)
            }) 

            ## text field to assign name to search results
            ## display information about selected FoI
            .whichN <- reactive(.whichTag(input$tagSelectList, .pR_SR$foi))

            output$infoSavedSearchUI <- renderText({
                if (length(data$obj) != 0 
                    && !is.null(.pR_SR$foi) 
                        && length(.pR_SR$foi) != 0) {
                    showFOI <- .showFOI(.pR_SR$foi, data$obj, .whichN(), TRUE)
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
                .buttonSearch(.pR_SR$foi, .unionFeat(), input$savedSearchText)
            )
            ### END: SEARCH ###
            
        
            
            ### TAB: DATA ###     
            output$selObjUI <- renderUI(
                radioButtons("selObj", "", 
                    choices = .namesObj(object))
            )
            
            sel <- reactiveValues(Obj = "object1")
            observe({
                if (!is.null(input$selObj)) {
                if (input$selObj == .namesObj(object)[1])
                    sel$Obj <- "object1"
                else 
                    sel$Obj <- "object2"
                }
            })
            
            output$markerLevel1Output <- renderUI( 
                .colourPCA(data$obj, "none", "object1", "markerL1", "marker level 1")
            )
            
            output$markerLevel2Output <- renderUI(
                .colourPCA(data$obj, "none", "object2", "markerL2", "marker level 2")
            )
            
            output$selectMarker <- renderUI(
                if (!is.null(input$markerL1) && 
                            !is.null(input$markerL2)) {
                    if (input$markerL1 != "none"&& input$markerL2 != "none") {
                        selectInput("selectMarker", "select marker", 
                            choices = c("all",
                                .mC(data$obj, input$markerL1, input$markerL2)),
                            selected = "all")
                    } else
                        selectInput("selectMarker", "select marker",
                            choices = c("all"))
                }
            )
            
            output$dataComp <- renderText(.overview())
            
            .overview <- reactive({
                if(!is.null(input$markerL1) && !is.null(input$markerL2)) {
                if (input$markerL1 != "none" && input$markerL2 != "none")
                    cfn <- compfnames(data$obj[[1]], data$obj[[2]], input$markerL1, 
                                                input$markerL2, verbose=FALSE)
                else 
                    cfn <- list(compfnames(data$obj[[1]], data$obj[[2]],
                                                    NULL, NULL, verbose=FALSE))
                    
                    .ov <- .calcCompNumbers(cfn)
                .tableHTML(.ov, input$compRadio, input$selectMarker)
                
                }
            })
            
            output$saveDataUI <- renderUI(
                if (!is.null(input$selectMarker))
                   # if (!.checkFeatText(data$obj, .prot$text, input$sRTextInput, 
                   #                     input$search, sel$Obj, name = TRUE))
                        actionButton("saveData", "Submit selection")
            )
            
                
#                 if (!is.null(input$markerL1) && !is.null(input$markerL2))
#                     if (input$compRadio == "Overview") {
#                         if (input$markerL1 == "none" || 
#                                     input$markerL2 == "none") {
#                             paste0(capture.output(
#                                 compfnames(
#                                     data$obj[[1]], data$obj[[2]], NULL, NULL)),
#                                 sep = "\n", collapse = ""
#                            ) 
#                         } else {
#                             paste0(capture.output(
#                                 compfnames(data$obj[[1]], data$obj[[2]], 
#                                         input$markerL1, input$markerL2)),
#                                 sep = "\n", collapse = ""
#                             )
#                         }
#                     }
#            )
#             
#             output$dataCompTextUI <- renderUI(
#                 if (input$compRadio == "Overview")
#                     textOutput("dataComp")
#             )
#             
            
#             output$fDataCompFeatUI <- renderDataTable(
#                 if (input$compRadio != "Overview" &&
#                         !is.null(input$markerL1) && !is.null(input$markerL2))
#                     .namesCompFeat(data$obj[[1]], data$obj[[2]], 
#                             input$markerL1, input$markerL2, 
#                             input$selectMarker, input$compRadio)
#             )            
            ### END: DATA ###

    
        }
    )
    runApp(app)
    
    
}