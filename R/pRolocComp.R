

pRolocComp <- function(object1 = tan2009r1, object2 = tan2009r2) {
    
    obj <- list(object1, object2)
    
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
                        .pR_condDisplaySelection(),
                        .pRn2_selObj(),
                        .pRn2_condTabPCA(),
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
                            .pRn2_tabPanelPCA(),
                            .pRn2_tabPanelProteinProfiles(),
                            .pR_tabPanelQuantitation(),
                            .pR_tabPanelfData(),
                            .pR_tabPanelpData(),
                            .pR_tabPanelSearch(),
                            id = "tab1" 
                        )#,
                       # width = 9
                    )
                )        
            ),
        
        server = function(input, output) {
            ## START OF SEARCH IMPLEMENTATION ##
            
            ## check boxes by clicking on plots PCA and plotDist
            dSelect <- reactiveValues(PCA = NULL, plotDist = NULL, text = NULL)
            
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
            
            output$checkBoxUI <- renderUI(
                .checkBoxdSelect(dSelect$PCA, dSelect$plotDist, dSelect$text)
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
            .searchInd1 <- reactive(.computeInd(obj, .unionFeat(), "object1"))
            .searchInd2 <- reactive(.computeInd(obj, .unionFeat(), "object2"))

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
                        dSelect$PCA <- NULL
                        dSelect$plotDist <- NULL
                        dSelect$text <- NULL
                    }
            })
            
            ## action button to submit
            output$saveTextUI <- renderUI(
                if (!is.null(input$search))
                    if (!.checkFeatText(obj, .prot$text, input$sRTextInput, 
                                input$search, input$selObj, name = TRUE))
                        actionButton("saveText", "Submit selection")
            )
            
            ## reset button
            output$resetMultUI <- renderUI(.reset(.searchInd1(), .searchInd2()))
            
            ## text-based search: protein und fvarLabels
            output$searchUI <- renderUI(.selVarText(obj, input$selObj))
            
            output$searchResultsUI <- renderUI(
                    .selResText(input$search, .searchResultsText())
            )
            
            ## reactive expressions for text based search
            ## levels to search
            .searchResultsText <- reactive(
                .sRsubset(obj, input$search, input$levelSearch, input$selObj)
            )
            
            ## vector with reactive values
            .prot <- reactiveValues(PCA1 = NULL, PCA2 = NULL, PCA = NULL, 
                        plotDist1 = NULL, plotDist2 = NULL, plotDist = NULL, 
                        text = NULL)
            
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
                        obj, .prot$text, input$saveText, 
                        isolate(input$sRTextInput), isolate(input$search), 
                        isolate(input$selObj), names = TRUE)
                }
            })
        
            ## END OF SEARCHING IMPLEMENTATION ##  
    
            
            
            ## START: TAB PCA ##
            ## colours  
            .params <- reactiveValues(
                colours = c("none", "none"), fcex = c(1, 1), 
                symbol = c("none", "none"), 
                PCAn1 = c(1, 1), PCAn2 = c(2, 2),
                xrange1 = c(min(.vPCA(obj, 1, 2, "object1")[, 1]), 
                            max(.vPCA(obj, 1, 2, "object1")[, 1])),
                xrange2 = c(min(.vPCA(obj, 1, 2, "object2")[, 1]), 
                            max(.vPCA(obj, 1, 2, "object2")[, 1])),
                yrange1 = c(min(.vPCA(obj, 1, 2, "object1")[, 2]), 
                            max(.vPCA(obj, 1, 2, "object1")[, 2])),
                yrange2 = c(min(.vPCA(obj, 1, 2, "object2")[, 2]), 
                            max(.vPCA(obj, 1, 2, "object2")[, 2])),
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
                if (input$selObj == "object1")
                    isolate(.ind$params <- 1)
                else
                    isolate(.ind$params <- 2)
            })
            
            ## values of PCA, dims is dependent on user input,
            ## so is xlim and ylim
            .valuesPCA1 <- reactive(.vPCA(obj, .params$PCAn1[1], 
                                    .params$PCAn2[1], "object1"))
       
            .valuesPCA2 <- reactive(.vPCA(obj, .params$PCAn1[2], 
                                    .params$PCAn2[2], "object2"))
                       
            ## selectInput for colours
            output$fcoloursOutput <- renderUI({ 
                .colourPCA(obj,
                    isolate(.params$colours[.ind$params]), input$selObj)
            })
        
            ## compute number of principal components to look for 
            ## and change UI accordingly
            output$PCAn1UI <- renderUI(
                .PC(obj, "x", isolate(.params$PCAn1)[.ind$params], input$selObj)
            )
        
            output$PCAn2UI <- renderUI(
                .PC(obj, "y", isolate(.params$PCAn2)[.ind$params], input$selObj)
            )
        
            ## selectInput for point size
            output$fcexOutput <- renderUI(
                .fcexPCA(obj, input$fcolours,
                    isolate(.params$fcex)[.ind$params], input$selObj)
            )
            
            ## selectInput for symboltype
            output$fsymboltypeOutput <- renderUI(
                .symbolPCA(obj, input$fcolours, 
                           isolate(.params$symbol)[.ind$params], input$selObj)
            )
            
            ## zoom function: parameters for x- and y-range for PCA plot
            output$xrange1UI <- renderUI(if (input$selObj == "object1")
                .rangePCA(.valuesPCA1(), 1, "xrange1"))  
            output$xrange2UI <- renderUI(if (input$selObj == "object2")
                .rangePCA(.valuesPCA2(), 1, "xrange2"))
            output$yrange1UI <- renderUI(if (input$selObj == "object1")
                .rangePCA(.valuesPCA1(), 2, "yrange1"))
            output$yrange2UI <- renderUI(if (input$selObj == "object2")
                .rangePCA(.valuesPCA2(), 2, "yrange2"))
            
            ## legend
            output$PCALegendUI <- renderUI(
                .legendPCA(obj, .params$colours[.ind$params], .params$legend, 
                    input$selObj)
            )
            
            output$PCALegendposUI <- renderUI(
                .legendPosPCA(obj, .params$colours[.ind$params], input$selObj)
            )
            
            ## Plots and reactive expressions for download
            .PCA1 <- reactive(
                .plotPCA(obj = obj, 
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
                .plotPCA(obj = obj, 
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
            
            .PCA2reac <- reactive(
                .plotPCA(obj = obj, 
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
                .plotPCA(obj = obj, 
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
            
            ## Download Handler for PCA plot
            output$plotPCA1Download <- downloadHandler(
                filename = function() {
                    paste('object1-PCA-', Sys.Date(), '.jpg', sep='')
                }, 
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCA1reac()
                    dev.off()
                }
            )
            
            output$plotPCA2Download <- downloadHandler(
                filename = function() {
                    paste('object2-PCA-', Sys.Date(), '.jpg', sep='')
                }, 
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .PCA2reac()
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
                if (!is.null(input$selObj) && !is.null(input$organelleAll) &&
                        !is.null(input$fNamesplDist)) {
                    if (input$selObj == "object1") {
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
                if (length(obj) != 0 && !is.null(input$plotDist1click)) { 
                    if (input$plotDist1click$x < (nrow(pData(obj[[1]])) + .3) &&
                        input$plotDist1click$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                    .minDistPlotDist(obj = obj, 
                            marker = .listParams$levPlotDist1[1],
                            org = .listParams$levPlotDistOrg1[1],
                            inputx = input$plotDist1click$x,
                            inputy = input$plotDist1click$y,
                            ind = "object1",
                            name = TRUE)[1]
                }
            )

            .minPlotDist2 <- reactive(
                if (length(obj) != 0 && !is.null(input$plotDist2click)) { 
                    if (input$plotDist2click$x < (nrow(pData(obj[[2]])) + .3) &&
                        input$plotDist2click$x > 0.5 &&
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1")
                        .minDistPlotDist(obj = obj, 
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
                if (length(obj) != 0 && !is.null(input$plotDist1hover$x)) {
                    if (input$plotDist1hover$x < (nrow(pData(obj[[1]])) + .3) &&
                        input$plotDist1hover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                                input$quantityPlotDist == "1") 
                        .minDistPlotDist(obj = obj,
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
                if (length(obj) != 0 && !is.null(input$plotDist2hover$x)) {
                    if (input$plotDist2hover$x < (nrow(pData(obj[[2]])) + .3) &&
                        input$plotDist2hover$x > 0.5 && 
                            !is.null(input$quantityPlotDist) && 
                            input$quantityPlotDist == "1") 
                    .minDistPlotDist(obj = obj,
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
                .featuresPlotDist(obj, input$selObj))

            ## UI for feature levels in fvarLabels or "all"
            output$organelleAllUI <- renderUI(
                .flevelPlotDist(
                    .orgName(obj, input$fNamesplDist, input$selObj),
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
            .plotDist1reac <- reactive(
                .plotPlotDist(obj = obj, 
                    levPlotDist = .listParams$levPlotDist1,
                    levPlotDistOrg = .listParams$levPlotDistOrg1,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd1(), ind = "object1")
            )
            
            output$plotDist1UI <- renderPlot(
                .plotPlotDist(obj = obj, 
                    levPlotDist = .listParams$levPlotDist1,
                    levPlotDistOrg = .listParams$levPlotDistOrg1,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd1(), ind = "object1")
            )

            
            .plotDist2reac <- reactive(
                .plotPlotDist(obj = obj, 
                    levPlotDist = .listParams$levPlotDist2,
                    levPlotDistOrg = .listParams$levPlotDistOrg2,
                    quantity = input$quantityPlotDist,
                    sI = .searchInd2(), ind = "object2")    
            )

            output$plotDist2UI <- renderPlot(
                .plotPlotDist(obj = obj, 
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
                    .plotDist1reac()
                    dev.off()}
            )
            
            output$plotDist2Download <- downloadHandler(
                filename = function() {
                    paste('object2-plotDist-', Sys.Date(), '.jpg', sep='')
                },
                content = function(file) {
                    jpeg(file, quality = 100, width = 800, height = 800)
                    .plotDist2reac()
                    dev.off()}
            )
            
            ## END: TAB PLOTDIST ## 

            
            ## START: TAB QUANTITATION ##
            output$exprsRadioUI <- renderUI(
                if (input$selObj == "object1")
                    .radioButton(.searchInd1(), TRUE)
                else
                    .radioButton(.searchInd2(), TRUE)
            )
            
            output$MSnExprsUI <- renderDataTable(
                if (input$selObj == "object1")
                    .dTable(obj, "quant", input$exprsRadio, 
                                .searchInd1(), "object1")
                else 
                    .dTable(obj, "quant", input$exprsRadio, 
                            .searchInd2(), "object2")                    
            )
            ## END: TAB QUANTITATION ##
            

            ## TAB: FEATURE META-DATA ##
            ## Generate the feature meta-data
            output$fDataRadioUI <- renderUI(
                if (input$selObj == "object1")
                    .radioButton(.searchInd1(), FALSE)
                else
                    .radioButton(.searchInd2(), FALSE)
            )

            output$MSnfDataUI <- renderDataTable(
                if (input$selObj == "object1")
                    .dTable(obj, "fD", input$fDataRadio, 
                                .searchInd1(), "object1")
                else 
                    .dTable(obj, "fD", input$fDataRadio, 
                                .searchInd2(), "object2")
            )
            ## END: FEATURE META-DATA ##

            

            ## TAB: SAMPLE META-DATA ##
            ## Generate the sample meta-data
            output$MSnpDataUI <- renderDataTable(
                .dTable(obj, "pD", ind = input$selObj))
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
                .newfoi$ind <- .obsNewFoI(obj, .unionFeat(),            
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
                if (length(obj) != 0 
                    && !is.null(.pR_SR$foi) 
                        && length(.pR_SR$foi) != 0) {
                    showFOI <- .showFOI(.pR_SR$foi, obj, .whichN(), TRUE)
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

    
        }
    )
    runApp(app)
    
    
}