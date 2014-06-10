

pRolocComp <- function(obj1 = tan2009r1, obj2 = tan2009r2) {
    
    obj <- list(obj1, obj2)
    
    ## increase upload limit to 20 MB
    options(shiny.maxRequestSize = 20*1024^2)
    
    ## pRolocGUI_SearchResults
    sr <- .createSR()
    
    app <- list(
        ui = bootstrapPage(
                fluidPage(
                    ## Application title
                    .pRn2_setTitlePanel(),
                    ## Sidebar Panel
                    sidebarPanel(
                        .pR_tags(),
                        .pR_condDisplaySelection(),
                        .pRn2_selObj(),
                        .pRn2_condTabPCA(),
                        .pR_condTabProteinProfiles(),
                        width = 3
                        ),
                    ## Main Panel
                    mainPanel(
                        tabsetPanel(
                            .pRn2_tabPanelPCA(),
                            .pR_tabPanelProteinProfiles(),
                            id = "tab1" 
                        ),
                        width = 9
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
#                 dSelect$plotDist <- .selClick(
#                     dSelect$plotDist, input$plotDistclick, 
#                     .prot$plotDist, FALSE
#                 )
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
            .searchInd1 <- reactive(
                .sI(cIS = input$chooseIdenSearch, 
                    tagSelectList = input$tagSelectList, 
                    protText = .computeInd(obj, .prot$text, ind = "object1"), 
                    protPCA = .computeInd(obj, .prot$PCA, ind = "object1"),
                    protPlotDist = NULL, 
                    protSearch = NULL) 
            )
            
            .searchInd2 <- reactive(
                .sI(cIS = input$chooseIdenSearch, 
                    tagSelectList = input$tagSelectList, 
                    protText = .computeInd(obj, .prot$text, ind = "object2"), 
                    protPCA = .computeInd(obj, .prot$PCA, ind = "object2"),
                    protPlotDist = NULL, 
                    protSearch = NULL) 
            )
            
            ## Clear multiple points on click
            observe({
                if (!is.null(input$resetMult))
                    if (input$resetMult > 0) {
                        .prot$PCA1 <- NULL
                        .prot$PCA2 <- NULL
                        .prot$PCA <- NULL
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
                                    plotDist = NULL, 
                                    text1 = NULL, text2 = NULL, text = NULL)
            
            ## observe indices and concatenate to .prot$PCA, .prot$plotDist
            ## and .prot$text
            observe({
                .prot$PCA1 <- .obsProtClick(
                    .prot$PCA1, minDist2dProt1PCA(), input$PCA1click)
            })
            observe({
                .prot$PCA2 <- .obsProtClick(
                    .prot$PCA2, minDist2dProt2PCA(), input$PCA2click)
            })
            observe({
                .prot$PCA <- c(.prot$PCA1, .prot$PCA2)
            })
            observe({
                .prot$plotDist <- .obsProtClick(
                    .prot$plotDist, .minDistProtPlotDist(), input$plotDistclick)
            })
            
            observe({
                if (!is.null(input$saveText)) {
                    .prot$text <- .obsProtText(
                        obj, .prot$text, input$saveText, 
                        isolate(input$sRTextInput), isolate(input$search), 
                        isolate(input$selObj), names = TRUE)
                }
            })
        
            ## END OF SEARCHING IMPLEMENTATION ##  
            
            output$helpPCA <- renderText(c(.prot$PCA))
            
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
            
            ## Plots
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
                    ind = "object1" 
                )
            )
            
            ## display 2D-nearest protein for obj1 in PCA plot
            output$hoverProt1PCA <- renderText(minDist2dProt1PCAHover())
            
            output$PCA2 <- renderPlot(
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
                     ind = "object2"
                )
            )
            
            ## display 2D-nearest protein for obj2 in PCA plot
            output$hoverProt2PCA <- renderText(minDist2dProt2PCAHover())
            
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
            
        }
    )
    runApp(app)
    
    
}