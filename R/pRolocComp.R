

pRolocComp <- function(obj1 = tan2009r1, obj2 = tan2009r2) {
    
    obj <- list(obj1, obj2)
    
    app <- list(
        ui = bootstrapPage(
                fluidPage(
                    ## Application title
                    .pRn2_setTitlePanel(),
                    ## Sidebar Panel
                    sidebarPanel(
                        .pR_condDisplaySelection(),
                        .pRn2_condTabPCA(),
                        width = 3
                        ),
                    ## Main Panel
                    mainPanel(
                        tabsetPanel(
                            .pRn2_tabPanelPCA(),
                            id = "tab1" 
                        ),
                        width = 9
                    )
                )        
            ),
        
        server = function(input, output) {
            
            #.dI <- reactive(tan2009r1)
            
            .searchInd <- reactive(NULL)
            
            ## START: TAB PCA ##
            ## colours
            .params <- reactiveValues(
                colours = c("none", "none"), fcex = c(1, 1), PCAn1 = c(1, 1), PCAn2 = c(2, 2))
            
         #   fcex = input$fcex,
          #  xrange = input$xrange,
         #   yrange = input$yrange,
         #   sb = input$fsymboltype,
        #    legend = input$legendyes, 
        #    legendpos = input$legendpos,
        #    sI = .searchInd(),
        #    cIS = input$chooseIdenSearch
        
            
            
            output$helpPCA <- renderText(c(.ind$params, input$selObj, .params$PCAn2, isolate({.params$PCAn1[.ind$params]})))
            
            
            .colours <- reactive({
                if (input$selObj == "object1")
                    fvarLabels(obj[[1]])
                if (input$selObj == "object2")
                    fvarLabels(obj[[2]])
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
            .valuesPCA <- reactive(.vPCA(obj, .params$PCAn1[.ind$params], 
                                    .params$PCAn2[.ind$params], input$selObj)) #.params$PCAn1[.ind$params], .params$PCAn2[.ind$params], input$selObj))
                       
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
            
            observe({
                if (!is.null(input$fcolours)) {
                    .params$colours[.ind$params] <- input$fcolours}
                if (!is.null(input$fcex)) {
                    .params$fcex[.ind$params] <- input$fcex}
                if (!is.null(input$PCAn1) && !is.null(input$PCAn2)) {
                    .params$PCAn1[.ind$params] <- input$PCAn1
                    .params$PCAn2[.ind$params] <- input$PCAn2 
                }
            })    
        
            output$fsymboltypeOutput <- renderUI({ 
                if (!is.null(input$fcolours) && 
                        input$fcolours %in% fvarLabels(obj[[1]])) 
                    selectInput("fsymboltype", "symbol type", 
                                c("none", .colours()),
                                selected="none")
            })
            
           
            
            ## zoom function: parameters for x- and y-range for PCA plot
            output$xrangeUI <- renderUI({
                .rangePCA(.valuesPCA(), 1)
            })  
            
            output$yrangeUI <- renderUI({
                .rangePCA(.valuesPCA(), 1)
                })
            
           
            
            output$PCALegendUI <- renderUI({
                if (length(input$fcolours))
                    if (input$fcolours %in% fvarLabels(obj[[1]]))
                        ## tick box: add legend
                        checkboxInput("legendyes", "legend", 
                                      value = FALSE)
            })
            
            output$PCALegendposUI <- renderUI({
                if (length(input$fcolours))
                    if (input$fcolours %in% fvarLabels(obj[[1]]))
                        ## drop down menu for position of legend
                        selectInput("legendpos", "position of legend",
                                    choices = c("bottomright", "bottom",
                                        "bottomleft","left", "topleft", "top",
                                        "topright", "right","center"), 
                                    selected="bottomright")
            })
            
            output$PCA1 <- renderPlot(
             #  if (!is.null(obj[[1]]))
                    .plotPCA(obj = obj, 
                            fcolours = .params$colours[1], 
                            fcex = .params$fcex[1],
                            xrange = input$xrange,
                            yrange = input$yrange,
                            sb = input$fsymboltype,
                            PCAn1 = .params$PCAn1[1],
                            PCAn2 = .params$PCAn2[1],
                            legend = input$legendyes, 
                            legendpos = input$legendpos,
                            sI = .searchInd(),
                            cIS = input$chooseIdenSearch,
                            ind = "object1" 
                    )
                    )
            
            output$PCA2 <- renderPlot(
                if (!is.null(obj))
                    .plotPCA(obj = obj, 
                             fcolours = .params$colours[2], 
                             fcex = .params$fcex[2],
                             xrange = input$xrange,
                             yrange = input$yrange,
                             sb = input$fsymboltype,
                             PCAn1 = .params$PCAn1[2],
                             PCAn2 = .params$PCAn2[2],
                             legend = input$legendyes, 
                             legendpos = input$legendpos,
                             sI = .searchInd(),
                             cIS = input$chooseIdenSearch,
                             ind = "object2"
                    )
            )
            
            ## END: TAB PCA ## 
            
        }
    )
    runApp(app)
    
    
}