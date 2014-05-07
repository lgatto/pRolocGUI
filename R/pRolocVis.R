#'@name pRolocVIS
#'@title pRolocVIS
#'@export
#'@author Thomas Naake <thomasnaake@@gmx.de>
#'@usage pRolocVIS()
#'@param object Pass a MSnSet to pRolocVIS directly. Default \code{NULL} will
#'load no MSnSet
#'@description A function to start a shiny session with one MSnSet data set. Run
#'\code{pRolocVIS()} to start the shiny application and choose between
#'three example MSnSet originating from \code{pRolocdata} or upload your
#'own MSnSet. Choosing between the tabs allows to display PCA plots,
#'protein profiles, the underlying data and upload abilities for past
#'search results.
#'@examples \dontrun{pRolocGUI()}

pRolocVIS <- function(object = NULL) {
  
  #on.exit(return(1))
  ## global
  ## load MSnSets
  data(andy2011)
  data(tan2009r1)
  data(dunkley2006)
  options(shiny.maxRequestSize = 20*1024^2)
  
  app <- list(  
    ui = 
      bootstrapPage( 
        fluidPage(
         responsive = TRUE,
          ## Application title
          .pRn1_setTitlePanel(),
          ## Sidebar Panel
          #.pRn1_setSidebarPanelUpload(),
          .pRn1_setSidebarPanel(),  
          ## Main Panel
          .pRn1_setMainPanel()
        )
      ),
    
    server = function(input, output) {   
      ## TAB: DATA/UPLOAD ##
      
      ## upload function for own data, access to data path implemented 
      ## by index "datapath", 
      ## see ?shiny::fileInput for further details
      output$Data1 <- renderUI({
        if (!is.null(object)){
          if (inherits(.dI(), "MSnSet"))
            helpText("committed object okay")
          else 
            helpText("committed object corrupt, 
                     MSnSet Christoforou 2011 will be used")
          }
        else
                 ## choose Data source, 
                 ## a drop down list of A. Christoforou 2011, 
                 ## Dunkley 2006, Tan et al. 2009 (all example data)
                 ## or use your own data by selecting load data
                 selectInput("data",
                             "Choose MSnSet data source:",
                             choices = c("Christoforou 2011", 
                                         "Dunkley et al. 2006",
                                         "Tan et al. 2009", 
                                         "own data"),
                             selected="Christoforou 2011")                
        
        })
      
      output$Data2 <- renderUI({
        if (is.null(object))
          fileInput("owndata", 
                  "Select your own MSnSet file",
                  ## accept=c('.rda', 'data/rda', '.RData', 'data/RData'),
                  multiple = FALSE)
        })
      
      output$Data3 <- renderUI({
        if (is.null(object))
          textOutput("warningowndataUI")
        })

      .dIownData <- reactive({
        if (!length(as.character(input$owndata["datapath"])))
          od <- andy2011
        else{
          ## check if MSnSet has ending .rda or .RData and if 
          ## it is MSnSet
          if (file_ext(input$owndata["name"]) %in% c("rda","RData")){
            if (inherits(
              get(load(as.character(input$owndata["datapath"]))), 
              "MSnSet"))
              od <- get(load(as.character(input$owndata["datapath"])))
            else
              od <- andy2011
          }
          else
            od <- andy2011
        }
      })
      
      
      if (is.null(object))
        .dI <- reactive({
          if (!is.null(input$data))
            switch(input$data,
                   "Christoforou 2011" = andy2011,
                   "Dunkley et al. 2006" = dunkley2006,
                   "Tan et al. 2009" = tan2009r1,
                   "own data" = .dIownData()
              )
            })
      else {
        if (inherits(object, "MSnSet"))
          .dI <- reactive(object)
        else 
          .dI <- reactive(andy2011) 
          }
        
   
      
      output$warningowndataUI <- renderText({
        if(input$data == "own data"){
          if (identical(.dI(), andy2011))
            return("noMSnSet selected, 
                   MSnSet Christoforou 2011 will be used")
          else
            return()
        }
        else
          return()
      })  
      ## END: UPLOAD ##
      
      
      ## start of search implementation
      ## reactive expressions for general search
      ## reactive expression to forward indices to 
      ## plot2D, plotDist and tabs quantitation
      ## and feature meta-data
      .searchInd <- reactive({
        if (length(input$chooseIdenSearch) == 1) {
          if ("text" %in% input$chooseIdenSearch)
            searchInd <- .protText$mult
          if ("mouse.PCA" %in% input$chooseIdenSearch)
            searchInd <- .protPCA$mult
          if ("mouse.plotDist" %in% input$chooseIdenSearch)
            searchInd <- .protPlotDist$mult
          if ("saved.searches" %in% input$chooseIdenSearch &&
              !is.null(input$tagSelectList) && 
              exists("pRolocGUI_SearchResults", .GlobalEnv))
            searchInd <- .whichNamesFOI()
        }
        else {
          searchInd <- NULL
          if ("text" %in% input$chooseIdenSearch)
            searchInd <- c(searchInd, .protText$mult)
          if ("mouse.PCA" %in% input$chooseIdenSearch)
            searchInd <- c(searchInd, .protPCA$mult)
          if ("mouse.plotDist" %in% input$chooseIdenSearch)
            searchInd <- c(searchInd, .protPlotDist$mult)
          if ("saved.searches" %in% input$chooseIdenSearch &&
              !is.null(input$tagSelectList) &&
              exists("pRolocGUI_SearchResults", .GlobalEnv))
            searchInd <- c(searchInd, .whichNamesFOI())
        }
        unique(searchInd)
      })
      
      ## Clear multiple points on click
      observe({
        if (input$resetMult > 0) {
          .protPCA$mult <- NULL
          .protPlotDist$mult <- NULL
          .protText$mult <- NULL
        }
      })
      
      ## text-based search: protein und fvarLabels
      output$searchUI <- renderUI({
        selectInput("search", "", choices = c("protein", .colours()))
      })
      
      output$searchResultsUI <- renderUI({
        if(is.null(input$search))
          return("loading...please wait")
        else {
          if (length(.searchResultsText()))
            selectInput("sRTextInput", label = "",
                choices = .searchResultsText())
          else
            return("not found")
        }
      })
      
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
      
      ## Multiple points list for text based search 
      ## (stores indices of searched levels)
      .protIndices <- reactive({
        if ("text" %in% input$chooseIdenSearch){
          if (input$search == "protein")
            which(rownames(.dI()) == input$sRTextInput)
          else
            which(fData(.dI())[input$search] == input$sRTextInput)
        }
      })
      
      ## vector with reactive values
      .protText <- reactiveValues(mult=NULL)
      
      ## observe indices and concatenate to protText$mult
      observe({
        if (input$saveText > 0)
          isolate(.protText$mult <- c(.protText$mult, .protIndices()))
        })
      ## End of searching implementation ##  
      
      #observe({
      #  if (input$closebutton != 0)
      #    isolate({stopApp()})
      #})
      
      
      
      
      
      
      
      ## TAB: PCA PLOT ##  
  
      ## colours
      .colours <- reactive({
        if(!is.null(.dI()))
          fvarLabels(.dI())
        })
      
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
      
      ## values of PCA, dims is dependent on user input.
      ## so is xlim and ylim
      .valuesPCA <- reactive({
        plot2D(.dI(), fcol=NULL,
            xlim=c(input$xrange[1], input$xrange[2]),
            ylim=c(input$yrange[1], input$yrange[2]),
            dims=c(as.numeric(input$PCAn1),
                   as.numeric(input$PCAn2)), plot=FALSE)
        })
      
      ## PCA plot, legend, points
      .plotPCA <- function() {
        par(mfrow=c(1, 1))
        
        if (length(input$fcolours)){
          if (input$fcolours %in% fvarLabels(.dI()))
            colour <- input$fcolours
          else
            colour <- NULL
          }
        
        if (length(input$fcex)) {
          if (input$fcex %in% fvarLabels(.dI()))
            fcex <- fData(.dI())[, input$fcex]
          else
            fcex <- as.numeric(input$fcex) # i.e. 1
          } 
        else
          fcex <- 1
        
        if (!is.null(input$xrange)) { ## outer if: to prevent error message
          if (is.null(input$fsymboltype) || input$fsymboltype == "none") 
            ## create plot2D and assign reactive variables to arguments,
            ## do not assign fpch (no symboltypes are plotted)
            plot2D(.dI(), fcol = colour,
                xlim = c(input$xrange[1], input$xrange[2]),
                ylim = c(input$yrange[1], input$yrange[2]),
                dims = c(as.numeric(input$PCAn1),
                       as.numeric(input$PCAn2)),
                cex = fcex)
          else
            ## create plot2D and assign reactive variables to arguments
            ## take input$fsymboltype for symboltype
            plot2D(.dI(),fcol = colour, fpch = input$fsymboltype,
                xlim = c(input$xrange[1], input$xrange[2]),
                ylim = c(input$yrange[1], input$yrange[2]),
                dims = c(as.numeric(input$PCAn1),
                       as.numeric(input$PCAn2)),
                cex = fcex)
          }
        
        if(length(input$legendyes)) ## outer if: to prevent error messages
          if (input$fcolours %in% fvarLabels(.dI()) && input$legendyes)
          ## add a legend to the plot with reactive variable as arguments
            addLegend(.dI(), fcol = colour, where = input$legendpos,
                bty = "n", cex = 1)
        
        if(length(.searchInd())) {
          if(length(input$chooseIdenSearch)) {
            foiPCA <- FeaturesOfInterest(description = "hoP",
                fnames = featureNames(.dI())[c(.searchInd())],
                object=.dI())
            highlightOnPlot(.dI(), foiPCA, 
                            args = list(
                              xlim = c(input$xrange[1], input$xrange[2]),
                              ylim = c(input$yrange[1], input$yrange[2]),
                              dims = c(as.numeric(input$PCAn1),
                                     as.numeric(input$PCAn2))),
                            col="black", cex=1.5)
            }
          }
        } ## end function .plotPCA
      
      ## render UI accordingly to .colours()
      output$fcoloursOutput <- renderUI({ 
        selectInput("fcolours", "colour", c("none",.colours()),
            selected="none")
        })
    
      output$fsymboltypeOutput <- renderUI({ 
        if (!is.null(input$fcolours) && input$fcolours %in% fvarLabels(.dI())) 
          selectInput("fsymboltype", "symbol type", c("none", .colours()),
              selected="none")
        })
      
      output$fcexOutput <- renderUI({
        ## initially !length(input$fcolours)
        ## to avoid an error message we have an outer if statement
        ## only show when there are numeric columns in fData (.fcex())
        if (length(input$fcolours) && length(.fcex())){
          if (input$fcolours != "none")
            selectInput("fcex", "point size", c("1", .fcex()),
                selected = "1")
          else
            return()
          }
        })
      
      ## zoom function: parameters for x- and y-range for PCA plot
      output$xrangeUI <- renderUI({
        if(is.null(input$PCAn1))
          return("loading...please wait")
        else
          ## get max and min values of first principal component
          ## create a range slider
          sliderInput("xrange", "zoom x-axis", 
              min = min(.valuesPCA()[,1])-1,
              max = max(.valuesPCA()[,1])+1,
              value = c(min(.valuesPCA()[,1]), max(.valuesPCA()[,1])))
        })  
      
      output$yrangeUI <- renderUI({
        if(is.null(input$PCAn1))
          return("loading...please wait")
        else
          ## get max and min values of second principal component
          ## create a range slider
          sliderInput("yrange", "zoom y-axis",
              min = min(.valuesPCA()[,2])-1, 
              max = max(.valuesPCA()[,2])+1,
              value = c(min(.valuesPCA()[,2]), max(.valuesPCA()[,2])))
        })
      
      ## compute number of principal components to look for 
      ## and change UI accordingly
      output$PCAn1UI <- renderUI({
        if (!is.null(.dI()))
        selectInput("PCAn1", "number of 1st principal component",
            selected = 1,
            choices = c(1:ncol(exprs(.dI()))))
        })
      
      output$PCAn2UI <- renderUI({
        if (!is.null(.dI()))
        selectInput("PCAn2", "number of 2nd principal component",
            selected = 2,
            choices = c(1:ncol(exprs(.dI()))))
        })
      
      ## Print fData of the latest selected protein in input PCA  
      ##output$info.prot.PCAUI <- renderTable({
      ##   fData(.dI())[minDist2dProtPCA(),]
      ##})
      
      output$PCA.legendUI <- renderUI({
        if (length(input$fcolours))
          if (input$fcolours %in% fvarLabels(.dI()))
            ## tick box: add legend
            checkboxInput("legendyes", "legend", 
                value = FALSE)
        })
      
      output$PCA.legendposUI <- renderUI({
        if (length(input$fcolours))
          if (input$fcolours %in% fvarLabels(.dI()))
            ## drop down menu for position of legend
            selectInput("legendpos",
                "position of legend",
                choices = c("bottomright", "bottom", "bottomleft","left",
                            "topleft", "top", "topright", "right","center"),
                selected="bottomright")
        })
      
      ## Generate PCA plot, use fcolours for colours and add legend function 
      ## (appearance and position dependent of user input)
      output$PCA <- renderPlot(.plotPCA())    
      
      ## for Plot/Download button (needs a reactive expression)
      .PCAPlotReac <- reactive(.plotPCA())
      
      ## Download Handler for PCA plot
      output$plotPCADownload <- downloadHandler(
        filename = function() {
          paste(input$data, "-" , Sys.Date(), '.png', sep='')
          },
        content = function(file) {
          png(file)
          print(.PCAPlotReac())
          dev.off()
          }
        )
      
      ## reactive expressions for search based on cursor input for PCA
      
      minDist2dProtPCA <- reactive({
        ## will be empty initially
        if (!is.null(input$PCAclick)){
          ## compute 2D distances from click input to each component 
          ## of the PCA plot, input$PCAclick$x and input$PCAclick$y
          ## is user input
          dist <- sqrt(
            (input$PCAclick$x - .valuesPCA()[,1])^2 + ## x-component
            (input$PCAclick$y - .valuesPCA()[,2])^2 ## y-component
              )
          minDist2d <- min(dist)
          ## compute the element (row index, i.e. the protein) which has the 
          ## shortest distance to the input (index will be returned)
          whichMinDist2d <- which(dist == minDist2d)
          }
        })
      
      ## Multiple points list
      ## Create a list-like object with reactive values
      .protPCA <- reactiveValues(mult=NULL)
      ## observe and concatenate new indices to .protPCA$mult
      observe({
        ## will be empty initially
        if(!is.null(input$PCAclick)){
          isolate({.protPCA$mult <- c(.protPCA$mult, minDist2dProtPCA())
          ## remove indices when indices are clicked another time
          if (length(which(as.vector(table(.protPCA$mult)) > 1))) 
      
            .protPCA$mult <- 
              .protPCA$mult[
                -which(.protPCA$mult == names(which(table(.protPCA$mult) > 1)))
                ]
            
            })   
          }
        }) 
      ## END: PCA PLOT ##

      
      
      ## TAB: PLOTDIST ##
      
      ## reactive expressions for plotDist
      
      ## organelle marker name
      .organelleMarkerName <- reactive(
        if (!is.null(.organelleMarkerName))
          names(table(fData(.dI())[input$sourceOrganelleMarkerPLDI]))
        )
      
      ## organelle for all name
      .organelleAllName <- reactive(
        if (!is.null(.organelleAllName))
          names(table(fData(.dI())[input$allOrganellePLDI]))
        )
      
      ## Index of element in list where parameters are stored
      .nCol <- reactive({
        if (is.null(input$numberPlotDist))
          1
        else
            as.numeric(input$numberPlotDist)
        })
      
      ## list where parameters for plot are stored
      ## create a list with reactive values
      .listParams <- reactiveValues(
        levOrgMark = NULL, 
        levOrgMarkOrg = NULL, 
        levSourMarkAll = NULL, 
        levSourMarkAllOrg = NULL
        )
      
      ## write paramters to list for plotDist at index of .nCol()
      observe({
        if (!is.null(input$organelleAll)) {
          isolate(.listParams$levOrgMark[.nCol()] <- 
              input$sourceOrganelleMarkerPLDI)
          isolate(.listParams$levOrgMarkOrg[.nCol()] <- 
              input$organelleMarker)
          isolate(.listParams$levSourMarkAll[.nCol()] <- 
              input$allOrganellePLDI)
          isolate(.listParams$levSourMarkAllOrg[.nCol()] <- 
              input$organelleAll)
          }
        })
      
      ## calculate protein nearest to user input
      .minDistProteinPlotDist <- reactive(
        if (!is.null(input$plotDistclick) && input$quantityPlotDist == "1") {
          ## compute indices of printed proteins in plotDist
          j <- match(
                 subset(featureNames(.dI()),
                   fData(.dI())[, .listParams$levOrgMark[1]] == 
                     .listParams$levOrgMarkOrg[1]),
                 featureNames(.dI())
                 )
          dist <- abs(
            input$plotDistclick$y - 
              exprs(.dI())[j, round(input$plotDistclick$x, 0)]
              )
          minDist <- min(dist)
          whichMinDist <- which(minDist == dist)
          ## return index
          j[whichMinDist[[1]]]
          }
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
              c(.protPlotDist$mult,.minDistProteinPlotDist())
            ## remove indices when indices are double-clicked
            if (length(which((as.vector(table(.protPlotDist$mult)) > 1))))
              .protPlotDist$mult <- 
                .protPlotDist$mult[-which(.protPlotDist$mult
                    == names(which(table(.protPlotDist$mult) > 1)))]
            })
          }
        }) 
      
      ## plotDist and highlighting selected points in plot
      .plotPlotDist <- function() {
        if(!is.null(.listParams$levOrgMark)) {
          
          if(as.numeric(input$quantityPlotDist)%%2==0)
            col <- as.numeric(input$quantityPlotDist)/2
          else
            col <- (as.numeric(input$quantityPlotDist)+1)/2
          
          if (as.numeric(input$quantityPlotDist)==1)
            par(mfrow=c(1,1))
          else
            par(mfrow=c(2, col))
          
          ## Actual plotting
          for (i in 1:length(.listParams$levOrgMark)) { 
            ## j (returns indices in fData/exprs)
            j <- match(
                   subset(featureNames(.dI()),
                     fData(.dI())[, .listParams$levOrgMark[i]] ==
                       .listParams$levOrgMarkOrg[i]),
                   featureNames(.dI())
                   )
            
           iComp <-  featureNames(
                  subset(.dI(), fData(.dI())[, .listParams$levSourMarkAll[i]] == 
                     .listParams$levSourMarkAllOrg[i]))
  
           jComp <- subset(
             featureNames(.dI()), 
             fData(.dI())[, .listParams$levOrgMark[i]] == 
               .listParams$levOrgMarkOrg[i])  
            
              
            ## which of j are in .searchInd(), returns index in j, e.g.
            ## "10", i.e. the tenth element of j is also in .searchInd()
            ind.col <- na.omit(match(.searchInd(), j))
            
            ## vector for colours
            if(length(ind.col)&& length(input$chooseIdenSearch)) {
              mcol.ind <- rep("steelblue",length(j)) 
              mcol.ind[ind.col] <- "black"
              }
            else
              mcol.ind <- "steelblue"
            
            ## vector for lwd
            if(length(ind.col) && length(input$chooseIdenSearch)) {
              lwd.ind <- rep(1,length(j))
              lwd.ind[ind.col] <- 3}
            else
              lwd.ind <- 1
            
            if (length(na.exclude(match(jComp, iComp))) == length(j)) {
            
              plotDist(
              subset(.dI(), fData(.dI())[, .listParams$levSourMarkAll[i]] == 
                  .listParams$levSourMarkAllOrg[i]), 
              markers = subset(
                  featureNames(.dI()), 
                  fData(.dI())[, .listParams$levOrgMark[i]] == 
                    .listParams$levOrgMarkOrg[i]),
              mcol = mcol.ind, 
              lwd = lwd.ind
              )
            title(.listParams$levOrgMarkOrg[i])
                    }
            } ## end for loop
          }
        }
      
      ## for Plot/Download button (needs a reactive expression)
      .plotDistReac <- reactive(.plotPlotDist()) 
      
      output$levelsOrganellesUI <- renderUI(
        selectInput("sourceOrganelleMarkerPLDI",
            "Source for organelle markers", 
            choices = .colours())
        )
      
      output$allOrganellesUI <- renderUI(
        selectInput("allOrganellePLDI",
            "Source for all assigned proteins to the organelle",
            choices = .colours(), 
            selected = input$sourceOrganelleMarkerPLDI)
        )
      
      output$organelleMarkerUI <- renderUI(
        if(!is.null(input$sourceOrganelleMarkerPLDI))
          selectInput("organelleMarker", 
              "Organelle for organelle markers",
              choices = .organelleMarkerName())
        )
      
      output$organelleAllUI <- renderUI(
        if(!is.null(input$allOrganellePLDI))
          selectInput("organelleAll",
              "Organelle for all assigned proteins to the organelle",
              choices = .organelleAllName(), 
              selected=input$organelleMarker)
        )
      
      output$numberPlotDistUI <- renderUI(
        if (!as.numeric(input$quantityPlotDist) == 1) {
          ## reset all parameters to avoid conflicts when decreasing the 
          ## quantity of plots
          .listParams$levOrgMark <- NULL
          .listParams$levOrgMarkOrg <- NULL
          .listParams$levSourMarkAll <- NULL
          .listParams$levSourMarkAllOrg <- NULL
  
          sliderInput("numberPlotDist",
              "Select number of plot to change",
              min = 1,max = as.numeric(input$quantityPlotDist), value = 1,
              step = 1)
        }
      )
      
      output$plotdist <- renderPlot(.plotPlotDist())
      
      output$plotDistDownload <- downloadHandler(
        filename = function() {
          paste(input$data, "-", "plotDist","-", Sys.Date(), '.png', sep='')
          },
        content = function(file) {
          png(file)
          print(.plotDistReac())
          dev.off()
          }
        )
      
      
      ## END: PLOTDIST ##
      
      
      
      ## TAB: QUANTITATION DATA ##
      ## Generate the quantitation data
      output$exprsRadioUI <- renderUI({
        radioButtons("exprsRadio","Select",
            choices = list("all or"="all", "selected"="selected"),
            selected = ifelse(length(.searchInd()), "selected", "all")
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
                " " = row.names(exprs(.dI())),
                exprs(.dI())
              )
            )[c(.searchInd()), ]
        })
      ## END: QUANTITATION DATA ##
      
      
      
      ## TAB: FEATURE META-DATA ##
      ## Generate the feature meta-data
      output$fDataRadioUI <- renderUI({
        radioButtons("fDataRadio","Select",
            choices=list("all or" = "all", "selected" = "selected"),
            selected = ifelse(length(.searchInd()), "selected", "all")
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
              " " = row.names(fData(.dI())),
              fData(.dI())
              )
            )[c(.searchInd()), ]
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
      ## get object pRolocGUI_SearchResults from
      ## the global environment and poll for changes
      .pR_SR <- reactivePoll(
          intervalMillis = 100, 
          session = NULL,
          checkFunc = .digestFOI,
          valueFunc = .readSR
          )
      
      ## Get the tag names of the list with the saved search results 
      .tagsList <- reactivePoll(
          intervalMillis = 100, 
          session = NULL,
          checkFunc = .digestFOI,
          valueFunc = .descrFOI
          )

      .whichN <- reactive({
        which(
            input$tagSelectList == substring(.descriptionFOI(.pR_SR()), 1, 15)
          )[1]
        })
   
      .names.FOI <- reactive({
        if (.areFeaturesOfInterest(.pR_SR()))
          .fnamesFOI(.pR_SR())
        else
          .fnamesFOI(.pR_SR())[[.whichN()]]
        })
   
      .whichNamesFOI <- reactive({
        if (.areFeaturesOfInterest(.pR_SR()))
          which(match(rownames(.dI()), .fnamesFOI(.pR_SR())) != "NA")
        else
          which(
             match(
                 rownames(.dI()), .fnamesFOI(.pR_SR())[[.whichN()]]
               )  != "NA"
            )
        })
   
        ## select Input for the tag names of the list
        output$tagsListSearchResultUI <- renderUI(
          if (exists("pRolocGUI_SearchResults", .GlobalEnv) | 
                !is.null(.pR_SR())) 
            selectInput("tagSelectList",
                "Select search result",
                choices = .tagsList()
              )
          )
  
        ## display information about selected FoI
        output$infoSavedSearch <- renderText({
          if (exists("pRolocGUI_SearchResults", .GlobalEnv)| 
                !is.null(.pR_SR())) {
            showFOI <- .showFOI(.pR_SR(), .dI(), .whichN())
            paste0(showFOI, sep = "\n", collapse = "")
            }
          else
            return("pRolocGUI_SearchResults not found in workspace")
          })
        
        ## text field to assign name to search results
        output$savedSearchTextUI <- renderUI(
          if(exists("pRolocGUI_SearchResults", .GlobalEnv) || 
               !is.null(.pR_SR())) 
            textInput("savedSearchText", 
                "Description", 
                value="new search result") 
          )
        
        ## action button to save new FoIs
        output$save.lists2SRUI <- renderUI({
          if ((exists("pRolocGUI_SearchResults", .GlobalEnv) ||
               !is.null(.pR_SR())) && !is.null(input$savedSearchText)) {
            if(!(input$savedSearchText %in% .descriptionFOI(.pR_SR())))
              actionButton("save.lists2SR", 
                  "Create new features of interest"
                )
            else
              return("name exists already, choose another name")
            }
          })
        
        ## Action Button when pRolocGUI_SearchResults does not exist 
        ## in .GlobalEnv --> initialize
        output$init.saveUI <- renderUI({
          if (!exists("pRolocGUI_SearchResults", .GlobalEnv) 
               && is.null(.tagsList()))
            actionButton("init.savedsearch",
                "Initialize saved searches"
              )
          })
 
        ## create new FoICollection to initialize saved searches and
        ## assign it to the name pRolocGUI_SearchResult in .GlobalEnv
        observe({
          if (!is.null(input$init.savedsearch) && input$init.savedsearch > 0) {
            init.foi <- FeaturesOfInterest(
                description = "empty",
                fnames = featureNames(tan2009r1)[0]
                )
            init.coll <- FoICollection()
            init.coll <- addFeaturesOfInterest(init.foi, init.coll)
            assign("pRolocGUI_SearchResults", init.coll, envir = .GlobalEnv)
            }
          })
  
          ## new features of Interest as a reactive expression
          .newfoi <- reactive({
            input$save.lists2SR
            isolate({
              FeaturesOfInterest(
                  description = input$savedSearchText,
                  fnames = featureNames(.dI())[.searchInd()],
                  object = .dI())
              })
            })     
          
          ## overwrite pRolocGUI_SearchResults with new features
          ## of interest by assigning it in .GlobalEnv, treat 
          ## accordingly if .pR_SR() is FeaturesOfInterest or 
          ## collection of Features Of Interest
          observe({
          if (!is.null(input$save.lists2SR) && input$save.lists2SR > 0 
               && !is.null(.searchInd()) && length(input$savedSearchText)) {
        
            if (.isFoICollection(.pR_SR()) &&
                  "empty" %in% .descriptionFOI(.pR_SR())) 
              oldSR <- FoICollection() 
            else
              oldSR <- .pR_SR()
          
            newFOI <- .newfoi()
          
            if (.areFeaturesOfInterest(.pR_SR())){
              newColl <- FoICollection() ## create new collection
              ## add old FoI to collection
              newColl <- addFeaturesOfInterest(oldSR, newColl)
              newColl <- isolate({
                input$save.lists2SR
                addFeaturesOfInterest(newFOI, newColl)
                })
              assign("pRolocGUI_SearchResults", newColl, envir = .GlobalEnv)
              }
            else {
              newColl <- isolate({
                input$save.lists2SR
                addFeaturesOfInterest(newFOI, oldSR)
                })
              assign("pRolocGUI_SearchResults", newColl, envir = .GlobalEnv)
              }
            } ## end if
          })## end observe
        } ## end server function
    
      ) ## end list
  
    runApp(app)

  } ## end function

