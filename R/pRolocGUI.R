#'@name pRolocGUI
#'
#'@title pRolocGUI
#'
#'@export
#'
#'@author Thomas Naake <thomasnaake@@gmx.de>
#'
#'@usage pRolocGUI()
#'
#'@description A function to start a shiny session with one MSnSet data set. Run
#'\code{pRolocGUI()} to start the shiny application and choose between
#'three example MSnSet originating from \code{pRolocdata} or upload your
#'own MSnSet. Choosing between the tabs allows to display PCA plots,
#'protein profiles, the underlying data and upload abilities for past
#'search results.
#'
#'@examples \dontrun{pRolocGUI()}
#'
pRolocGUI <- function(){
  ## global
  data(andy2011)
  data(tan2009r1)
  data(dunkley2006)
  options(shiny.maxRequestSize=20*1024^2)
  
  app <- list(  
    ui = 
      bootstrapPage( ## here goes the ui.R
        fluidPage(
         responsive = TRUE,
          ## Application title
          .pRn1_setTitlePanel(),
          .pRn1_setSidebarPanel(),  
          .pRn1_setMainPanel()
             )),
      
      
    server = function(input, output){ ## here goes the server.R
      
      ## start of search implementation
      ## reactive expressions for general search
      
      ## reactive expression to forward indices to 
      ## plot2D and plotDist and tabs quantitation
      ## and feature meta-data
      .search.ind <- reactive({
        if (length(input$chooseIdenSearch)==1){
          if ("text" %in% input$chooseIdenSearch){
            search.ind <- .prot.text$mult}
          if ("mouse.PCA" %in% input$chooseIdenSearch){
            search.ind <- .prot.PCA$mult}
          if ("mouse.plotDist" %in% input$chooseIdenSearch){
            search.ind <- .prot.plotDist$mult}
          if ("saved.searches" %in% input$chooseIdenSearch &&
                !is.null(input$tag.select.list)){
            if (exists("pRolocGUI_SearchResults", .GlobalEnv)){
              search.ind <- .which.names.FOI()}}
          search.ind}
        else{
          search.ind <- NULL
          if ("text" %in% input$chooseIdenSearch){
            search.ind <- c(search.ind, .prot.text$mult)}
          if ("mouse.PCA" %in% input$chooseIdenSearch){
            search.ind <- c(search.ind, .prot.PCA$mult)}
          if ("mouse.plotDist" %in% input$chooseIdenSearch){
            search.ind <- c(search.ind, .prot.plotDist$mult)}
          if ("saved.searches" %in% input$chooseIdenSearch &&
                !is.null(input$tag.select.list)){
            if (exists("pRolocGUI_SearchResults", .GlobalEnv)){
              search.ind <- c(search.ind, .which.names.FOI())}}
          search.ind
        }
        search.ind <- unique(search.ind)
      })
      
      ## Clear multiple points on click
      observe({
        if (input$reset.mult > 0){
          .prot.PCA$mult <- NULL
          .prot.plotDist$mult <- NULL
          .prot.text$mult <- NULL}})
      
      ## protein und fvarLabels
      ## see also: search.R
      output$search.UI <- renderUI({
        selectInput("search","",choices=c("protein",.colours()))})
      
      output$search.results.UI <- renderUI({
        if(is.null(input$search)){
          return("loading...please wait")
        }else{
          if (length(.searchResults.text()) !=0 ){
            selectInput("search.results", label="",
                        choices=.searchResults.text())
          } else{return("not found")}}
      })
      
      ## reactive expressions for text based search
      
      ## levels to search
      .searchResults.text <- reactive({
        subset(
          if(input$search != "protein"){
            names(table(fData(.dI())[input$search]))}
          else{
            row.names(.dI())}, 
          grepl(input$level.search,
                if(input$search != "protein"){
                  names(table(fData(.dI())[input$search]))}
                else{
                  row.names(.dI())}))
      })
      
      ## Multiple points list for text based search 
      ## (stores indices of searched levels)
      .proteins.indices <- reactive({
        if ("text" %in% input$chooseIdenSearch){
          if (input$search == "protein"){
            which(rownames(.dI())== input$search.results)}
          else{ 
            which(fData(.dI())[input$search] == input$search.results)}
        }})
      
      ## vector with reactive values
      .prot.text <- reactiveValues(mult=NULL)
      
      ## observe indices and concatenate to prot.text$mult
      observe({
        if (input$save.text > 0){
          isolate(.prot.text$mult <- c(.prot.text$mult, .proteins.indices()))
        }})
      ## End of searching implementation ##  
      
      #observe({
      #  if (input$closebutton != 0)
      #    isolate({stopApp()})
      #})
      
      ## TAB: DATA/UPLOAD ##
      ## reactive expressions for upload of MSnSets
      
      ## upload function for own data, access to data path implemented by index 
      ## "datapath", see ?shiny::fileInput for further details
      .dIownData <- reactive({
        if (length(as.character(input$owndata["datapath"])) == 0){
          od <- andy2011}else{
          ## check if MSnSet has ending .rda or .RData and if it is MSnSet
          if (file_ext(input$owndata["name"]) %in% c("rda","RData")){
            if (inherits(get(load(as.character(input$owndata["datapath"]))), 
                         "MSnSet") == TRUE){
              od <- get(load(as.character(input$owndata["datapath"])))
            }else{od <- andy2011}}else{od <- andy2011}
        }})
      
      ## use either example data andy2011, dunkley 2006, tan2009 (in pRolocdata) 
      ## or own data and assign it appropriately
      .dI <- reactive({
        switch(input$data,
               "Christoforou 2011" = andy2011,
               "Dunkley et al. 2006" = dunkley2006,
               "Tan et al. 2009" = tan2009r1,
               "own data" = .dIownData())
      })
     
      
      ## see also: upload.R    
      output$warningowndataUI <- renderText({
        if(input$data == "own data"){
          if (identical(.dI(), andy2011)){
            return("noMSnSet selected,
                   MSnSet Christoforou 2011 will be used")
          }else{return()}}else{return()}})  
      ## END: UPLOAD ##
      
      
      
      ## TAB: PCA PLOT ##  
      
      ## reactive expressions for PCA plot and
      ## function for PCA plot
      
      ## colours
      .colours <- reactive({
        if(!is.null(colours)){ 
          ## fvarLabels accesses labels of feature data
          fvarLabels(.dI())} 
      })
      
      ## point size
      .fcex <- reactive({
        ## check for colums in fData(.dI()) which are numeric
        col.num <- which(sapply(fData(.dI()), is.numeric) == TRUE)
        ## write indices in vector col.num
        col.num <- as.vector(col.num)
        if(!length(col.num)==0){
          ## return fvarLabels of colums which are numeric
          fvarLabels(.dI())[col.num]}
      })
      
      ## values of PCA, dims is dependent on user input.
      ## so is xlim and ylim
      .values.PCA <- reactive({
        plot2D(.dI(),fcol=NULL,
               xlim=c(input$xrange[1],input$xrange[2]),
               ylim=c(input$yrange[1],input$yrange[2]),
               dims=c(as.numeric(input$PCAn1),
                      as.numeric(input$PCAn2)),plot=FALSE)})
      
      ## PCA plot, legend, points
      .plotPCA <- function(){
        par(mfrow=c(1,1))
        
        if (length(input$fcolours)){
        if (input$fcolours %in% fvarLabels(.dI())){
          colour <- input$fcolours} else{
            colour <- NULL}}
        
        if (length(input$fcex) != 0){
          if (input$fcex %in% fvarLabels(.dI())){
            fcex <- fData(.dI())[,input$fcex]
          }else{fcex <- as.numeric(input$fcex)}
        }else{fcex <- 1}
        
        if (!is.null(input$xrange)){
          if (input$fsymboltype == "none"){
            ## create plot2D and assign reactive variables to arguments,
            ## do not assign fpch (no symboltypes are plotted)
            plot2D(.dI(),fcol=colour,
                   xlim=c(input$xrange[1],input$xrange[2]),
                   ylim=c(input$yrange[1],input$yrange[2]),
                   dims=c(as.numeric(input$PCAn1),
                          as.numeric(input$PCAn2)),
                   cex=fcex)}else{
                     ## create plot2D and assign reactive variables to arguments
                     ## take input$fsymboltype for symboltype
                     plot2D(.dI(),fcol=colour, fpch=input$fsymboltype,
                            xlim=c(input$xrange[1],input$xrange[2]),
                            ylim=c(input$yrange[1],input$yrange[2]),
                            dims=c(as.numeric(input$PCAn1),
                                   as.numeric(input$PCAn2)),
                            cex=fcex)}}
        
        if(length(input$legendyes)){
        if (input$fcolours %in% fvarLabels(.dI()) && input$legendyes){
          ## add a legend to the plot with reactive variable as arguments
          addLegend(.dI(), fcol=colour, where=input$legendposition,
                    bty="n", cex=1)}}
        
        if(length(.search.ind()) != 0){
          if(length(input$chooseIdenSearch)){
            .foi.PCA <- FeaturesOfInterest(description="hoP",
                        fnames=featureNames(.dI())[c(.search.ind())],
                        object=.dI())
            highlightOnPlot(.dI(), .foi.PCA, col="black", cex=1.5)}}
           # was formerly:
           # points(x=.values.PCA()[.search.ind(),1],
           #        y=.values.PCA()[.search.ind(),2],
           #        type="p",col="black",
           #        pch=1,cex=2,lwd=1.5)}}
      } ## end function .plotPCA
      
      ## the UI will be rendered accordingly to .colours which is reactive to the 
      ## MSnSet selected
      output$fcoloursOutput <- renderUI({ 
        selectInput("fcolours","colour",c("none",.colours()),
                    selected="none")})
      
      output$fsymboltypeOutput <- renderUI({ 
        selectInput("fsymboltype","symbol type", c("none",.colours()),
                    selected="none")})
      
      output$fcexOutput <- renderUI({
        ## initially length(input$fcolours) == 0
        ## to avoid an error message we have an outer if statement
        ## only show when there are numeric columns in fData (.fcex())
        if (length(input$fcolours) != 0 && length(.fcex() != 0)){
          if (input$fcolours != "none"){
            selectInput("fcex", "point size", c("1",.fcex()),
                        selected="1")}
        }else{return()}})
      
      ## zoom function in server.R notation
      output$xrangeUI <- renderUI({
        if(is.null(input$PCAn1)){
          return("loading...please wait")
        } else{
          ## get max and min values of first principal component
          ## create a range slider
          sliderInput("xrange","zoom x-axis", 
                      min=min(.values.PCA()[,1])-1,max=max(.values.PCA()[,1])+1,
                      value=c(min(.values.PCA()[,1]),max(.values.PCA()[,1])))}})  
      
      output$yrangeUI <- renderUI({
        if(is.null(input$PCAn1)){
          return("loading...please wait")
        } else{
          ## get max and min values of second principal component
          ## create a range slider
          sliderInput("yrange","zoom y axis",
                      min=min(.values.PCA()[,2])-1,max=max(.values.PCA()[,2])+1,
                      value=c(min(.values.PCA()[,2]),max(.values.PCA()[,2])))}})
      
      ## compute number of principal components to look for in server.R notation
      ## and change UI accordingly
      output$PCAn1UI <- renderUI({
        selectInput("PCAn1","number of 1st principal component",selected=1,
                    choices=c(1:ncol(exprs(.dI()))))})
      output$PCAn2UI <- renderUI({
        selectInput("PCAn2","number of 2nd principal component",selected=2,
                    choices=c(1:ncol(exprs(.dI()))))})
      
      ## Print fData of the latest selected protein in input PCA  
      #output$info.prot.PCAUI <- renderTable({
      #   fData(.dI())[.min.dist2d.prot.pca(),]
      #})
      
      output$PCA.legendUI <- renderUI({
        if (length(input$fcolours) != 0){
          if (input$fcolours %in% fvarLabels(.dI())){
            ## tick box: add legend
            checkboxInput("legendyes","legend of PCA plot", 
                          value = FALSE)}else{return()}}})
      
      output$PCA.legendpositionUI <- renderUI({
        if (length(input$fcolours) != 0){
          if (input$fcolours %in% fvarLabels(.dI())){
            ## drop down menu for position of legend
            selectInput("legendposition",
                        "position of legend (PCA plot)",
                        choices = c("bottomright","bottom",
                                    "bottomleft","left","topleft",
                                    "top","topright",
                                    "right","center"),
                        selected="bottomright")}else{return()}}})
      
      ## Generate PCA plot, use fcolours for colours and add legend function 
      ## (appearance and position dependent of user input)
      output$PCA <- renderPlot(.plotPCA())    
      
      ## for Plot/Download button (needs a reactive expression)
      .PCA.plot.reac <- reactive(.plotPCA())
      
      ## Download Handler for PCA plot
      output$plotPCA.download <- downloadHandler(
        filename = function() {paste(input$data,"-",Sys.Date(), '.png', sep='')},
        content = function(file) {
          png(file)
          print(.PCA.plot.reac())
          dev.off()
        })
      
      ## reactive expressions for search based on cursor input for PCA
      
      .min.dist2d.prot.pca <- reactive({
        ## will be empty initially
        if (!is.null(input$PCAclick)){
          ## compute 2D distances from click input to each component 
          ## of the PCA plot, input$PCAclick$x and input$PCAclick$y
          ## is user input
          min.dist2d <- min(sqrt((input$PCAclick$x-.values.PCA()[,1])^2 + 
                                   (input$PCAclick$y-.values.PCA()[,2])^2))
          ## compute the element (row index, i.e. the protein) which has the 
          ## shortest distance to the input (index will be returned)
          which.min.dist2d <- which(sqrt((input$PCAclick$x-.values.PCA()[,1])^2 + 
                                           (input$PCAclick$y-.values.PCA()[,2])^2) == min.dist2d)
        }})
      
      ## Multiple points list
      ## Create a list-like object with reactive values
      .prot.PCA <- reactiveValues(mult=NULL)
      ## observe and concatenate new indices to .prot.PCA$mult
      observe({
        ## will be empty initially
        if(!is.null(input$PCAclick)){
          isolate({.prot.PCA$mult <- c(.prot.PCA$mult,.min.dist2d.prot.pca())
                   ## remove indices when indices are double-clicked
                   if (length(which((as.vector(table(.prot.PCA$mult)) > 1) 
                                    == TRUE))){
                     .prot.PCA$mult <- 
                       .prot.PCA$mult[-which(.prot.PCA$mult 
                                             == names(which(table(.prot.PCA$mult)>1)))]}
          })}})
      
                   
      ## END: PCA PLOT ##
      
      
      
      ## TAB: PLOTDIST ##
      
      ## reactive expressions for plotDist
      
      ## organelle marker name
      .organelle.marker.name <- reactive({
        if (is.null(.organelle.marker.name)){
          return()}
        else{
          names(table(fData(.dI())[input$source.organelle.marker.pldi]))}})
      
      ## organelle for all name
      .organelle.all.name <- reactive({
        if (is.null(.organelle.all.name)){
          return()} 
        else{
          names(table(fData(.dI())[input$all.organelle.pldi]))}})
      
      ## Index of element in list where parameters are stored
      .n.col <- reactive({
        if (is.null(input$number.plot.dist)){
          1}else{
            as.numeric(input$number.plot.dist)}})
      
      ## list where parameters for plot are stored
      ## create a list with reactive values
      .list.params <- reactiveValues(lev.org.mark = NULL, lev.org.mark.org = NULL, 
                                     lev.sour.mark.all = NULL, lev.sour.mark.all.org = NULL)
      
      ## write paramters to list for plotDist at index of .n.col()
      observe({
        if (!is.null(input$organelle.all)){
          isolate(.list.params$lev.org.mark[.n.col()] <- 
                    c(input$source.organelle.marker.pldi))
          isolate(.list.params$lev.org.mark.org[.n.col()] <- 
                    c(input$organelle.marker))
          isolate(.list.params$lev.sour.mark.all[.n.col()] <- 
                    c(input$all.organelle.pldi))
          isolate(.list.params$lev.sour.mark.all.org[.n.col()] <- 
                    c(input$organelle.all))
        }})
      
      ## plotDist and highlighting selected points in plot
      .plotplotDist <- function(){
        if(!is.null(.list.params$lev.org.mark)){
          col <-  if(as.numeric(input$quantity.plot.dist)%%2==0){
            as.numeric(input$quantity.plot.dist)/2}else{
              (as.numeric(input$quantity.plot.dist)+1)/2}
          
          if(as.numeric(input$quantity.plot.dist)==1){
            par(mfrow=c(1,1))}else{par(mfrow=c(2,col))}
          
          ## Actual plotting
          for(i in 1:length(.list.params$lev.org.mark)){ 
            ## j (returns indices in fData/exprs)
            j <- match(subset(featureNames(.dI()),
                              fData(.dI())[,.list.params$lev.org.mark[i]] ==
                                .list.params$lev.org.mark.org[i]),
                       featureNames(.dI()))
            ## which of j are in .search.ind(), returns index in j, e.g.
            ## "10", i.e. the tenth element of j is also in .search.ind()
            ind.col <- na.omit(match(.search.ind(),j))
            ## a vector for colours, only create it 
            ## when there are shared elements
            ## AND (length(input$chooseIdenSearch)) > 0
            if(length(ind.col)!=0 && length(input$chooseIdenSearch)){
              ## create a vector with length(j) 
              ## with the elements "steelblue"
              mcol.ind <- rep("steelblue",length(j))
              ## when there is a shared element in search.ind() and j 
              ## address the colours "black" to it 
              mcol.ind[ind.col] <- "black"
              mcol.ind}else{mcol.ind <- "steelblue"}
            ## a vector for lwd, only create it when there are 
            ## shared elements AND length(input$chooseIdenSearch) > 0
            if(length(ind.col)!=0 && length(input$chooseIdenSearch)){
              ## create a vector with length(j) with the elements "1"
              lwd.ind <- rep(1,length(j))
              ## when there is a shared element in .search.ind() and 
              ## j address lwd 3 to it
              lwd.ind[ind.col] <- 3
              lwd.ind}else{lwd.ind <- 1}
            
            plotDist(subset(.dI(), 
                            fData(.dI())[,.list.params$lev.sour.mark.all[i]] == 
                              .list.params$lev.sour.mark.all.org[i]), 
                     markers = subset(featureNames(.dI()), 
                                      fData(.dI())[,.list.params$lev.org.mark[i]] == 
                                        .list.params$lev.org.mark.org[i]),
                     mcol=mcol.ind, lwd=lwd.ind)
            title(.list.params$lev.org.mark.org[i])
          } ## end for loop
        }}
      
      ## for Plot/Download button (needs a reactive expression)
      .plotDist.reac <- reactive({.plotplotDist()}) 
      
      output$levels.organellesUI <- renderUI({
        selectInput("source.organelle.marker.pldi",
                    "Source for organelle markers",
                    .colours())})
      
      output$all.organellesUI <- renderUI({
        selectInput("all.organelle.pldi",
                    "Source for all assigned proteins to the organelle",
                    .colours(),selected=input$source.organelle.marker.pldi)})
      
      output$organelle.markerUI <- renderUI({
        if(is.null(input$source.organelle.marker.pldi)){
          return("loading...please wait")} else{
            selectInput("organelle.marker",
                        "Organelle for organelle markers",choices=.organelle.marker.name())}})
      
      output$organelle.allUI <- renderUI({
        if(is.null(input$all.organelle.pldi)){
          return("loading...please wait")} else{
            selectInput("organelle.all",
                        "Organelle for all assigned proteins to the organelle",
                        choices=.organelle.all.name(),selected=input$organelle.marker)}})
      
      output$number.plot.distUI <- renderUI({
        if (as.numeric(input$quantity.plot.dist) == 1){
          return()
        }else{
          ## reset all parameters to avoid conflicts when decreasing the 
          ## quantity of plots
          .list.params$lev.org.mark <- NULL
          .list.params$lev.org.mark.org <- NULL
          .list.params$lev.sour.mark.all <- NULL
          .list.params$lev.sour.mark.all.org <- NULL
          
          sliderInput("number.plot.dist",
                      "Select number of plot to change",
                      min=1,max=as.numeric(input$quantity.plot.dist),value=1,
                      step=1)}
      })
      
      output$plotdist <- renderPlot({
        .plotplotDist()
      })
      
      output$plotDist.download <- downloadHandler(
        filename = function() {paste(input$data,
                                     "-","plotDist","-", 
                                     Sys.Date(), '.png', sep='')},
        content = function(file) {
          png(file)
          print(.plotDist.reac())
          dev.off()
        })
      
      ## reactive expressions for search based on cursor input 
      ## for protein profiles (plotDist)
      
      ## calculate protein nearest to user input
      .min.dist.protein.plotDist <- reactive({
        if (!is.null(input$plotDistclick$x) && input$quantity.plot.dist == "1"){
          ## compute indices of printed proteins in plotDist
          j <- match(subset(featureNames(.dI()),
                            fData(.dI())[,.list.params$lev.org.mark[1]] ==
                              .list.params$lev.org.mark.org[1]),
                     featureNames(.dI()))
          min.dist <- min(abs(input$plotDistclick$y - 
                                exprs(.dI())[j,round(input$plotDistclick$x,0)]))
          which.min.dist <- which(min.dist == abs(input$plotDistclick$y - 
                                                    exprs(.dI())[j,round(input$plotDistclick$x,0)]))
          ## return index
          j[which.min.dist[[1]]]}
      })
      
      ## Multiple points list
      ## Create a list-like object with reactive values
      .prot.plotDist <- reactiveValues(mult=NULL)
      
      ## observe and add new points to prot.plotDist$mult
      observe({
        ## will be empty initially
        if(!is.null(input$plotDistclick)){
          isolate({.prot.plotDist$mult <-
                     c(.prot.plotDist$mult,.min.dist.protein.plotDist())
                   ## remove indices when indices are double-clicked
                   if (length(which((as.vector(table(.prot.plotDist$mult)) > 1)
                                    == TRUE))){
                     .prot.plotDist$mult <- 
                       .prot.plotDist$mult[-which(.prot.plotDist$mult
                                                  == names(which(table(.prot.plotDist$mult)>1)))]}
          })}}) 
      
      ## END: PLOTDIST ##
      
      
      
      ## TAB: QUANTITATION DATA ##
      ## Generate the quantitation data
      output$exprs.radioUI <- renderUI({
        radioButtons("exprs.radio","Select",
                     choices=list("all or"="all",
                                  "selected"="selected"),
                     selected=if(length(.search.ind())){"selected"}else{"all"})
      })
      
      output$MSn.exprs <- renderDataTable({
        ## use cbind to display quantitation data properly
        ## define a new column name " " for the first column to avoid 
        ## an automatically assigned coliumn name
        if(input$exprs.radio == "all"){
          data.exprs <- as.data.frame(
            cbind(" "=row.names(exprs(.dI())),
                  exprs(.dI())))}
        if(input$exprs.radio == "selected"){
          data.exprs <- as.data.frame(
            cbind(" "=row.names(exprs(.dI())),
                  exprs(.dI())))[c(.search.ind()),]}
        data.exprs
      })
      ## END: QUANTITATION DATA ##
      
      
      
      ## TAB: FEATURE META-DATA ##
      ## Generate the feature meta-data
      output$fData.radioUI <- renderUI({
        radioButtons("fData.radio","Select",
                     choices=list("all or"="all",
                                  "selected"="selected"),
                     selected=if(length(.search.ind())){"selected"}else{"all"})
      })
      
      output$MSn.fData <- renderDataTable({
        ## use cbind to display feature meta-data properly
        ## assign a new column name for the first column to avoid 
        ## an automatically assigned column name
        if(input$fData.radio == "all"){
          data.fData <- as.data.frame(
            cbind(" "=row.names(fData(.dI())),
                  fData(.dI())))}
        if(input$fData.radio == "selected"){
          data.fData <- as.data.frame(
            cbind(" "=row.names(fData(.dI())),
                  fData(.dI())))[c(.search.ind()),]}
        data.fData
      })
      ## END: FEATURE META-DATA ##
      
      
      
      ## TAB: SAMPLE META-DATA ##
      ## Generate the sample meta-data
      output$MSn.pData <- renderDataTable(
        ## use cbind to display sample meta-data properly
        ## for the first column name assign a new name to avoid 
        ## any automatically assigned column name
        expr = as.data.frame(
          cbind(" " = row.names(pData(.dI())),
                pData(.dI()))))
      ## END: SAMPLE META-DATA ##
      
      
      
      ## TAB: SEARCH ##
      ## reactive expression for saved searches
      
      ## get object pRolocGUI_SearchResults from
      ## the global environment and poll for changes
     
      .pRolocGUI_SR <- reactivePoll(intervalMillis = 100, 
                     session = NULL,
                     checkFunc = .digestFOI,
                     valueFunc = .readSR)
      
      ## Get the tag names of the list with the saved search results 
      .tags.list <- reactivePoll(intervalMillis = 1000, 
                                   session = NULL,
                                   checkFunc = .digestFOI,
                                   valueFunc = .descrFOI)

   .which.n <- reactive({
     which(input$tag.select.list == substring(descriptionFOI(.pRolocGUI_SR()), 1, 15))[1]
   })
   
   .names.FOI <- reactive({
     if (is.FeaturesOfInterest(.pRolocGUI_SR())){
       fnamesFOI(.pRolocGUI_SR())}else{
     fnamesFOI(.pRolocGUI_SR())[[.which.n()]]}})
   
   .which.names.FOI <- reactive({
     if (is.FeaturesOfInterest(.pRolocGUI_SR())){
       which(match(rownames(.dI()), fnamesFOI(.pRolocGUI_SR())) != "NA")}else{
       which(match(rownames(.dI()), 
        fnamesFOI(.pRolocGUI_SR())[[.which.n()]]) != "NA")}})
   
   ## select Input for the tag names of the list
   output$tagslist.SearchResultUI <- renderUI({
     if (exists("pRolocGUI_SearchResults", .GlobalEnv) | !is.null(.pRolocGUI_SR())){
       selectInput("tag.select.list","Select search result",
                   choices = .tags.list())}
                    })
  
   output$info.savedsearch <- renderText({
     if (exists("pRolocGUI_SearchResults", envir = .GlobalEnv)| !is.null(.pRolocGUI_SR())){
     show.FOI <- .show.FOI(.pRolocGUI_SR(), .dI(), .which.n())
     paste0(show.FOI, sep="\n", collapse="")}else{
       return("pRolocGUI_SearchResults not found in workspace")
     }
   })
     
  output$saved.search.textUI <- renderUI({
    if(exists("pRolocGUI_SearchResults", envir = .GlobalEnv) | !is.null(.pRolocGUI_SR())){
    textInput("saved.search.text", "Description", value="new search result")}})
    
   output$save.lists2SRUI <- renderUI({
    if ((exists("pRolocGUI_SearchResults", envir = .GlobalEnv) | 
          !is.null(.pRolocGUI_SR())) && !is.null(input$saved.search.text)){
    if(!(input$saved.search.text %in% descriptionFOI(.pRolocGUI_SR()))){
   actionButton("save.lists2SR", "Create new features of interest")
 }else{return("name exists already, choose another name")}
   }})
 
 output$init.saveUI <- renderUI({
   if (!exists("pRolocGUI_SearchResults", envir = .GlobalEnv) && is.null(.tags.list())){
     actionButton("init.savedsearch","Initialize saved searches")
   }
 })
 
 observe({
   if (!is.null(input$init.savedsearch) && input$init.savedsearch > 0){
     init.foi <- FeaturesOfInterest(description = "empty",
                        fnames = featureNames(tan2009r1)[0])
     init.coll <- FoICollection()
     init.coll <- addFeaturesOfInterest(init.foi, init.coll)
     assign("pRolocGUI_SearchResults", init.coll, envir = .GlobalEnv)
   }
 })
  
  .newfoi <- reactive({
    input$save.lists2SR
    isolate({
      FeaturesOfInterest(description = input$saved.search.text,
                         fnames = featureNames(.dI())[.search.ind()],
                         object = .dI())})})     
 
   observe({
     if (!is.null(input$save.lists2SR) && input$save.lists2SR > 0 
        && !is.null(.search.ind()) && length(input$saved.search.text)){
        
     if (is.FoICollection(.pRolocGUI_SR()) &&
         "empty" %in% descriptionFOI(.pRolocGUI_SR())){
       oldSR <- FoICollection()} else{
       oldSR <- .pRolocGUI_SR()}
          
          newFOI <- .newfoi()
          
          if (is.FeaturesOfInterest(.pRolocGUI_SR())){
            newColl <- FoICollection() ## create new collection
            ## add old FoI to collection
            newColl <- addFeaturesOfInterest(oldSR, newColl)
            newColl <- isolate({
              input$save.lists2SR
              addFeaturesOfInterest(newFOI, newColl)})
            assign("pRolocGUI_SearchResults", newColl, envir = .GlobalEnv)
          }else {
          newColl <- isolate({
            input$save.lists2SR
            addFeaturesOfInterest(newFOI, oldSR)})
          assign("pRolocGUI_SearchResults", newColl, envir = .GlobalEnv)
          }
      } ## end if
      })## end observe
  }
    
    
    
    ) ## end list
  
  runApp(app)

} ## end function

