
shinyServer(function(input,output,session){
    
    
    pathname <- list.files(pattern="[.]R$",
                      path=system.file("SOURCES", package="pRolocGUI"),
                      full.names=TRUE)
    for (i in 1:length(pathname)){
      source(pathname[i], local=TRUE)
    }
    ## start of search implementation
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
    ## End of searching implementation ##  
  
  
  
    ## TAB: DATA/UPLOAD ##
    ## see also: upload.R    
    output$warningowndataUI <- renderText({
        if(input$data == "own data"){
            if (identical(dataInput(), andy2011)){
              return("noMSnSet selected,
                  MSnSet Christoforou 2011 will be used")
        }else{return()}}else{return()}})  
    ## END: UPLOAD ##
  
  
  
    ## TAB: PCA PLOT ##    
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
                choices=c(1:ncol(exprs(dataInput()))))})
    output$PCAn2UI <- renderUI({
        selectInput("PCAn2","number of 2nd principal component",selected=2,
                choices=c(1:ncol(exprs(dataInput()))))})
  
    ## Print fData of the latest selected protein in input PCA  
    #output$info.prot.PCAUI <- renderTable({
     #   fData(dataInput())[.min.dist2d.protein.pca(),]
    #})
  
    output$PCA.legendUI <- renderUI({
        if (length(input$fcolours) != 0){
        if (input$fcolours %in% fvarLabels(dataInput())){
            ## tick box: add legend
            checkboxInput("legendyes","legend of PCA plot", 
                  value = FALSE)}else{return()}}})
  
    output$PCA.legendpositionUI <- renderUI({
         if (length(input$fcolours) != 0){
         if (input$fcolours %in% fvarLabels(dataInput())){
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
    output$PCA <- renderPlot({
        .plotPCA()
    })    
 
    ## Download Handler for PCA plot
    output$plotPCA.download <- downloadHandler(
       filename = function() {paste(input$data,"-",Sys.Date(), '.png', sep='')},
       content = function(file) {
       png(file)
       print(.PCA.plot.reac())
       dev.off()
     })
    ## END: PCA PLOT ##
  
  
 
    ## TAB: PLOTDIST ##
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
                          cbind(" "=row.names(exprs(dataInput())),
                          exprs(dataInput())))}
        if(input$exprs.radio == "selected"){
            data.exprs <- as.data.frame(
                          cbind(" "=row.names(exprs(dataInput())),
                          exprs(dataInput())))[c(.search.ind()),]}
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
                          cbind(" "=row.names(fData(dataInput())),
                          fData(dataInput())))}
        if(input$fData.radio == "selected"){
            data.fData <- as.data.frame(
                          cbind(" "=row.names(fData(dataInput())),
                          fData(dataInput())))[c(.search.ind()),]}
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
               cbind(" " = row.names(pData(dataInput())),
               pData(dataInput()))))
    ## END: SAMPLE META-DATA ##
    
    
    
    ## TAB: SEARCH ##
    
    ## select Input for the tag names of the list
    output$tagslist.SearchResultUI <- renderUI({
      if (exists("pRolocGUI_SearchResults", .GlobalEnv)){
      selectInput("tag.select.list","Select search result",
                  choices = .tags.list(),
                  selected=if(!is.null(.tags.list())){
                    "SearchResult1"})}})
                  
    output$tableSearchResults <- renderDataTable({
      if (input$tag.select.list %in% 
            names(get("pRolocGUI_SearchResults", .GlobalEnv))){
      as.data.frame(fData(
      get("pRolocGUI_SearchResults", .GlobalEnv)[input$tag.select.list][[1]]))}
    })
   
})
