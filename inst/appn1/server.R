## increase and limit the upload of external data to 20MB
options(shiny.maxRequestSize=20*1024^2)
## load MSnSets from package pRolocdata
data("andy2011")
data("dunkley2006")
data("tan2009r1")

shinyServer(function(input,output,session){
    
    ## start of search implementation
    output$search.UI <- renderUI({
        selectInput("search","",choices=c("protein",colours()))})
  
    searchResults <- reactive({
        subset(if(input$search != "protein"){
            names(table(fData(dataInput())[input$search]))}else{
            row.names(dataInput())},
            grepl(input$level.search,
                if(input$search != "protein"){
                names(table(fData(dataInput())[input$search]))}else{
                row.names(dataInput())}))
    })
  
    output$search.results.UI <- renderUI({
        if(is.null(input$search)){
        return("loading...please wait")
        }else{
        if (length(searchResults()) !=0 ){
        selectInput("search.results","Select your choice",
                    choices=searchResults())
        } else{return("not found")}}
    })
  
    search.ind <- reactive({
        if (input$chooseIdenSearch == "text"){
            search.ind <- unique(prot.text$mult)
        }
        if (input$chooseIdenSearch == "mouse.PCA"){
            search.ind <- which(!is.na(match(rownames(dataInput()),
                                prot.PCA$mult)) == TRUE)
        }
        if (input$chooseIdenSearch == "mouse.plotDist"){
            search.ind <- which(!is.na(match(rownames(dataInput()),
                                prot.plotDist$mult)) == TRUE)
        }
        search.ind
    })
    
    ## Multiple points list for text based search 
    ## (stores indices of searched levels)
    proteins.indices <- reactive({
        if (input$chooseIdenSearch == "text"){
            if (input$search == "protein"){
                which(row.names(dataInput())== input$search.results)}
            else{ 
                which(fData(dataInput())[input$search] == input$search.results)}
        }
    })
    ## Create a vector with reactive values
    prot.text <- reactiveValues(mult=NULL)
    observe({
      if (input$save.text > 0){
      isolate(prot.text$mult <- c(prot.text$mult, proteins.indices()))
      }
    })

    ## Clear multiple points on click
    observe({
        if (input$reset.mult > 0){
            prot.PCA$mult <- NULL
            prot.plotDist$mult <- NULL
            prot.text$mult <- NULL}})
    ## End of searching implementation ##  
  
  
  
    ## TAB: DATA/UPLOAD ##
    ## upload function for own data, access to data path implemented by index 
    ## "datapath", 
    ## see ?fileInput for further details
    dataInputownData <- reactive({
        if (length(as.character(input$owndata["datapath"])) == 0){
            owndata <- andy2011
        }else{
        library(tools)
        if (file_ext(as.character(input$owndata["name"])) %in% c("rda","RData")){
        if (inherits(get(load(as.character(input$owndata["datapath"]))), 
                   "MSnSet") == TRUE){
            owndata <- get(load(as.character(input$owndata["datapath"])))
        }else{owndata <- andy2011}}else{owndata <- andy2011}
        }
        })
    
    output$warningowndataUI <- renderText({
        if (!is.null(input$owndata)){
            if (input$data == "own data" && identical(dataInput(), andy2011)){
                return("no MSnSet selected, 
                       MSnSet Christoforou 2011 will be used")
            }else{return()}
        }else{return()}
    })
    ## use either example data andy2011, dunkley 2006, tan2009 (in pRolocdata) 
    ## or own data and assign it approp)riately
    dataInput <- reactive({
    switch(input$data,
           "Christoforou 2011" = andy2011,
           "Dunkley et al. 2006" = dunkley2006,
           "Tan et al. 2009" = tan2009r1,
           "own data" = dataInputownData())})
    ## END: UPLOAD ##
  
  
  
    ## TAB: PCA PLOT ##
    colours <- reactive({
      if(is.null(colours)){ ## to avoid error messages found on 
      ## http://stackoverflow.com/questions/19673234/shiny-preventing-initial-
      ## error-messages-in-endpoints-while-conductor-executes
      return()} else{
          ## fvarLabels accesses labels of feature data
          fvarLabels(dataInput())}}) 
    ## the UI will be rendered accordingly to colours which is reactive to the 
    ## MSnSet selected
    output$fcoloursOutput <- renderUI({ 
        selectInput("fcolours","colour",c("none",colours()),
                selected="none")})
  
    output$fsymboltypeOutput <- renderUI({ 
        selectInput("fsymboltype","symbol type", c("none",colours()),
                selected="none")})
  
    fcex <- reactive({
        ## check for colums in fData(dataInput()) which are numeric
        col.num <- which(sapply(fData(dataInput()), is.numeric) == TRUE)
        ## write indices in vector col.num
        col.num <- as.vector(col.num)
        if(length(col.num)==0){
            return()
        }else{
            ## return fvarLabels of colums which are numeric
           fvarLabels(dataInput())[col.num]
        }})
  
    output$fcexOutput <- renderUI({
        ## initially length(input$fcolours) == 0
        ## to avoid an error message we have an outer if statement
        if (length(input$fcolours) != 0){
            if (input$fcolours == "none"){
                return()
            }else{
                selectInput("fcex", "point size", c("0.5","1","2",fcex()),
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
                min=min(values.PCA()[,1])-1,max=max(values.PCA()[,1])+1,
                value=c(min(values.PCA()[,1]),max(values.PCA()[,1])))}})  
  
    output$yrangeUI <- renderUI({
        if(is.null(input$PCAn1)){
            return("loading...please wait")
        } else{
    ## get max and min values of second principal component
    ## create a range slider
    sliderInput("yrange","zoom y axis",
                min=min(values.PCA()[,2])-1,max=max(values.PCA()[,2])+1,
                value=c(min(values.PCA()[,2]),max(values.PCA()[,2])))}})
  
    ## select number of principal components to look for in server.R notation
    output$PCAn1UI <- renderUI({
        selectInput("PCAn1","number of 1st principal component",selected=1,
                choices=c(1:ncol(exprs(dataInput()))))})
    output$PCAn2UI <- renderUI({
        selectInput("PCAn2","number of 2nd principal component",selected=2,
                choices=c(1:ncol(exprs(dataInput()))))})
    
    ## create reactive expression with values of PCA
    values.PCA <- reactive({plot2D(dataInput(),fcol=NULL,
                         xlim=c(input$xrange[1],input$xrange[2]),
                         ylim=c(input$yrange[1],input$yrange[2]),
                         dims=c(as.numeric(input$PCAn1),
                         as.numeric(input$PCAn2)),plot=FALSE)})
    
    ## display information about proteins by clicking:
    ## Create a vector where we can store reactive values for this session 
    ## (coordinates in PCA plot)
    coord.PCA <- reactiveValues(x=NULL, y=NULL)
    ## Listen for clicking
    PCA.coordx <- reactive({
        ## will be empty initially
        if (is.null(input$PCAclick$x)){
            return()}else{coord.PCA$x <- input$PCAclick$x}})
    PCA.coordy <- reactive({
        ## will be empty initially
        if (is.null(input$PCAclick$y)){
            return()}else{coord.PCA$y <- input$PCAclick$y}})
      
    min.dist2d.protein <- reactive({
        ## will be empty initially
        if (is.null(input$PCAclick)){
            return()} else{
            ## compute 2D distances from click input to each component 
            ## of the PCA plot
            min.dist2d <- min(sqrt((PCA.coordx()-values.PCA()[,1])^2 + 
                             (PCA.coordy()-values.PCA()[,2])^2))
            ## compute the element (row index, i.e. the protein) which has the 
            ## shortest distance to the input 
            which.min.dist2d <- which(sqrt((PCA.coordx()-values.PCA()[,1])^2 + 
                              (PCA.coordy()-values.PCA()[,2])^2) == min.dist2d)                       
            ## return the name of the protein
            row.names(dataInput())[which.min.dist2d]
            }
    })
  
  
    ## Multiple points list
    ## Create a vector with reactive values
    prot.PCA <- reactiveValues(mult=NULL)
    observe({
        ## will be empty initially
        if(is.null(input$PCAclick)){
             return()}
        isolate(prot.PCA$mult <- c(prot.PCA$mult,min.dist2d.protein()))
    })
  
    ## Print the coordinates in the plot, 
    ## the name of the protein
    output$coord.PCAUI <- renderText({
        c("x:",PCA.coordx(),"y:",PCA.coordy(),min.dist2d.protein())
    })
  
    output$info.prot.PCAUI <- renderTable({
        fData(dataInput())[row.names(fData(dataInput()))==min.dist2d.protein(),]
    })
  
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
        par(mfrow=c(1,1))
    
        if (input$fcolours %in% fvarLabels(dataInput())){
            colour <- input$fcolours} else{
            colour <- NULL}
  
        if (length(input$fcex) != 0){
            if (input$fcex %in% fvarLabels(dataInput())){
                fcex <- fData(dataInput())[,input$fcex]
            }else{fcex <- as.numeric(input$fcex)}
            }else{fcex <- 1}
  
        if (input$fsymboltype == "none"){
            ## create plot2D and assign reactive variables to arguments,
            ## do not assign fpch (no symboltypes are plotted)
            plot2D(dataInput(),fcol=colour,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                as.numeric(input$PCAn2)),
                cex=fcex)
        }else{
            ## create plot2D and assign reactive variables to arguments
            ## take input$fsymboltype for symboltype
            plot2D(dataInput(),fcol=colour, fpch=input$fsymboltype,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                as.numeric(input$PCAn2)),
                cex=fcex)
        }
   
   
        if (input$fcolours %in% fvarLabels(dataInput()) &&
            input$legendyes == TRUE){
             ## add a legend to the plot with reactive variable as arguments
             addLegend(dataInput(), fcol=colour, where=input$legendposition,
                  bty="n", cex=1)}
   
        if(length(search.ind()) != 0){
            if(input$searchyes == TRUE){
                points(x=values.PCA()[search.ind(),1],
                y=values.PCA()[search.ind(),2],
                type="p",col="black",
                pch=1,cex=2,lwd=1.5)
           }
        }
    })
 
 
 
    ## for Plot/Download button (needs a reactive expression)
    PCA.plot.reac <- reactive({
        par(mfrow=c(1,1))
    
        if (input$fcolours %in% fvarLabels(dataInput())){
            colour <- input$fcolours} else{
            colour <- NULL}
  
        if (length(input$fcex) != 0){
            if (input$fcex %in% fvarLabels(dataInput())){
                fcex <- fData(dataInput())[,input$fcex]
            }else{fcex <- as.numeric(input$fcex)}
            }else{fcex <- 1}
  
        if (input$fsymboltype == "none"){
            ## create plot2D and assign reactive variables to arguments,
            ## do not assign fpch (no symboltypes are plotted)
            plot2D(dataInput(),fcol=colour,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                as.numeric(input$PCAn2)),
                cex=fcex)
        }else{
            ## create plot2D and assign reactive variables to arguments
            ## take input$fsymboltype for symboltype
            plot2D(dataInput(),fcol=colour, fpch=input$fsymboltype,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                as.numeric(input$PCAn2)),
                cex=fcex)
        }
   
   
        if (input$fcolours %in% fvarLabels(dataInput()) &&
            input$legendyes == TRUE){
             ## add a legend to the plot with reactive variable as arguments
             addLegend(dataInput(), fcol=colour, where=input$legendposition,
                  bty="n", cex=1)}
   
        if(length(search.ind()) != 0){
            if(input$searchyes == TRUE){
                points(x=values.PCA()[search.ind(),1],
                y=values.PCA()[search.ind(),2],
                type="p",col="black",
                pch=1,cex=2,lwd=1.5)
           }
        }
    })
 
    output$plotPCA.download <- downloadHandler(
       filename = function() {paste(input$data,"-",Sys.Date(), '.png', sep='')},
       content = function(file) {
       png(file)
       print(PCA.plot.reac())
       dev.off()
     })
    ## END: PCA PLOT ##
  
  
 
    ## TAB: PLOTDIST ##
    output$levels.organellesUI <- renderUI({
        selectInput("source.organelle.marker.pldi",
               "Select source for organelle markers (markers)",
               colours())})
 
    output$all.organellesUI <- renderUI({
        selectInput("all.organelle.pldi",
               "Select source for all assigned proteins to the organelle",
               colours(),selected=input$source.organelle.marker.pldi)})
 
    organelle.marker.name <- reactive({
        if (is.null(organelle.marker.name)){
            return()} else{
            names(table(fData(dataInput())[input$source.organelle.marker.pldi]))}})
 
    output$organelle.markerUI <- renderUI({
        if(is.null(input$source.organelle.marker.pldi)){
            return("loading...please wait")} else{
            selectInput("organelle.marker",
                   "Select organelle (select appropriate source for 
                   organelle markers)",choices=organelle.marker.name())}})
 
    organelle.all.name <- reactive({
        if (is.null(organelle.all.name)){
            return()} else{
            names(table(fData(dataInput())[input$all.organelle.pldi]))}})
 
    output$organelle.allUI <- renderUI({
        if(is.null(input$all.organelle.pldi)){
             return("loading...please wait")} else{
             selectInput("organelle.all",
                   "Select organelle (select appropriate source 
                   for all assigned proteins to the organelle)",
                   choices=organelle.all.name(),selected=input$organelle.marker)}})
 
    output$number.plot.distUI <- renderUI({
        if (as.numeric(input$quantity.plot.dist) == 1){
            return()
        }else{
        ## reset all parameters to avoid conflicts when decreasing the 
        ## quantity of plots
        list.params$lev.org.mark <- NULL
        list.params$lev.org.mark.org <- NULL
        list.params$lev.sour.mark.all <- NULL
        list.params$lev.sour.mark.all.org <- NULL
    
        sliderInput("number.plot.dist",
                 "Select the number of plot you want to change",
                 min=1,max=as.numeric(input$quantity.plot.dist),value=1,
                 step=1)}
    })
 
    ## Index in list element
    n.col <- reactive({
        if (is.null(input$number.plot.dist)){
            1}
        as.numeric(input$number.plot.dist)})
    ## create list where parameters for plot are stored
    list.params <- reactiveValues(lev.org.mark = NULL, 
                               lev.org.mark.org = NULL,
                               lev.sour.mark.all = NULL,
                               lev.sour.mark.all.org = NULL)
    ## write paramters in list for plotDist in list
    observe({
        if (is.null(input$organelle.all)){
            return()
        }
        isolate(list.params$lev.org.mark[n.col()] <- 
            c(input$source.organelle.marker.pldi))
        isolate(list.params$lev.org.mark.org[n.col()] <- 
            c(input$organelle.marker))
        isolate(list.params$lev.sour.mark.all[n.col()] <- 
            c(input$all.organelle.pldi))
        isolate(list.params$lev.sour.mark.all.org[n.col()] <- 
            c(input$organelle.all))
    })
 
    ## display information about proteins by clicking:
    ## Create a vector where we can store reactive values for this 
    ## session (coordinates in plotDist plot)
    coord.plotDist <- reactiveValues(x=NULL,y=NULL)
    ## Listen for clicking:
    plotDist.coordx <- reactive({
        ## will be empty initially
        if (is.null(input$plotDistclick$x)){
            return()
        }else{coord.plotDist$x <- round(input$plotDistclick$x,0)}})
    plotDist.coordy <- reactive({
        ## will be empty initially
        if (is.null(input$plotDistclick$y)){
            return()
        }else{coord.plotDist$y <- input$plotDistclick$y}})
 
    min.dist.protein.plotDist <- reactive({
        if (is.null(input$plotDistclick$x)){
            return()
        }else{
            if (input$quantity.plot.dist == "1"){
            ## compute indices of printed proteins in plotDist
            j <- match(subset(featureNames(dataInput()),
                        fData(dataInput())[,list.params$lev.org.mark[1]] ==
                        list.params$lev.org.mark.org[1]),
                        featureNames(dataInput()))
            min.dist <- min(abs(plotDist.coordy() - 
                        exprs(dataInput())[j,plotDist.coordx()]))
            which.min.dist <- which(min.dist == abs(plotDist.coordy() - 
                        exprs(dataInput())[j,plotDist.coordx()]))
       
            row.names(dataInput())[j[which.min.dist[[1]]]]
        }}
    })
 
    ## Multiple points list
    prot.plotDist <- reactiveValues(mult=NULL)
    observe({
        ## will be empty initially
        if (is.null(input$plotDistclick)){
            return()}
        isolate(prot.plotDist$mult <-
            c(prot.plotDist$mult,min.dist.protein.plotDist()))
    })
 
    ## Print the coordinates in the plot,
    ## the name of the protein
    output$coord.plotDistUI <- renderText({
        c(plotDist.coordx(),plotDist.coordy(),min.dist.protein.plotDist())
    })
 
 
    output$plotdist <- renderPlot({
        if(is.null(list.params$lev.org.mark)){
        return()
        }
   
        col <-  if(as.numeric(input$quantity.plot.dist)%%2==0){
            as.numeric(input$quantity.plot.dist)/2}else{
            (as.numeric(input$quantity.plot.dist)+1)/2}
   
        if(as.numeric(input$quantity.plot.dist)==1){
            par(mfrow=c(1,1))}else{par(mfrow=c(2,col))}
   
        ## Actual plotting
        for(i in 1:length(list.params$lev.org.mark)){ 
            ## j (returns indices in fData/exprs)
            j <- match(subset(featureNames(dataInput()),
                       fData(dataInput())[,list.params$lev.org.mark[i]] ==
                       list.params$lev.org.mark.org[i]),
                       featureNames(dataInput()))
            ## which of j are in ind.search(), returns index in j, e.g.
            ## "10", i.e. the tenth element of j is also in ind.search
            ind.col <- na.omit(match(search.ind(),j))
            ## a vector for colours, only create it 
            ## when there are shared elements
            ## AND input$searchyes == TRUE
            if(length(ind.col)!=0 && input$searchyes==TRUE){
                ## create a vector with length(j) 
                ## with the elements "steelblue"
                mcol.ind <- rep("steelblue",length(j))
                ## when there is a shared element in ind.search() and j 
                ## address the colours "black" to it 
                mcol.ind[ind.col] <- "black"
                mcol.ind
            }else{mcol.ind <- "steelblue"}
            ## a vector for lwd, only create it when there are shared elements
            ## AND input$searchyes == TRUE
            if(length(ind.col)!=0 && input$searchyes == TRUE){
                ## create a vector with length(j) with the elements "1"
                lwd.ind <- rep(1,length(j))
                ## when there is a shared element in ind.search() and j address 
                ## lwd 3 to it
                lwd.ind[ind.col] <- 3
                lwd.ind
                }else{lwd.ind <- 1}
     
     
            plotDist(subset(dataInput(), 
                     fData(dataInput())[,list.params$lev.sour.mark.all[i]] == 
                     list.params$lev.sour.mark.all.org[i]), 
                     markers = subset(featureNames(dataInput()), 
                               fData(dataInput())[,list.params$lev.org.mark[i]] == 
                               list.params$lev.org.mark.org[i]),
                     mcol=mcol.ind, lwd=lwd.ind)
            title(list.params$lev.org.mark.org[i])
        } ## end for loop
    })  ## end renderPlot()
 
 
 
    plotDist.reac <- reactive({
        if(is.null(list.params$lev.org.mark)){
        return()
        }
   
        col <-  if(as.numeric(input$quantity.plot.dist)%%2==0){
            as.numeric(input$quantity.plot.dist)/2}else{
            (as.numeric(input$quantity.plot.dist)+1)/2}
   
        if(as.numeric(input$quantity.plot.dist)==1){
            par(mfrow=c(1,1))}else{par(mfrow=c(2,col))}
   
        ## Actual plotting
        for(i in 1:length(list.params$lev.org.mark)){ 
            ## j (returns indices in fData/exprs)
            j <- match(subset(featureNames(dataInput()),
                       fData(dataInput())[,list.params$lev.org.mark[i]] ==
                       list.params$lev.org.mark.org[i]),
                       featureNames(dataInput()))
            ## which of j are in ind.search(), returns index in j, e.g.
            ## "10", i.e. the tenth element of j is also in ind.search
            ind.col <- na.omit(match(search.ind(),j))
            ## a vector for colours, only create it when 
            ## there are shared elements
            ## AND input$searchyes == TRUE
            if(length(ind.col)!=0 && input$searchyes==TRUE){
                ## create a vector with length(j) 
                ## with the elements "steelblue"
                mcol.ind <- rep("steelblue",length(j))
                ## when there is a shared element in ind.search() and j 
                ## address the colours "black" to it 
                mcol.ind[ind.col] <- "black"
                mcol.ind
            }else{mcol.ind <- "steelblue"}
            ## a vector for lwd, only create it when there are shared elements
            ## AND input$searchyes == TRUE
            if(length(ind.col)!=0 && input$searchyes == TRUE){
                ## create a vector with length(j) with the elements "1"
                lwd.ind <- rep(1,length(j))
                ## when there is a shared element in ind.search() and j address 
                ## lwd 3 to it
                lwd.ind[ind.col] <- 3
                lwd.ind
                }else{lwd.ind <- 1}
     
     
            plotDist(subset(dataInput(), 
                     fData(dataInput())[,list.params$lev.sour.mark.all[i]] == 
                     list.params$lev.sour.mark.all.org[i]), 
                     markers = subset(featureNames(dataInput()), 
                               fData(dataInput())[,list.params$lev.org.mark[i]] == 
                               list.params$lev.org.mark.org[i]),
                     mcol=mcol.ind, lwd=lwd.ind)
            title(list.params$lev.org.mark.org[i])
        } ## end for loop
    })  ## end renderPlot()
  
    output$plotDist.download <- downloadHandler(
        filename = function() {paste(input$data,
                               "-","plotDist","-", 
                               Sys.Date(), '.png', sep='')},
        content = function(file) {
        png(file)
        print(plotDist.reac())
        dev.off()
    })
    ## END: PLOTDIST ##
    

 
    ## TAB: QUANTITATION DATA ##
    ## Generate the quantitation data
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
                          exprs(dataInput())))[c(search.ind()),]}
        data.exprs
        },
        options = NULL)
    ## END: QUANTITATION DATA ##
  
  
  
    ## TAB: FEATURE META-DATA ##
    ## Generate the feature meta-data
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
                          fData(dataInput())))[c(search.ind()),]}
        data.fData
        },
        options = NULL)
    ## END: FEATURE META-DATA ##
  
  
  
    ## TAB: SAMPLE META-DATA ##
    ## Generate the sample meta-data
    output$MSn.pData <- renderDataTable(
        ## use cbind to display sample meta-data properly
        ## for the first column name assign a new name to avoid 
        ## any automatically assigned column name
        expr = as.data.frame(
               cbind(" " = row.names(pData(dataInput())),
               pData(dataInput()))),  
        options = NULL)
    ## END: SAMPLE META-DATA ##
  
})
