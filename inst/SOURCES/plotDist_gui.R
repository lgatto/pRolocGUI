## reactive expressions for plotDist

## organelle marker name
.organelle.marker.name <- reactive({
    if (is.null(.organelle.marker.name)){
        return()}
    else{
        names(table(fData(dataInput())[input$source.organelle.marker.pldi]))}})

## organelle for all name
.organelle.all.name <- reactive({
  if (is.null(.organelle.all.name)){
    return()} 
  else{
      names(table(fData(dataInput())[input$all.organelle.pldi]))}})

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
            j <- match(subset(featureNames(dataInput()),
                fData(dataInput())[,.list.params$lev.org.mark[i]] ==
                    .list.params$lev.org.mark.org[i]),
                featureNames(dataInput()))
            ## which of j are in ind.search(), returns index in j, e.g.
            ## "10", i.e. the tenth element of j is also in ind.search
            ind.col <- na.omit(match(.search.ind(),j))
            ## a vector for colours, only create it 
            ## when there are shared elements
            ## AND (length(input$chooseIdenSearch)) > 0
            if(length(ind.col)!=0 && length(input$chooseIdenSearch)){
                ## create a vector with length(j) 
                ## with the elements "steelblue"
                mcol.ind <- rep("steelblue",length(j))
                ## when there is a shared element in ind.search() and j 
                ## address the colours "black" to it 
                mcol.ind[ind.col] <- "black"
                mcol.ind}else{mcol.ind <- "steelblue"}
            ## a vector for lwd, only create it when there are 
            ## shared elements AND length(input$chooseIdenSearch) > 0
            if(length(ind.col)!=0 && length(input$chooseIdenSearch)){
                ## create a vector with length(j) with the elements "1"
                lwd.ind <- rep(1,length(j))
                ## when there is a shared element in ind.search() and 
                ## j address lwd 3 to it
                lwd.ind[ind.col] <- 3
                lwd.ind}else{lwd.ind <- 1}
      
            plotDist(subset(dataInput(), 
                fData(dataInput())[,.list.params$lev.sour.mark.all[i]] == 
                    .list.params$lev.sour.mark.all.org[i]), 
                markers = subset(featureNames(dataInput()), 
                    fData(dataInput())[,.list.params$lev.org.mark[i]] == 
                        .list.params$lev.org.mark.org[i]),
                mcol=mcol.ind, lwd=lwd.ind)
            title(.list.params$lev.org.mark.org[i])
        } ## end for loop
    }}

## for Plot/Download button (needs a reactive expression)
.plotDist.reac <- reactive({.plotplotDist()}) 
