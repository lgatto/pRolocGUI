## reactive expressions for search based on cursor input 
## for protein profiles (plotDist)

## calculate protein nearest to user input
.min.dist.protein.plotDist <- reactive({
  if (!is.null(input$plotDistclick$x) && input$quantity.plot.dist == "1"){
    ## compute indices of printed proteins in plotDist
    j <- match(subset(featureNames(dataInput()),
         fData(dataInput())[,.list.params$lev.org.mark[1]] ==
            .list.params$lev.org.mark.org[1]),
         featureNames(dataInput()))
    min.dist <- min(abs(input$plotDistclick$y - 
        exprs(dataInput())[j,round(input$plotDistclick$x,0)]))
    which.min.dist <- which(min.dist == abs(input$plotDistclick$y - 
        exprs(dataInput())[j,round(input$plotDistclick$x,0)]))
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
