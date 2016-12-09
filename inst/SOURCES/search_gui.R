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
                    search.ind <- .saved.search.results()}}
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
                    search.ind <- c(search.ind, .saved.search.results())}}
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