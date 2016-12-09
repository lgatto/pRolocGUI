## reactive expressions for text based search

## levels to search
.searchResults.text <- reactive({
    subset(
        if(input$search != "protein"){
            names(table(fData(dataInput())[input$search]))}
        else{
            row.names(dataInput())}, 
        grepl(input$level.search,
            if(input$search != "protein"){
                names(table(fData(dataInput())[input$search]))}
            else{
                row.names(dataInput())}))
    })

## Multiple points list for text based search 
## (stores indices of searched levels)
.proteins.indices <- reactive({
    if ("text" %in% input$chooseIdenSearch){
        if (input$search == "protein"){
            which(rownames(dataInput())== input$search.results)}
        else{ 
            which(fData(dataInput())[input$search] == input$search.results)}
    }})

## vector with reactive values
.prot.text <- reactiveValues(mult=NULL)

## observe indices and concatenate to prot.text$mult
observe({
    if (input$save.text > 0){
        isolate(.prot.text$mult <- c(.prot.text$mult, .proteins.indices()))
    }})