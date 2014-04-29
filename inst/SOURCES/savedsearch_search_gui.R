## reactive expression for saved searches

## get object pRolocGUI_SearchResults from
## the global environment
.pRolocGUI_SR <- reactive({
  if (input$save.lists2SR > 0){
    get("pRolocGUI_SearchResults", .GlobalEnv)}
  else{get("pRolocGUI_SearchResults", .GlobalEnv)}
})

## Get the tag names of the list with the saved search results 
.tags.list <- reactive({ 
  if(input$save.lists2SR > 0){
    c(names(get("pRolocGUI_SearchResults", .GlobalEnv)), 
      paste("SearchResult", .n.list(), sep=""))}else{
        names(get("pRolocGUI_SearchResults", .GlobalEnv))}
})

## create a list-like object with reactive values
.pRolocGUI_SearchResults_sh <- reactiveValues(searchresult = NULL)
## observe and write selected points (.search.ind()) 
## to .pRolocGUI_SearchResults_sh
observe({
  if (!is.null(.search.ind()) | length(.search.ind()) != 0){
    .pRolocGUI_SearchResults_sh$searchresult <- 
      dataInput()[c(.search.ind()),]}
})

## length of list
.n.list <- reactive({
  n.list <- length(.pRolocGUI_SR()) #+ 1
  as.numeric(n.list)})

## concatenate new search results to the list
observe({
  if (input$save.lists2SR > 0){
    if (!is.null(.search.ind())){
      pRolocGUI_SR <- c(.pRolocGUI_SR(), 
                        .pRolocGUI_SearchResults_sh$searchresult)
      name_sr <- paste("SearchResult", .n.list(), sep="")
      names(pRolocGUI_SR)[.n.list()+1] <- name_sr
      assign("pRolocGUI_SearchResults", pRolocGUI_SR, envir = .GlobalEnv)
      get("pRolocGUI_SearchResults", .GlobalEnv)
    }}})

.saved.search.results <- reactive({
    if (input$save.lists.radio == "no"){
      search.results <- NULL
    }else{
    if (!is.null(input$tag.select.list) &&
        length(na.omit(match(rownames(get("pRolocGUI_SearchResults", .GlobalEnv)[input$tag.select.list][[1]]),
                              rownames(dataInput()))))){
      search.results <- na.omit(match(rownames(get("pRolocGUI_SearchResults", .GlobalEnv)[input$tag.select.list][[1]]),
                                      rownames(dataInput())))}else{search.results <- NULL}
      
    }
    search.results
})

