## reactive expressions for upload of MSnSets

## upload function for own data, access to data path implemented by index 
## "datapath", see ?shiny::fileInput for further details
dataInputownData <- reactive({
    if (length(as.character(input$owndata["datapath"])) == 0){
        owndata <- andy2011
    }else{
        library(tools)
        ## check if MSnSet has ending .rda or .RData and if it is MSnSet
        if (file_ext(as.character(input$owndata["name"])) %in% c("rda","RData")){
            if (inherits(get(load(as.character(input$owndata["datapath"]))), 
                   "MSnSet") == TRUE){
                owndata <- get(load(as.character(input$owndata["datapath"])))
            }else{owndata <- andy2011}}else{owndata <- andy2011}
  }})

## use either example data andy2011, dunkley 2006, tan2009 (in pRolocdata) 
## or own data and assign it appropriately
dataInput <- reactive({
  switch(input$data,
         "Christoforou 2011" = andy2011,
         "Dunkley et al. 2006" = dunkley2006,
         "Tan et al. 2009" = tan2009r1,
         "own data" = dataInputownData())
  })


