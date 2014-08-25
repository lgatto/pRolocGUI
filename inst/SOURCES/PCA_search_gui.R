## reactive expressions for search based on cursor input for PCA

.min.dist2d.protein.pca <- reactive({
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
        isolate({.prot.PCA$mult <- c(.prot.PCA$mult,.min.dist2d.protein.pca())
            ## remove indices when indices are double-clicked
            if (length(which((as.vector(table(.prot.PCA$mult)) > 1) 
                                 == TRUE))){
                .prot.PCA$mult <- 
                    .prot.PCA$mult[-which(.prot.PCA$mult 
                                 == names(which(table(.prot.PCA$mult)>1)))]}
            })}})
