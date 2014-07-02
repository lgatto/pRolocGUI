## reactive expressions for PCA plot and
## function for PCA plot

## colours
.colours <- reactive({
    if(!is.null(colours)){ 
        ## fvarLabels accesses labels of feature data
        fvarLabels(dataInput())} 
})

## point size
.fcex <- reactive({
  ## check for colums in fData(dataInput()) which are numeric
  col.num <- which(sapply(fData(dataInput()), is.numeric) == TRUE)
  ## write indices in vector col.num
  col.num <- as.vector(col.num)
  if(!length(col.num)==0){
      ## return fvarLabels of colums which are numeric
      fvarLabels(dataInput())[col.num]}
  })

## values of PCA, dims is dependent on user input.
## so is xlim and ylim
.values.PCA <- reactive({
    plot2D(dataInput(),fcol=NULL,
        xlim=c(input$xrange[1],input$xrange[2]),
        ylim=c(input$yrange[1],input$yrange[2]),
        dims=c(as.numeric(input$PCAn1),
        as.numeric(input$PCAn2)),plot=FALSE)})

## PCA plot, legend, points
.plotPCA <- function(){
    par(mfrow=c(1,1))
    
    if (input$fcolours %in% fvarLabels(dataInput())){
    colour <- input$fcolours} else{
      colour <- NULL}
  
    if (length(input$fcex) != 0){
        if (input$fcex %in% fvarLabels(dataInput())){
            fcex <- fData(dataInput())[,input$fcex]
        }else{fcex <- as.numeric(input$fcex)}
    }else{fcex <- 1}
    
    if (!is.null(input$xrange)){
        if (input$fsymboltype == "none"){
            ## create plot2D and assign reactive variables to arguments,
            ## do not assign fpch (no symboltypes are plotted)
            plot2D(dataInput(),fcol=colour,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                       as.numeric(input$PCAn2)),
                cex=fcex)}else{
            ## create plot2D and assign reactive variables to arguments
            ## take input$fsymboltype for symboltype
            plot2D(dataInput(),fcol=colour, fpch=input$fsymboltype,
                xlim=c(input$xrange[1],input$xrange[2]),
                ylim=c(input$yrange[1],input$yrange[2]),
                dims=c(as.numeric(input$PCAn1),
                       as.numeric(input$PCAn2)),
                cex=fcex)}}

   if (input$fcolours %in% fvarLabels(dataInput()) && input$legendyes){
       ## add a legend to the plot with reactive variable as arguments
       addLegend(dataInput(), fcol=colour, where=input$legendposition,
                 bty="n", cex=1)}
  
   if(length(.search.ind()) != 0){
       if(length(input$chooseIdenSearch)){
           points(x=.values.PCA()[.search.ind(),1],
                  y=.values.PCA()[.search.ind(),2],
                  type="p",col="black",
                  pch=1,cex=2,lwd=1.5)}}
    } ## end .plotPCA()

## for Plot/Download button (needs a reactive expression)
.PCA.plot.reac <- reactive({.plotPCA()})

