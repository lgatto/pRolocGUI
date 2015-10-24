narrowFeatureData <- function(object,
                              n1 = 6, n2 = 6,
                              fcol = "markers") {
    if (length(fcol) > 1)
        fcol <- NULL
    if (is.null(fcol)) {
        i <- selectFeatureData(object)
        fData(object) <- fData(object)[, i]
        return(object)
    }
    n <- n1 + n2
    fv <- fvarLabels(object)
    ln <- length(fv)
    if (ln <= n) return(object)
    i <- c(1:n1, (ln-n2+1):ln)
    if (any(fcol %in% fv) & !any(fcol %in% fv[i]))
        i <- c(1:n1, (ln-n2+2):ln, grep(fcol, fv))
    fData(object) <- fData(object)[, i]
    if (validObject(object))
        return(object)
}


##' Starts a shiny interface to select feature variables to be retained.
##'
##' @title Select feature variables of interest
##' @param object An \code{MSnSet}.
##' @return Updated \code{MSnSet}, containing only selected feature
##'     variables.
##' @author Laurent Gatto
##' @examples
##' \dontrun{
##' library("pRolocdata")
##' data(hyperLOPIT2015)
##' x <- selectFeatureData(hyperLOPIT2015) ## select via GUI
##' head(fData(x))
##' }
selectFeatureData <- function(object) {
    k <- .selectFeatureData(object)
    fData(object) <- fData(object)[, k]
    object
}

.selectFeatureData <- function(object) { sel <- fv <-
    fvarLabels(object) on.exit(return(sel)) ui <- fluidPage(title =
    'Examples of DataTables', sidebarLayout(sidebarPanel(
    checkboxGroupInput('vars', 'Feature variables', as.list(fv),
    selected = sel)), mainPanel(dataTableOutput('fd'))))

    server <- function(input, output) {
        output$fd <- renderDataTable({
            sel <<- input$vars
            fData(object)[, input$vars, drop = FALSE]
        })
    }
    app <- list(ui=ui, server=server)
    runApp(app)
}
