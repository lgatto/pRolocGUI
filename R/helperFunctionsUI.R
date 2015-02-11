#########################
## ui helper functions ##
#########################


.pRn1_setTitlePanel <- function() 
    titlePanel(title="", windowTitle="pRolocVis")

.pRn2_setTitlePanel <- function() 
    titlePanel(title="", windowTitle="pRolocComp")

## START: sidebar Panel ## 
.pR_tags <- function() {
    tags$head(
        tags$style(type="text/css","hr{margin:0;}"),
        tags$style(HTML("#resetMult{
            margin:0;
            padding: 7 10 7 1;
            position: relative;
            display: block;
            width: 100%;}")),
        tags$style(HTML("#saveText{
            margin:0;
            padding: 7 10 7 1;
            position: relative;
            display: block;
            width: 100%;}")),
        tags$style(HTML("#removeText{
            margin:0;
            padding: 7 10 7 1;
            position: relative;
            display: block;
            width: 100%;}")),
        tags$style(HTML("#saveSumMat{
            margin:0;
            padding: 7 10 7 1;
            position: relative;
            display: block;
            width: 100%;}")),
        tags$style(HTML("#removeSumMat{
            margin:0;
            padding: 7 10 7 1;
            position: relative;
            display: block;
            width: 100%;}")),
        tags$style(HTML(".well{
            padding: 15px 18px 12px 18px;
            margin: 10px 5px;}")),
        tags$style(HTML("h4{
            margin: 0px 0px 5px 0px;}"))
        )
}

.pRn2_tags <- function() {
    tags$head(
        tags$style(HTML("#selObj{
            margin: -5px;}")
        )
    )
}

.pRn2_selObj <- function() {
    conditionalPanel(
        condition = "input.tab1 != 'search' && input.tab1 != 'data'",
            wellPanel(
                htmlOutput("selObjUI")
            )
    ) 
}

.pRn2_commonFeat <- function() {
    conditionalPanel(
        condition = "input.tab1 != 'sample meta-data' && 
            input.tab1 != 'search'",
            wellPanel(
                radioButtons("commonFeat", "display",
                             choices = c("all", "common features"), 
                             selected = "all")
            )
    )
}

.pR_condDisplaySelection <- function() {
    conditionalPanel(            
        condition = "input.tab1 == 'PCA' ||
                input.tab1 == 'quantitation' ||
                input.tab1 == 'feature meta-data' ||
                input.tab1 == 'protein profiles'",
        wellPanel(
            h4("Display selection"),
            ## Control Checkboxes, if deactivated points will 
            ## not be plotted
            htmlOutput("checkBoxUI"), 
            ## fvarLabels() of selected MSnSet in which to look for
            htmlOutput("searchUI"),
            ## a search field to facilitate the search for
            ## a certain level which ...
            textInput("levelSearch", "Search for"),
            ## has to be selected here
            htmlOutput("searchResultsUI"),
            ## action Button to add selected levels 
            ## to internal assignment concerning query search
            htmlOutput("saveTextUI"),
            htmlOutput("removeTextUI"),
            tags$hr(),
            ## action Button to delete internal assignments
            htmlOutput("resetMultUI"),
            ## link to help page
            htmlOutput("linkDisplayUI")
        )
    )
}

.pR_condTabData <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'data'",
        ## link to help page
        htmlOutput("linkDataUI")
    )
}

.pRn1_condTabPCA <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'PCA'",
        wellPanel(
            h4("Plot"),
            ## drop down menu for colours of PCA plot
            htmlOutput("fcoloursUI"),
            ## drop down menu for symbol type of PCA plot
            htmlOutput("fsymboltypeUI"),
            ## drow pown menu for point size of PCA plot
            htmlOutput("fcexUI"),
            ## numeric Input for PCAn (first component)
            htmlOutput("PCAn1UI"),
            ## numeric Input for PCAn (second component)
            htmlOutput("PCAn2UI"),
            ## legend, will be added when colours != "none"
            htmlOutput("PCALegendUI"),
            htmlOutput("PCALegendposUI"),
            ## zoom slider for x and y axis
            htmlOutput("xrangeUI"),
            htmlOutput("yrangeUI"),
            ## mirror second object
            ## link to help page
            htmlOutput("linkPCAUI")
        )
    )
}


.pRn2_condTabPCA <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'PCA'",
        wellPanel(
            h4("Plot"),
            ## drop down menu for colours of PCA plot
            htmlOutput("fcoloursOutput"),
            ## drop down menu for symbol type of PCA plot
            htmlOutput("fsymboltypeOutput"),
            ## drow pown menu for point size of PCA plot
            htmlOutput("fcexOutput"),
            ## numeric Input for PCAn (first component)
            htmlOutput("PCAn1UI"),
            ## numeric Input for PCAn (second component)
            htmlOutput("PCAn2UI"),
            ## legend, will be added when colours != "none"
            htmlOutput("PCALegendUI"),
            htmlOutput("PCALegendposUI"),
            ## zoom slider for x and y axis
            htmlOutput("xrange1UI"),
            htmlOutput("xrange2UI"),
            htmlOutput("yrange1UI"),
            htmlOutput("yrange2UI"),
            ## mirror second object
            checkboxGroupInput("mirrorObj", label = "mirror 2nd object",
                         choices = c("x-axis" = "x", "y-axis" = "y"), 
                         selected = NULL),
            ## link to help page
            htmlOutput("linkPCAUI")
        )
    )
}

.pR_condTabProteinProfiles <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'protein profiles'",
        wellPanel(
            h4("Plot"),
            ## drop down menu for quantity of plots
            htmlOutput("quantityPlotDistUI"),
            tags$br(),
            ## drop down menu for 'feature(s) in'
            htmlOutput("allOrganellesUI"),
            ## drop down menu for 'select organelle 
            ## in all assigned proteins'
            htmlOutput("organelleAllUI"),
            htmlOutput("numberPlotDistUI"),
            htmlOutput("linkPPUI")
        )
    )
}

.pR_condTabQuantitation <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'quantitation'",
        wellPanel(
            htmlOutput("exprsRadioUI"),
            htmlOutput("linkExprsUI"))
    )
}

.pR_condTabfData <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'feature meta-data'",
        wellPanel(
            htmlOutput("fDataRadioUI"),
            htmlOutput("linkfDataUI"))
    )
}

.pR_condTabpData <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'sample meta-data'",
        htmlOutput("linkpDataUI")
    )
}

.pR_condTabSearch <- function() {
    conditionalPanel(
        condition="input.tab1 == 'search'",
        wellPanel(
            h4("Searches"),
            helpText(""),
            htmlOutput("savedSearchTextUI"),
            ## initilialize search
            htmlOutput("initSaveUI"),
            ## action Button
            htmlOutput("saveLists2SRUI"),
            br(""),
            htmlOutput("multSaSe"),
            htmlOutput("linkSearchUI")
        )
    )
}

.pR_condTabComp <- function() {
    conditionalPanel(
        condition="input.tab1 == 'data'",
        wellPanel(
            h4("Summary matrix"),
            helpText(""),
            htmlOutput("markerLevel1Output"),
            htmlOutput("markerLevel2Output"),
            hr(),
            helpText(""),
            h4("Selection"),
            helpText(""),
            htmlOutput("selectMarker"),
            radioButtons("compRadio", label = "", 
                choices = c("common", "unique1", "unique2"),
                selected = "common"),
            htmlOutput("saveSumMatUI"),
            htmlOutput("removeSumMatUI")
        ),
        wellPanel(
            h4("Subset MSnSets"),
            helpText(""),
            radioButtons("commonFeat", "Features used",
                choices = c("common", "unique", "common & unique"), 
                selected = "common & unique")
        ),
        htmlOutput("linkDataUI")
    )
}


## END: Sidebar panel ##

## START: Main Panel ## 
    
.pRn1_tabPanelData <- function() {
    tabPanel(
        "data",
        htmlOutput("Data1UI"),
        ## upload function for own data, access to data path implemented 
        ## by index "datapath", 
        ## see ?shiny::fileInput for further details
        fileInput("upload", "Upload MSnSet file",
                  multiple = FALSE),
        htmlOutput("warningUploadUI")
    )
}

.pRn1_tabPanelPCA <- function() {
    tabPanel(
        "PCA",
        plotOutput(
            "PCAUI", 
            width = "100%",height="800px",
            clickId = "PCAclick",
            hoverId = "PCAhover", hoverDelay = 100,
            hoverDelayType = "throttle"
        ),
        tableOutput("hoverProtPCAUI"),
        downloadButton("plotPCADownload","Download Plot")
    )
}

.pRn2_tabPanelPCA <- function() {
    tabPanel("PCA",
            column(width = 6,
                plotOutput("PCA1", width = "100%",height="600px",
                        clickId = "PCA1click",
                        hoverId = "PCA1hover",
                        hoverDelay = 100,
                        hoverDelayType = "throttle"
                ),
                downloadButton("plotPCA1Download", "Download"),
                helpText("")
            ),
            column(width = 6,
                plotOutput("PCA2", width = "100%",height="600px",
                        clickId = "PCA2click",
                        hoverId = "PCA2hover",
                        hoverDelay = 100,
                        hoverDelayType = "throttle"
                ),   
                downloadButton("plotPCA2Download", "Download"),
                helpText("")
            ),
            tableOutput("hoverProt1PCA"),
            tableOutput("hoverProt2PCA")
    )
}

.pRn1_tabPanelProteinProfiles <- function() {
    tabPanel(
        "protein profiles",
        plotOutput(
            "plotDistUI", 
            width="100%", height="800px",
            clickId = "plotDistclick",
            hoverId = "plotDisthover", hoverDelay = 100,
            hoverDelayType = "throttle"
        ),
        tableOutput("hoverProtPlotDistUI"),
        downloadButton("plotDistDownload","Download Plot")
    )
}

.pRn2_tabPanelProteinProfiles <- function() {
    tabPanel(
        "protein profiles",
        column(width = 6,
            plotOutput(
                "plotDist1UI", 
                width="100%", height="800px",
                clickId = "plotDist1click",
                hoverId = "plotDist1hover", hoverDelay = 100,
                hoverDelayType = "throttle"
            ),
            downloadButton("plotDist1Download", "Download"),
            helpText("")
        ),
        column(width = 6,
            plotOutput(
                "plotDist2UI",
                width="100%", height="800px",
                clickId = "plotDist2click",
                hoverId = "plotDist2hover", hoverDelay = 100,
                hoverDelayType = "throttle"
            ),
            downloadButton("plotDist2Download","Download"),
            helpText("")
        ),
        tableOutput("hoverPlotDist1"),
        tableOutput("hoverPlotDist2")
    )
}

.pR_tabPanelQuantitation <- function() {
    tabPanel("quantitation",
             dataTableOutput("MSnExprsUI")
    )
}    

.pR_tabPanelfData <- function() {
    tabPanel("feature meta-data", 
             dataTableOutput("MSnfDataUI")
    )
}

.pR_tabPanelpData <- function() {
    tabPanel("sample meta-data", 
             dataTableOutput("MSnpDataUI")
    )
}

.pR_tabPanelSearch <- function() {
    tabPanel("search",
             ## selectInput for choosing between the different 
             ## search Results
             htmlOutput("tagsListSearchUI"),
             verbatimTextOutput("infoSavedSearchUI")
    )
}

.pR_tabPanelComp <- function() {
    tabPanel("data",
             uiOutput("dataComp")
    )
}

