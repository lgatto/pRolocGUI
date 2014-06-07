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
        tags$style(HTML(".well{
            padding: 15px 18px 12px 18px;}")),
        tags$style(HTML("h4{
            margin: 0px 0px 5px 0px;}"))
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
            actionButton("saveText", "Submit selection"),
            tags$hr(),
            ## action Button to delete internal assignments
            actionButton("resetMult", "Clear features"),
            ## link to help page
            htmlOutput("linkDisplayUI")
        )
    )
}

.pR_condTabData <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'Data'",
        strong("Welcome to", span("pRolocVis", style = "color:gray"), 
            ", an interactive visualisation tool 
            for organelle proteomics data."),
        helpText(""),
        ## link to help page
        htmlOutput("linkDataUI")
    )
}

.pR_condTabPCA <- function() {
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
            radioButtons("selObj", "select plot", 
                        choices = c("object1", "object2"), selected = "object1"),
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
            htmlOutput("xrangeUI"),
            htmlOutput("yrangeUI"),
            ## link to help page
            htmlOutput("linkPCA")
        )
    )
}

.pR_condTabProteinProfiles <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'protein profiles'",
        wellPanel(
            h4("Plot"),
            ## drop down menu for quantity of plots
            selectInput("quantityPlotDist",
                    "number of plots to display",
                    choices=c(1:8), selected=1),
            tags$br(),
            ## drop down menu for 'select source for 
            ## all assigned proteins to the organelle'
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
        htmlOutput("exprsRadioUI"),
        htmlOutput("linkExprsUI")
    )
}

.pR_condTabfData <- function() {
    conditionalPanel(
        condition = "input.tab1 == 'feature meta-data'",
        htmlOutput("fDataRadioUI"),
        htmlOutput("linkfDataUI")
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
        h4("Searches"),
        htmlOutput("savedSearchTextUI"),
        ## selectInput for choosing between the different 
        ## search Results
        htmlOutput("tagsListSearchUI"),
        ## initilialize search
        htmlOutput("initSaveUI"),
        ## action Button
        htmlOutput("saveLists2SRUI"),
        htmlOutput("linkSearchUI")
    )
}


## END: Sidebar panel ##

## START: Main Panel ## 
    
.pR_tabPanelData <- function() {
    tabPanel(
        "Data",
        htmlOutput("Data1UI"),
        htmlOutput("Data2UI"),
        htmlOutput("Data3UI")
    )
}

.pR_tabPanelPCA <- function() {
    tabPanel(
        "PCA",
        plotOutput(
            "PCAUI", 
            width = "100%",height="800px",
            clickId = "PCAclick",
            hoverId = "PCAhover", hoverDelay = 100,
            hoverDelayType = "throttle"
        ),
        textOutput("hoverProtPCAUI"),
        downloadButton("plotPCADownload","Download Plot")
    )
}

.pRn2_tabPanelPCA <- function() {
    tabPanel("PCA",
             verbatimTextOutput("helpPCA"),
             plotOutput("PCA1", width = "100%",height="600px",
                        clickId = "PCA1click",
                        hoverId = "PCA1hover",
                        hoverDelay = 100,
                        hoverDelayType = "throttle"
             ),
             textOutput("hoverProt1PCA"),
             plotOutput("PCA2", width = "100%",height="600px",
                        clickId = "PCA2click",
                        hoverId = "PCA2hover",
                        hoverDelay = 100,
                        hoverDelayType = "throttle"
             ),
             textOutput("hoverProt2PCA"),                     
             downloadButton("plotPCADownload","Download Plot")   
    )
}

.pR_tabPanelProteinProfiles <- function() {
    tabPanel(
        "protein profiles",
        plotOutput(
            "plotDistUI", 
            width="100%", height="800px",
            clickId = "plotDistclick",
            hoverId = "plotDisthover", hoverDelay = 100,
            hoverDelayType = "throttle"
        ),
        textOutput("hoverProtPlotDistUI"),
        downloadButton("plotDistDownload","Download Plot")
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
             verbatimTextOutput("infoSavedSearchUI")
    )
}

