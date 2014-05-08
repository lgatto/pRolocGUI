
#########################
## ui helper functions ##
#########################

addResourcePath(prefix="md_vignette", 
               directoryPath="/home/thomas/Documents/Studium/Cambridge/app-1_dev")


.pRn1_setTitlePanel <- function() 
  titlePanel(title="", windowTitle="pRolocVis")

.pRn1_setSidebarPanel <- function(){
  sidebarPanel(
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
                      padding: 15px 19px 15px 19px;}")),
      tags$style(HTML("h4{
                      margin: 3px 0px 5px 0px;}"))
      ),
    
    ## Panel showing up when tab Data is selected
    
    conditionalPanel(
      
      condition = "input.tab1 == 'PCA' |
          input.tab1 == 'quantitation' |
          input.tab1 == 'feature meta-data' |
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
        textInput("level.search", "Search for"),
        ## has to be selected here
        htmlOutput("searchResultsUI"),
        textOutput("search.indUI"),
        actionButton("saveText", "Submit selection"),
        hr(),
        actionButton("resetMult", "Clear features"),
        a(href="/md_vignette/md_vignette.html#display", 
          "Help", 
          target="_blank")
        )
      ),
    conditionalPanel(
      condition = "input.tab1 == 'Data'",
      strong("Welcome to", span("pRolocGUI", style = "color:gray"), 
          ", the interactive visualisation tool 
          for organelle proteomics data."),
      ##a(href="https://www.rstudio.com", "rstudio", target="_blank"),
      br(),
      br(),
      ##p("Please select in the drop down menu
      ##    on the right an example MSnSet or 'own data' 
      ##    and upload your own MSnSet data afterwards 
      ##    by using the 'Browse...' button."),
     ##br(),
      a(href="/md_vignette/md_vignette.html#tabspRolocVisData", 
        "Help", 
        target="_blank")
      ),
    ## Panel showing up when tab PCA is selected
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
        htmlOutput("PCA.legendUI"),
        htmlOutput("PCA.legendposUI"),
        ## zoom slider for x axis
        htmlOutput("xrangeUI"),
        ## zoom slider for y axis
        htmlOutput("yrangeUI"),
        a(href="/md_vignette/md_vignette.html#tabspRolocVisPCA", 
          "Help", 
          target="_blank")
        )
      ),
    ## Panel showing up when tab 'protein profiles' 
    ## is selected
    conditionalPanel(
      condition = "input.tab1 == 'protein profiles'",
      wellPanel(
        h4("Plot"),
        ## drop down menu for quantity of plots
        selectInput("quantityPlotDist",
                    "number of plots to display",
                    choices=c(1:8),selected=4),
        ## drop down menu for 'Select source for 
        ## organelle markers':
        htmlOutput("levelsOrganellesUI"),
        ## drop down menu for 'select organelle 
        ## in orgarnelle markers'
        htmlOutput("organelleMarkerUI"),
        ## drop down menu for 'select source for 
        ## all assigned proteins to the organelle'
        htmlOutput("allOrganellesUI"),
        ## drop down menu for 'select organelle 
        ## in all assigned proteins'
        htmlOutput("organelleAllUI"),
        htmlOutput("numberPlotDistUI"),
        a(href="/md_vignette/md_vignette.html#tabspRolocVisPP", 
          "Help", 
          target="_blank")
      )
    ),
    ## Panel showing up when tab 'quantitation' is selected
    conditionalPanel(
      condition = "input.tab1 == 'quantitation'",
      htmlOutput("exprsRadioUI"),
      a(href="/md_vignette/md_vignette.html#tabspRolocVisExprs", 
        "Help", 
        target="_blank")
      ),
    ## Panel showing up when tab 'feature meta-data' is selected
    conditionalPanel(
      condition = "input.tab1 == 'feature meta-data'",
      htmlOutput("fDataRadioUI"),
      a(href="/md_vignette/md_vignette.html#tabspRolocVisfData", 
        "Help", 
        target="_blank")
      ),
    ## Panel showing up when tab 'sample meta-data' is selected
    conditionalPanel(
      condition = "input.tab1 == 'sample meta-data'",
      a(href="/md_vignette/md_vignette.html#tabspRolocVispData",
        "Help",
        target="_blank")),  
    conditionalPanel(
      condition="input.tab1 == 'search'",
      h4("Searches"),
      htmlOutput("savedSearchTextUI"),
      ## selectInput for choosing between the different 
      ## search Results
      htmlOutput("tagsListSearchResultUI"),
      ## initilialize search
      htmlOutput("init.saveUI"),
      ## action Button
      htmlOutput("save.lists2SRUI"),
      a(href="/md_vignette/md_vignette.html#tabspRolocVisSearch", 
        "Help", 
        target="_blank")
      ),                     
    width = 3
    )
  
  

  
  }

.pRn1_setMainPanel <- function(){
  mainPanel(
    tabsetPanel(
      tabPanel("Data",
        htmlOutput("Data1"),
        htmlOutput("Data2"),
        htmlOutput("Data3")),
      tabPanel("PCA",
        plotOutput("PCA", width = "100%",height="800px",
            clickId = "PCAclick",
            hoverId = "PCAhover",
            hoverDelay = 100,
            hoverDelayType = "throttle"
            ),
        textOutput("hoverProtPCA"),
        downloadButton("plotPCADownload","Download Plot")
        ),
      tabPanel("protein profiles",
        plotOutput("plotdist", width="100%", height="800px",
            clickId = "plotDistclick",
            hoverId = "plotDisthover",
            hoverDelay = 100,
            hoverDelayType = "throttle"
            ),
        textOutput("hoverProtPlotDist"),
        downloadButton("plotDistDownload","Download Plot")
        ),
      tabPanel("quantitation", 
        dataTableOutput("MSnExprs")
        ),
      tabPanel("feature meta-data", 
        dataTableOutput("MSnfData")
        ),
      tabPanel("sample meta-data", 
        dataTableOutput("MSnpData")
        ),
      tabPanel("search",
        verbatimTextOutput("infoSavedSearch")
        ),
      id = "tab1"
    ),
    ##actionButton("closebutton", "Stop pRolocVis"), 
    width=9)
}