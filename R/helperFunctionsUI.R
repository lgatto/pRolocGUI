
#########################
## ui helper functions ##
#########################

.pRn1_setTitlePanel <- function() 
  titlePanel("pRolocGUI")

.pRn1_setSidebarPanel <- function(){
  sidebarPanel(
    ## Panel showing up when tab Data is selected
    conditionalPanel(
      condition = "input.tab1 == 'PCA' |
          input.tab1 == 'quantitation' |
          input.tab1 == 'feature meta-data' |
          input.tab1 == 'protein profiles'",
      h4("Search"),
      ## Control Checkboxes, if deactivated points will 
      ## not be plotted
      checkboxGroupInput("chooseIdenSearch", 
          label = "",
          choices = c("PCA" = "mouse.PCA",
            "protein profiles" = "mouse.plotDist",
            "saved searches" = "saved.searches",
            "level in" = "text"),
          selected=NULL),
      ## fvarLabels() of selected MSnSet in which to look for
      htmlOutput("searchUI"),
      ## a search field to facilitate the search for
      ## a certain level which ...
      textInput("level.search", "Search for"),
      ## has to be selected here
      htmlOutput("searchResultsUI"),
      textOutput("search.indUI"),
      actionButton("saveText", "Submit selected level"),
      actionButton("resetMult", "Clear multiple points")),
    conditionalPanel(
      condition = "input.tab1 == 'Data'",
      strong("Welcome to", span("pRolocGUI", style = "color:gray"), 
          ", the interactive visualisation tool 
          for organelle proteomics data."),
      br(),
      br(),
      p("Please select in the drop down menu
          on the right an example MSnSet or 'own data' 
          and upload your own MSnSet data afterwards 
          by using the 'Browse...' button.")
      ),
    ## Panel showing up when tab PCA is selected
    conditionalPanel(
      condition = "input.tab1 == 'PCA'",
      hr(),
      h4("Display"),
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
      htmlOutput("yrangeUI")
      ),
    ## Panel showing up when tab 'quantitation' is selected
    conditionalPanel(
      condition = "input.tab1 == 'quantitation'",
      hr(),
      htmlOutput("exprsRadioUI")
      ),
    ## Panel showing up when tab 'feature meta-data' is selected
    conditionalPanel(
      condition = "input.tab1 == 'feature meta-data'",
      hr(),
      htmlOutput("fDataRadioUI")
      ),
    ## Panel showing up when tab 'protein profiles' 
    ## is selected
    conditionalPanel(
<<<<<<< HEAD
      condition = "input.tab1 == 'protein profiles'",
      hr(),
      h4("Display"),
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
      htmlOutput("numberPlotDistUI")
      ),
=======
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
>>>>>>> master
    conditionalPanel(
      condition="input.tab1 == 'search'",
      h4("Display"),
      htmlOutput("savedSearchTextUI"),
      ## selectInput for choosing between the different 
      ## search Results
      htmlOutput("tagsListSearchResultUI"),
      ## initilialize search
      htmlOutput("init.saveUI"),
      ## action Button
      htmlOutput("save.lists2SRUI")
      ),                     
    width = 3
    )
}

.pRn1_setMainPanel <- function(){
  mainPanel(
    tabsetPanel(
      tabPanel("Data",
        ## choose Data source, 
        ## a drop down list of A. Christoforou 2011, 
        ## Dunkley 2006, Tan et al. 2009 (all example data)
        ## or use your own data by selecting load data
          selectInput("data",
              "Choose MSnSet data source:",
              choices = c("Christoforou 2011", "Dunkley et al. 2006",
                "Tan et al. 2009", "own data"),
              selected="Christoforou 2011"),
          fileInput("owndata", 
              "If you have chosen 'own data' select your own MSnSet file", ## upload your own data
              ## accept=c('.rda', 'data/rda', '.RData', 'data/RData'),
              multiple = FALSE),
          textOutput("warningowndataUI")
        ),
      tabPanel("PCA",
        plotOutput("PCA", width = "100%",height="800px",
            clickId = "PCAclick",
            ## hoverId = "PCAhover",
            ## hoverDelay = 300,
            ## hoverDelayType = "throttle"
            ),
        downloadButton("plotPCADownload","Download Plot")
        ),
      tabPanel("protein profiles",
        plotOutput("plotdist", width="100%", height="800px",
            clickId = "plotDistclick"),
        downloadButton("plotDistDownload","Download Plot")
<<<<<<< HEAD
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
    #actionButton("closebutton", "Stop pRolocGUI"), 
    width=9)
}
=======
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
    tabPanel("Data",
             uiOutput("dataComp")
    )
}

>>>>>>> master
