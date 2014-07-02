library(shiny)
## Define UI
shinyUI(fluidPage(
  
    ## Application title
    titlePanel("pRolocGUI"),
  
    ## Sidebar
    sidebarPanel(
        ## Panel showing up when tab Data is selected
        conditionalPanel(condition="input.tab1 == 'PCA' |
                         input.tab1 == 'quantitation' |
                         input.tab1 == 'feature meta-data' |
                         input.tab1 == 'protein profiles'",
                         ## Control Checkbox, if deactivated selected points
                         ## will not appear in plot
                         checkboxInput("searchyes",
                                       "Search by and display ...",
                                       value = FALSE),
                         radioButtons("chooseIdenSearch",label="",
                                      choices=c(
                                      "cursor input (PCA)"="mouse.PCA",
                                      "cursor input (protein profiles)"="mouse.plotDist",
                                      "levels(s) in ..."="text"),
                                      selected="text"),
                         ## fvarLabels() of selected MSnSet in which to look for
                         htmlOutput("search.UI"),
                         ## a search field to facilitate the search for
                         ## a certain level which ...
                         textInput("level.search", "Search for"),
                         ## has to be selected here
                         htmlOutput("search.results.UI"),
                         textOutput("search.indUI"),
                         actionButton("save.text","Submit selected levels "),
                         actionButton("reset.mult","Clear all multiple points")),
    conditionalPanel(condition="input.tab1 == 'Data'",
                     helpText("Welcome to this interactive visualisation tool 
                              for organelle proteomics data. Please select 
                              on the right an example MSnSet or 'own data' 
                              in the drop down menu and upload your own 
                              MSnSet data afterwards by using 
                              the 'Browse...' button.")),
    ## Panel showing up when tab PCA is selected
    conditionalPanel(condition="input.tab1 == 'PCA'",
                     helpText("..........................................."),
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
                     htmlOutput("PCA.legendpositionUI"),
                     ## zoom slider for x axis
                     htmlOutput("xrangeUI"),
                     ## zoom slider for y axis
                     htmlOutput("yrangeUI")
    ),
    ## Panel showing up when tab 'quantitation' is selected
    conditionalPanel(condition="input.tab1 == 'quantitation'",
                     helpText("quantitation Data for the selected MSnSet"),
                     radioButtons("exprs.radio","Select",
                                  choices=list("all or"="all",
                                               "selected"="selected"),
                                  selected="all")),
    ## Panel showing up when tab 'feature meta-data' is selected
    conditionalPanel(condition="input.tab1 == 'feature meta-data'",
                     helpText("feature meta-data for the selected MSnSet"),
                     radioButtons("fData.radio","Select",
                                  choices=list("all or"="all",
                                               "selected"="selected"),
                                  selected="all")),
    ## Panel showing up when tab 'sample meta-data' is selected
    conditionalPanel(condition="input.tab1 == 'sample meta-data'",
                     helpText("sample meta-data for the selected MSnSet")),
    ## Panel showing up when tab 'protein profiles' 
    ## is selected
    conditionalPanel(condition=c("input.tab1 == 
                                'protein profiles'"),
                     helpText("..........................................."),
                     ## drop down menu for quantity of plots
                     selectInput("quantity.plot.dist",
                                 "number of plots to display",
                                 choices=c(1:8),selected=4),
                     ## drop down menu for 'Select source for 
                     ## organelle markers':
                     htmlOutput("levels.organellesUI"),
                     ## drop down menu for 'select organelle 
                     ## in orgarnelle markers'
                     htmlOutput("organelle.markerUI"),
                     ## drop down menu for 'select source for 
                     ## all assigned proteins to the organelle'
                     htmlOutput("all.organellesUI"),
                     ## drop down menu for 'select organelle 
                     ## in all assigned proteins'
                     htmlOutput("organelle.allUI"),
                     htmlOutput("number.plot.distUI")),
                     width = 3
    ),
  
    ## Main Panel
    mainPanel(
        tabsetPanel(
          tabPanel("Data",
               ## choose Data source, 
               ## a drop down list of A. Christoforou 2011, 
               ## Dunkley 2006, Tan et al. 2009 (all example data)
               ## or use your own data by selecting load data
               selectInput("data","Choose MSnSet data source:",
                           choices = c("Christoforou 2011","Dunkley et al. 2006",
                                       "Tan et al. 2009","own data"),
                                       selected="Christoforou 2011"),
               fileInput("owndata", "If you have chosen 'own data' 
                         select your own MSnSet file", ## upload your own data
                         accept=c('.rda','data/rda','.RData','data/RData'),
                         multiple = FALSE),
               textOutput("warningowndataUI")
               ),
        tabPanel("PCA",
               plotOutput("PCA",width="100%",height="600px",
                           clickId = "PCAclick",
                           ## hoverId = "PCAhover",
                           ## hoverDelay = 300,
                           ## hoverDelayType = "throttle"
                          ),
               verbatimTextOutput("coord.PCAUI"),
               tableOutput("info.prot.PCAUI"),
               downloadButton("plotPCA.download","Download Plot")
               ),
        tabPanel("protein profiles",
               plotOutput("plotdist",width="100%",height="800px",
                          clickId = "plotDistclick"),
               textOutput("coord.plotDistUI"),
               downloadButton("plotDist.download","Download Plot")),
        tabPanel("quantitation", dataTableOutput("MSn.exprs")),
        tabPanel("feature meta-data", dataTableOutput("MSn.fData")),
        tabPanel("sample meta-data", dataTableOutput("MSn.pData")),
                  id = "tab1"
               ), 
        width=9)
)
)
