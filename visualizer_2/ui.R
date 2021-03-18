## SILO Dashboard PLUS

########################## Load Libraries ########################## 
## Load libraries
library(shiny)
library(shinyFiles)
library(here)
library(shinydashboard)
library(dplyr) # To handle data
library(plotly) # To produce interactive maps
library(leaflet) #for producing interactive maps
library(sf) #for handling spatial data
library(tmap)
library(readxl) #for reading excel files
library(shinydashboardPlus) ## Extra elements for dashboard
library(DT) ## For table outputs
library(rgdal) ## Handle export shapefiles
library(shinyjs) ## Handle javascript shiny
library(RColorBrewer) #for picking colors
library(tidyr) ## For data manipulation
library(tidyverse) ## For data manipulation
library(tmap)
library(tmaptools)
########################## Data and extra files ########################## 

setwd(here())

## Read external files for extra functions (to be checked)(fix in the latest version)

source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/fileSettings.r", sep ="/"))  ## Load auxiliary files like labels
source(paste(here(),"visualizer_2/SILOVisualizer/functions/figureTableFunctions.R", sep ='/')) #contains functions for creating plots, maps and tables
source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/SiloLogic.r", sep ="/"))
source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/SiloFunctions.r", sep ="/"))
source(paste(getwd(),"visualizer_2/SILOVisualizer/functions/fileReader.r", sep ="/"))


# create colors
msmQualitative <- brewer.pal(12, "Set3")[c(1, 3:12)] #colors used for qualitative attributes
msmSequential <- viridisLite::viridis(10, direction = -1) #colors used for sequential attributes
msmPastel <- brewer.pal(9, "Pastel1") #colors used for pie chart
showRegionalPlots <-TRUE

########################## Header ##########################

ui = dashboardPagePlus( 
    dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "SILO Visualizer 2.0"),
            img(icon("city"))),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "table"
        ),
########################## Sidebar ##########################

    dashboardSidebar( width = 300,
     sidebarMenu(
         menuItem(
            text = "Select scenarios",
            tabName = "scenarios",
            icon = icon("folder"),
            shinyDirButton("dir", "Base scenario folder","Select directory"),
            checkboxInput("comparison","Compare scenarios", value=FALSE),
            conditionalPanel( "input.comparison == true",
                shinyDirButton("dir2","Alternative scenario folder", "Select directory")),
            actionButton("update","Update")
         ),
         menuItem(
            text ="Select visualization",
            tabName = "visualization",
            icon = icon("map"),
            radioButtons("renderType", "Select the visualization type",
                choices = list("Spatial" = "spatial","Aspatial" = "aspatial"))
         ),
         menuItem(
             text ="Select parameters",
             tabName = "parameters",
             icon = icon("list"),
             conditionalPanel("input.renderType == 'spatial'",
                ## Time horizon 
                sliderInput(inputId = "year",label = h4("Select Year"),
                    value = initialYear, min = initialYear, max = finalYear),
                ## Year selection
                radioButtons("zoneAgg", "Select zone aggregation", 
                    choices = c("Dissagregated" = "dissagregated","Aggregated" = "aggregated") , selected = "aggregated"),
                #checkboxInput("zone_level", "View at zone level", value=FALSE),
                ## Spatial conditional panel
                conditionalPanel("input.renderType == 'spatial'",
                    selectInput("spatialLevel", h4("Select spatial attribute"),c("Population" = 'population',
                        "Households" = 'households',
                        "Jobs" = 'jobs',
                        "Available land"='availLand',
                        "Average price" = 'avePrice',
                        "Dwellings" = 'dwellings',
                        "Accessibilites" ='accessibilities',
                        "Income" = 'income'
                        )),
                    conditionalPanel("input.spatialLevel == 'dwellings'",
                        selectInput("sDwelling",h5("Select dwelling level"),(sDwelling))),
                    conditionalPanel("input.spatialLevel == 'income'",
                        selectInput("sIncome", h5("Select income class"), (sIncome))),
                    conditionalPanel("input.spatialLevel == 'accessibilities'",
                        selectInput("sAcc", h5("Select accessibility mode"), (sAccessibility))),
                    conditionalPanel("input.zoneAgg == 'aggregated'",
                        checkboxInput("enable_regions","Enable click on region plots", value = FALSE)),
                    ## View growth
                    
                    #conditionalPanel("input.comparison == true",
                    #    radioButtons("comparisonSelector", h4("Select type of comparison"),
                    #        choices = list("Compare scenarios" = 1,
                    #        "Base year comparison"= 2,
                    #        "No comparison" = 3), selected = 3),
                    #    conditionalPanel("input.comparisonSelector == 3",
                    #    radioButtons("scenarioSelector", h4("Select map scenario"),
                    #        choices = list("Scenario 1" = 1, "Scenario 2" = 2), selected = 1))),
                    numericInput("siloMapCategories", h4("Enter number of categories"), 7, 3, 15, 1),
                    radioButtons("siloMapStyle", h4("Select classification style"), c("pretty", "equal", "quantile"), inline = TRUE)
                    )
                ),
                conditionalPanel("input.renderType == 'aspatial'",
                    selectInput("aspatialLevel","Select aspatial attribute",(aspatialMenu)),
                    conditionalPanel("input.aspatialLevel == 'households'",
                        selectInput("HHLevel", "Select household level", (aHH)),
                        conditionalPanel("input.HHLevel == 'hhSizInc' |input.HHLevel == 'hhRentIncome'",
                            checkboxInput("switchView","Switch View", value=FALSE))),
                    conditionalPanel("input.aspatialLevel == 'persons'",
                        selectInput("personsLevel", "Select person level",(aPerson)),
                        conditionalPanel("input.personsLevel == 'peAgeGend'",
                            checkboxInput("pyramid", "View as population pyramid", value = FALSE))),
                    conditionalPanel("input.aspatialLevel == 'dwellings'",
                        selectInput("dwellingsLevel", "Select dwelling level", (aDwelling))),
                    conditionalPanel("input.aspatialLevel == 'regional'",
                        selectInput("regionalLevel", "Select dwelling level", (aRegional))),
                    conditionalPanel("input.aspatialLevel == 'events'",
                        selectInput("eventsLevel", "Select dwelling level", (aEvent)))
                )
            ),
         menuItem(
             text = "Comparison parameters",
             tabName = "comparisons",
             icon = icon("balance-scale-right"),
             conditionalPanel("input.comparison == true",
                              radioButtons("comparisonSelectorDouble", h4("Select type of comparison"),
                                           choices = list("Compare scenarios" = 1,
                                                          "Base year comparison"= 2,
                                                          "No comparison" = 3), selected = 3),
                              conditionalPanel("input.comparisonSelectorDouble == 3",
                                               radioButtons("scenarioSelector", h4("Select map scenario"),
                                                            choices = list("Scenario 1" = 1, "Scenario 2" = 2), selected = 1))),
             conditionalPanel("input.comparison == false",
                              radioButtons("comparisonSelectorSingle", h4("Select comparison"),
                                           choices = list("Base year comparison" = 2,
                                                          "No comparison" = 3), selected = 3),
                              conditionalPanel("input.comparisonSelectorSingle ==3",
                                               radioButtons("ScenarioSelectorSingle", h4("Selected Scenario"),
                                                            choices = list("Scenario 1" = 1), selected =1))),
             # Virtual panel to show / hide when data is not available
             conditionalPanel(condition = " 5 == 3",
                              checkboxInput("showClickPlot1", "Show click plot 1", value = TRUE),
             
                              checkboxInput("showClickPlot2", "Show click plot 2", TRUE),
                              
                              checkboxInput("showClickPlot3", "Show click plot 3", TRUE),

                              )
             
             #shinyDirButton("dir", "Base scenario folder","Select directory"),
             #checkboxInput("comparison","Compare scenarios", value=FALSE),
             #conditionalPanel( "input.comparison == true",
             #                  shinyDirButton("dir2","Alternative scenario folder", "Select directory")),
             #actionButton("update","Update")
         )
        )
    ),
########################## Body ##########################
    dashboardBody(
        fluidRow(
            boxPlus(
                closable = FALSE,
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                conditionalPanel(condition = "input.renderType == 'spatial'",
                    leafletOutput("siloMap", height = 850)
                ),
                conditionalPanel(condition = "input.renderType == 'aspatial'",
                    plotlyOutput("siloPlot", height = 850))
                )
            ),
        conditionalPanel(condition = "input.enable_regions == 1 && input.zoneAgg == 'aggregated'",
            boxPlus(
                title = ("SILO Regional plots"),
                closable = FALSE,
                status = "primary",
                solidHeader = TRUE,
                width = NULL,
                    fluidRow(column(12,
                        fluidRow(
                            column (4, conditionalPanel(condition = "input.showClickPlot1 == true && input.renderType == 'spatial' ",
                                plotlyOutput("regCommPlot")    
                                )),
                            column (4, conditionalPanel(condition = "input.showClickPlot2 == true && input.renderType == 'spatial'",
                                plotlyOutput("avLandPlot")
                                )),
                            column (4, conditionalPanel(condition = "input.showClickPlot3 == true && input.renderType == 'spatial'", 
                                plotlyOutput("jobsBySectorPlot")
                                )))))
            ))
    ),
########################## Right sidebar ##########################
    rightsidebar = rightSidebar(
        background = "light",
        width = 450,
        rightSidebarTabContent(
            id =1, 
            active = TRUE,
            conditionalPanel(condition = "input.renderType == 'spatial'",
                dataTableOutput('gisTable'),
                downloadButton("downloadData", "Download .csv"),
                downloadButton("downloadShape", "Download GeoJSON (to be implemented)")
                ),
            conditionalPanel(condition = "input.renderType == 'aspatial'",
                dataTableOutput('aspatialTable'),
                downloadButton("downloadAspatialData", "Download .csv")
                )
        )
    )
)