## SILO Dashboard

## Set libraries

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

## Set working directory

setwd(here())

## Read external files for extra functions (to be checked)(fix in the latest version)

source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/fileSettings.r", sep ="/"))  ## Load auxiliary files like labels, seetings and shapefiles
source(paste(here(),"visualizer/figureTableFunctions.R", sep ='/')) #contains functions for creating plots, maps and tables
source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/SiloLogic.r", sep ="/"))

## Dashboard

ui = dashboardPagePlus(
    dashboardHeaderPlus(
        title = "SILO Visualizer 2.0",
        enable_rightsidebar = TRUE,
        fixed =TRUE,
        rightSidebarIcon = "table"),
    
    dashboardSidebar( width = 300,

    ## Title
    h4("Select folders with the input files"),
    ## Folder selector
    shinyDirButton("dir", "Base scenario folder","Select directory"),
        checkboxInput("comparison","Compare scenarios", value=FALSE),
        conditionalPanel( "input.comparison == true",
            shinyDirButton("dir2","Alternative scenario folder", "Select directory")
        ),
    actionButton("update","Update"),
    ## Radio buttons for choose the representation
    radioButtons("renderType", h4("Select the visualization type"),
        choices = list("Spatial" = "spatial",
        "Aspatial" = "aspatial")),
    ## Slider for year selection and disagregated map in Spatial visualizations
    conditionalPanel("input.renderType == 'spatial'",
    ## Time horizon (update with model parameters)
        sliderInput(inputId = "year",label = h4("Select Year"),
        value = 2020, min = initialYear, max = finalYear),
        checkboxInput("zone_level", "View at zone level", value=FALSE)
    ),
    ## Select box input (depends on the radio button)
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
                             ## View growth
                             conditionalPanel("input.comparison == true",
                                              radioButtons("comparisonSelector", h4("Select type of comparison"),
                                                           choices = list("Compare scenarios" = 1,
                                                                          "Base year comparison"= 2,
                                                                          "No comparison" = 3), selected = 3),
                                              conditionalPanel("input.comparisonSelector == 3",
                                                               radioButtons("scenarioSelector", h4("Select map scenario"),
                                                                            choices = list("Scenario 1" = 1, "Scenario 2" = 2), selected = 1))
                                              ),
                             numericInput("siloMapCategories", h4("Enter number of categories"), 7, 3, 15, 1),
                             radioButtons("siloMapStyle", h4("Select classification style"), c("pretty", "equal", "quantile"), inline = TRUE)
                             
                             
                            ),
    
            conditionalPanel("input.renderType == 'aspatial'",
                             selectInput("aspatialLevel","Select aspatial attribute", c("Overview" = 'overview',
                                                                                        "Households" = 'households',
                                                                                        "Persons" = 'persons',
                                                                                        "Dwellings" = 'dwellings',
                                                                                        "Regional" = 'regional',
                                                                                        "Events" = 'events')),
                             conditionalPanel("input.aspatialLevel == 'households'",
                                              selectInput("HHLevel", "Select house level", (aHH))),
                             conditionalPanel("input.aspatialLevel == 'persons'",
                                              selectInput("personsLevel", "Select person level",(aPerson))),
                             conditionalPanel("input.aspatialLevel == 'dwellings'",
                                              selectInput("dwellingsLevel", "Select dwelling level", (aDwelling))),
                             conditionalPanel("input.aspatialLevel == 'regional'",
                                              selectInput("dwellingsLevel", "Select dwelling level", (aRegional))),
                             conditionalPanel("input.aspatialLevel == 'events'",
                                              selectInput("dwellingsLevel", "Select dwelling level", (aEvent)))
                             )
            
    ),
## Dashboard body
    dashboardBody(
        tabsetPanel(
            tabPanel("Graph", height = "100%",
                conditionalPanel(condition = "input.renderType == 'spatial'",
                    leafletOutput("siloMap", height = 980))
                #,
                #conditionalPanel(condition = "input.renderType == 'aspatial'")
            ),
            tabPanel("Table",
                conditionalPanel(condition = "input.renderType == 'spatial'",
                    dataTableOutput('gisTable')),
                downloadButton("downloadData", "Download")
                
                     

            )
        )
    )
)