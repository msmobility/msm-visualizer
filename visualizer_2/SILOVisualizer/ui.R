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

## Set working directory

setwd(here())

## Read external files for extra functions (to be checked)(fix in the latest version)

source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/fileSettings.r", sep ="/"))
source(paste(here(),"visualizer/figureTableFunctions.R", sep ='/')) #contains functions for creating plots, maps and tables
source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/SiloLogic.r", sep ="/"))

## Dashboard

ui = dashboardPage(
    dashboardHeader(title = "SILO Visualizer 2.0"),
    dashboardSidebar( width = 300,
    ## Title
    h5("  Select folder with the result files"),
    ## Folder selector
    shinyDirButton("dir", "Select input folder", "Select directory"),
        checkboxInput("comparison","Compare scenarios", value=FALSE),
        conditionalPanel( "input.comparison == true",
            shinyDirButton("dir2","Select scenario folder", "Select directory")
        ),
    actionButton("update","Update data"),
    ## Radio buttons for choose the representation
    radioButtons("renderType", "Select the visualization type",
        choices = list("Spatial" = "spatial",
        "Aspatial" = "aspatial")),
    ## Slider for year selection and dissagregated map in Spatial visualizations
    conditionalPanel("input.renderType == 'spatial'",
    ## Time horizon (update with model parameters)
        sliderInput(inputId = "year",label = "Year",
        value = 2020, min = initialYear, max = finalYear),
        checkboxInput("zone_level", "View at zone level", value=FALSE)
    ),
    ## Select box input (depends on the radio button)
    conditionalPanel("input.renderType == 'spatial'",
        selectInput("spatialLevel", "Select spatial attribute",c("Population" = 'population',
                                                                                      "Households" = 'households',
                                                                                      "Jobs" = 'jobs',
                                                                                      "Available land"='availLand',
                                                                                      "Average price" = 'avePrice',
                                                                                      "Dwellings" = 'dwellings',
                                                                                      "Accessibilites" ='accessibilities',
                                                                                      "Income" = 'income'
                                                                                      )),
    conditionalPanel("input.spatialLevel == 'dwellings'",
        selectInput("sDwelling","Select dwelling level",(sDwelling))),
                             conditionalPanel("input.spatialLevel == 'income'",
                                              selectInput("sIncome", "Select income class", (sIncome))),
                             conditionalPanel("input.spatialLevel == 'accessibilities'",
                                              selectInput("sAcc", "Select accessibility mode", (sAccessibility))),
                             ## View growth
                             checkboxInput("baseYearcomparison","View growth from base year", value=FALSE),
                             
                             numericInput("siloMapCategories", "Enter number of categories", 7, 3, 15, 1),
                             radioButtons("siloMapStyle", "Select classification style", c("pretty", "equal", "quantile"), inline = TRUE)
                             
                             
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
                conditionalPanel( condition = "input.num >=50",
                #tableOutput("table")
                
                verbatimTextOutput("value")
                )
            )
        )
    )
)