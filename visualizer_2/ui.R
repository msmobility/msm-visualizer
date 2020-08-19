## SILO Dashboard PLUS

##########################
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
##########################
### Data preparation

## Set working directory

setwd(here())

## Read external files for extra functions (to be checked)(fix in the latest version)

source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/fileSettings.r", sep ="/"))  ## Load auxiliary files like labels, settings and shapefiles
source(paste(here(),"visualizer/figureTableFunctions.R", sep ='/')) #contains functions for creating plots, maps and tables
source(paste(getwd(),"/visualizer_2/SILOVisualizer/functions/SiloLogic.r", sep ="/"))

# create colors
msmQualitative <- brewer.pal(12, "Set3")[c(1, 3:12)] #colors used for qualitative attributes
msmSequential <- viridisLite::viridis(10, direction = -1) #colors used for sequential attributes
msmPastel <- brewer.pal(9, "Pastel1") #colors used for pie chart

##########################
### Dashboard Header

ui = dashboardPagePlus(
    dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "SILO Visualizer 2.0"),
            img(icon("city"))),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "table"
        ),
##########################
    ### Sidebar
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
                ## Time horizon (update with model parameters)
                sliderInput(inputId = "year",label = h4("Select Year"),
                    value = 2020, min = initialYear, max = finalYear),
                ## Year selection
                checkboxInput("zone_level", "View at zone level", value=FALSE),
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
                    ## View growth
                    conditionalPanel("input.comparison == true",
                        radioButtons("comparisonSelector", h4("Select type of comparison"),
                            choices = list("Compare scenarios" = 1,
                            "Base year comparison"= 2,
                            "No comparison" = 3), selected = 3),
                        conditionalPanel("input.comparisonSelector == 3",
                        radioButtons("scenarioSelector", h4("Select map scenario"),
                            choices = list("Scenario 1" = 1, "Scenario 2" = 2), selected = 1))),
                    numericInput("siloMapCategories", h4("Enter number of categories"), 7, 3, 15, 1),
                    radioButtons("siloMapStyle", h4("Select classification style"), c("pretty", "equal", "quantile"), inline = TRUE)
                )),
                conditionalPanel("input.renderType == 'aspatial'",
                    selectInput("aspatialLevel","Select aspatial attribute", c("Overview" = 'overview',
                        "Households" = 'households',
                        "Persons" = 'persons',
                        "Dwellings" = 'dwellings',
                        "Regional" = 'regional',
                        "Events" = 'events')),
                    conditionalPanel("input.aspatialLevel == 'households'",
                        selectInput("HHLevel", "Select household level", (aHH)),
                        conditionalPanel("input.HHLevel == 'hhSizInc' |input.HHLevel == 'hhRentIncome'",
                            checkboxInput("switchView","Switch View", value=FALSE))),
                    conditionalPanel("input.aspatialLevel == 'persons'",
                        selectInput("personsLevel", "Select person level",(aPerson))),
                    conditionalPanel("input.aspatialLevel == 'dwellings'",
                        selectInput("dwellingsLevel", "Select dwelling level", (aDwelling))),
                    conditionalPanel("input.aspatialLevel == 'regional'",
                        selectInput("dwellingsLevel", "Select dwelling level", (aRegional))),
                    conditionalPanel("input.aspatialLevel == 'events'",
                        selectInput("dwellingsLevel", "Select dwelling level", (aEvent)))
                )
            )
        )
    ),
##########################
    ## Dashboard body
    dashboardBody(
        box(
            title = 'Graph visualization',
            closable = FALSE,
            status = "primary",
            collapsible = TRUE,
            enable_dropdown = TRUE,
            width = NULL,
            conditionalPanel(condition = "input.renderType == 'spatial'",
                leafletOutput("siloMap", height = 900),
                #wellPanel(textOutput("cnty"))
                ),
            conditionalPanel(condition = "input.renderType == 'aspatial'",
                plotlyOutput("siloPlot", height = 900))
        )
    ),
##########################
    ## Right sidebar
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