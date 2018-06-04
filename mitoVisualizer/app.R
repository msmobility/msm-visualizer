#
# This is a Shiny web application for visualizing MITO results

library(shiny)

library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

library(rgdal)
library(tmap)
library(leaflet)


my <- 1000000
#zones <- readOGR("C:/Users/matthewokrah/Desktop/okrahR/silo/map", "zonesNew")
zones <- readOGR("map", "zonesNew")

purposes <- c("Home-based Work" = "HBW", "Home-based Eduction" = "HBE", "Home-based Shopping" = "HBS",
              "Home-based Other" = "HBO", "Non-home-based Work" = "NHBW", "Non-home-based Other" = "NHBO", "All" = "All")

spatial_attributes <- c("Produced Trips per Area" = "P", "Attracted Trips per Area" = "A", "Average Travel Time Budget" = "TTB", 
                        "Average Distance Travelled" = "AvDist", "Average Time Travelled" = "AvTime")

spatial_units <- c(" (Trips/sq.km)" = "P", " (Trips/sq.km)" = "A", " (Minutes)" = "TTB", 
                   " (Km)" = "AvDist", " (Minutes)" = "AvTime")

spatial_styles <- c("quantile" = "P", "quantile" = "A", "pretty" = "TTB", 
                    "pretty" = "AvDist", "pretty" = "AvTime")

aspatial_attributes <- c("Number of Persons by Number of Trips" = "PPbyTrips", "Number of Households by Number of Trips" = "HHbyTrips", 
                         "Travel Distance Distribution" = "TDD", "Travel Time Distribution" = "TTD", "Mode Share" = "ModeShare")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("MITO Model Results"),
  
  # Sidebar for user interaction
  sidebarLayout(
    
    sidebarPanel(
      
      wellPanel(
        h3("Uploading result files"),
        fileInput(inputId = "resultFile",
                  label = "Upload result file", 
                  accept = ".csv"),
        fileInput(inputId = "spatialResultFile", 
                  label = "Upload spatial result file", 
                  accept = ".csv")
      ),
      
      wellPanel(
        h3("Selecting trip purpose and type of data"),
        selectInput(inputId = "purpose", 
                    label = "Select desired trip purpose",
                    choices = purposes),
        br(),
        radioButtons(inputId = "dataType", 
                     label = "Select the type of data to display", 
                     choices = c("Aspatial Data", "Spatial Data"), 
                     selected = "Spatial Data")
      ),
      
      wellPanel(
        conditionalPanel("input.dataType == 'Aspatial Data'",
                         h3("Displaying aspatial data"),
                         radioButtons(inputId = "aspatialData",
                                      label = "Select aspatial attribute to display",
                                      choices = aspatial_attributes)
        ),
        
        conditionalPanel("input.dataType == 'Spatial Data'",
                         h3("Displaying spatial data"),
                         radioButtons(inputId = "spatialData",
                                      label = "Select spatial attribute to display",
                                      choices = spatial_attributes)
        )
      )
      
    ),
    
    # Show figure
    mainPanel(
      
      conditionalPanel("input.dataType == 'Aspatial Data'", h3(textOutput("aspatialCaption"))),
      conditionalPanel("input.dataType == 'Spatial Data'", h3(textOutput("spatialCaption"))),
      br(),
      conditionalPanel("input.dataType == 'Spatial Data'", leafletOutput("map", height = "800px")),
      conditionalPanel("input.dataType == 'Aspatial Data' && input.aspatialData == 'ModeShare'", plotlyOutput("modalsplit", height = "800px"))
      #conditionalPanel("input.dataType == 'Aspatial Data' && input.aspatialData == 'ModeShare'", plotlyOutput("modalsplit", height = "800px"))
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  clean_aspatial <- reactive({
    req(input$resultFile)
    link <- input$resultFile
    raw <- read_csv(link$datapath)
    raw %>% separate(Attribute, into = c("Feature", "Options"), sep = "_") %>% 
      filter(HBW != "null") %>% 
      mutate(HBW = as.numeric(HBW), HBE = as.numeric(HBE), HBS = as.numeric(HBS), HBO = as.numeric(HBO), 
             NHBW = as.numeric(NHBW), NHBO = as.numeric(NHBO))
  })
  
  plot_subset <- reactive({
    clean_aspatial() %>% 
      filter(Feature == input$aspatialData)
  })
  
  
  output$modalsplit <- renderPlotly({
    df <- plot_subset()
    df$yy <- df[[input$purpose]] 
    plot_ly(df, labels = ~Options, values = ~yy, type = 'pie') %>% 
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  
  clean_spatial <- reactive({
    req(input$spatialResultFile)
    link <- input$spatialResultFile
    raw <- read_csv(link$datapath) 
    final <- merge(zones, raw, by.x = "id", by.y = "Zone", all.x = FALSE)
    final@data <- final@data %>% 
      mutate(AllP = my * (HBWP + HBEP + HBSP + HBOP + NHBWP + NHBOP)/Area,
             AllA = my * (HBWA + HBEA + HBSA + HBOA + NHBWA + NHBOA)/Area,
             HBWP = my * HBWP/Area, HBEP = my * HBEP/Area, HBSP = my * HBSP/Area, HBOP = my * HBOP/Area, NHBWP = my * NHBWP/Area, NHBOP = my * NHBOP/Area,
             HBWA = my * HBWA/Area, HBEA = my * HBEA/Area, HBSA = my * HBSA/Area, HBOA = my * HBOA/Area, NHBWA = my * NHBWA/Area, NHBOA = my * NHBOA/Area)
    final
  })
  
  tofill <- reactive({
    toString(paste(input$purpose, input$spatialData, sep = ""))
  })
  
  output$map <- renderLeaflet({
    heading <-
      tmap_leaflet(tm_shape(clean_spatial()) +
                     tm_fill(tofill(), title = paste(names(spatial_attributes[spatial_attributes == input$spatialData]), 
                                                     names(spatial_units[spatial_units == input$spatialData])), 
                             style = names(spatial_styles[spatial_styles == input$spatialData])) +
                     tm_layout(title = paste(names(spatial_attributes[spatial_attributes == input$spatialData]), " for ", 
                                             names(purposes[purposes == input$purpose]), " Trips"))
      )
  })
  
  #style = "fixed", breaks = c(0.1, 0.2, 0.3)
  output$spatialCaption <- renderText({
    paste(names(spatial_attributes[spatial_attributes == input$spatialData]), " for ", names(purposes[purposes == input$purpose]), " Trips")
  })
  
  output$aspatialCaption <- renderText({
    paste(names(aspatial_attributes[aspatial_attributes == input$aspatialData]), " for ", names(purposes[purposes == input$purpose]), " Trips")
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

