#
# This is a Shiny web application for visualizing MITO results

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)

library(rgdal)
library(tmap)
library(leaflet)

# Define a value to be used to convert squared meters to squared kilometers
my <- 1000000

# Read in the zones shapefile
zones <- readOGR("map", "zonesNew")

# Define a variable to hold trip purposes
purposes <- c("Home-based Work" = "HBW", "Home-based Eduction" = "HBE", "Home-based Shopping" = "HBS",
              "Home-based Other" = "HBO", "Non-home-based Work" = "NHBW", "Non-home-based Other" = "NHBO", "All Purposes" = "All")

# Define a variable to hold the spatial attributes
spatial_attributes <- c("Produced Trips per Area" = "P", "Attracted Trips per Area" = "A", "Average Travel Time Budget" = "TTB", 
                        "Average Distance Travelled" = "AvDist", "Average Time Travelled" = "AvTime")

# Define a variable to hold the units to be used in the legends of map
spatial_units <- c(" (Trips/sq.km)" = "P", " (Trips/sq.km)" = "A", " (Minutes)" = "TTB", 
                   " (Km)" = "AvDist", " (Minutes)" = "AvTime")

# Define a variable to hold the styles to be applied in defining categories for spatial data
spatial_styles <- c("quantile" = "P", "quantile" = "A", "pretty" = "TTB", 
                    "pretty" = "AvDist", "pretty" = "AvTime")

# Define a variable to hold the aspatial attributes
aspatial_attributes <- c("Persons by Number of Trips" = "PPbyTrips", "Households by Number of Trips" = "HHbyTrips", 
                         "Travel Distance Distribution" = "Distance", "Travel Time Distribution" = "Time", "Mode Share" = "ModeShare")

# Define a variable to hold x axis labels for aspatial attributes
aspatial_x_labels <- c("Number of Trips" = "PPbyTrips", "Number of Trips" = "HHbyTrips", "Travel Distance (km)" = "Distance", 
                       "Travel Time (minutes)" = "Time")

# Define a variable to hold y axis labels for aspatial attributes
aspatial_y_labels <- c("Number of Persons" = "PPbyTrips", "Number of Households" = "HHbyTrips", 
                       "Frequency" = "Distance", "Frequency" = "Time")



# Define the User Interface for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("MITO Model Results"),
  
  # Sidebar for user interaction
  sidebarLayout(
    
    sidebarPanel(
      
      # Create file inputs in a well panel where result files will be uploaded
      wellPanel(
        fileInput(inputId = "resultFile",
                  label = "Upload result file", 
                  accept = ".csv"),
        fileInput(inputId = "spatialResultFile", 
                  label = "Upload spatial result file", 
                  accept = ".csv")
      ),
      
      # Create radio buttons in a well panel for selecting data type and trip purpose
      wellPanel(
        radioButtons(inputId = "dataType", 
                     label = "What type of results do you want to display?", 
                     choices = c("Aspatial Data", "Spatial Data"), 
                     inline = TRUE),
        br(),
        radioButtons(inputId = "purpose", 
                    label = "Which trip purpose are you interested in?",
                    choices = purposes,
                    inline = TRUE)
      ),
      
      # Create conditional panels with buttons and inputs for selecting the attributes to display based on the data type selected
      wellPanel(
        
        # Define what should be displayed when Aspatial Data is selected as data type
        conditionalPanel("input.dataType == 'Aspatial Data'",
                         radioButtons(inputId = "aspatialData",
                                      label = "Which aspatial attribute do you want to display",
                                      choices = aspatial_attributes,
                                      selected = "ModeShare"),
                         br(),
                         radioButtons(inputId = "absRel",
                                      label = "Are you interested in counts or percentages?",
                                      choices = c("Count" = "", "Percentage" = "s"),
                                      inline = TRUE)
        ),
        
        # Define what should be displayed when Spatial Data is selected as data type
        conditionalPanel("input.dataType == 'Spatial Data'",
                         radioButtons(inputId = "spatialData",
                                     label = "Which spatial attribute do you want to display?",
                                     choices = spatial_attributes,
                                     selected = "P"),
                         br(),
                         sliderInput(inputId = "categories",
                                    label = "How many categories do you want to see?",
                                    value = 5, min = 3, max = 10)
        )
      )
    ),
    
    # Main panel to display maps or charts based on the selections in the sidebar panel
    mainPanel(
      
      conditionalPanel("input.dataType == 'Aspatial Data'", h3(textOutput("aspatialCaption"))),
      conditionalPanel("input.dataType == 'Spatial Data'", h3(textOutput("spatialCaption"))),
      br(),
      conditionalPanel("input.dataType == 'Spatial Data'", leafletOutput("map", height = "800px")),
      conditionalPanel("input.dataType == 'Aspatial Data' && input.aspatialData == 'ModeShare'", plotlyOutput("modalsplit", height = "800px")),
      conditionalPanel("input.dataType == 'Aspatial Data' && input.aspatialData != 'ModeShare'", plotlyOutput("charts", height = "800px"))
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Create a reactive that holds the cleaned aspatial data
  clean_aspatial <- reactive({
    req(input$resultFile)
    link <- input$resultFile
    raw <- read_csv(link$datapath)
    refined <- raw %>% separate(Attribute, into = c("Feature", "Alternatives"), sep = "_") %>% 
      mutate(HBW = as.numeric(HBW), HBE = as.numeric(HBE), HBS = as.numeric(HBS), HBO = as.numeric(HBO), 
             NHBW = as.numeric(NHBW), NHBO = as.numeric(NHBO))
    refined[is.na(refined)] <- 0
    refined
  })
  
  # Create a reactive that holds the selected aspatial attribute to be displayed
  plot_subset <- reactive({
    clean_aspatial() %>% 
      filter(Feature == input$aspatialData)
  })
  
  # Create a pie chart that displays modal split
  output$modalsplit <- renderPlotly({
    df <- plot_subset() %>% 
      mutate(TravelMode = recode(Alternatives, "autoDriver" = "Car Driver", "autoPassenger" = "Car Passenger", "bicycle" = "Bicylce", 
                                         "bus" = "Bus", "train" = "Train", "tramOrMetro" = "Tram/Metro", "walk" = "Walk",
                                         "privateAV" = "Private AV", "sharedAV" = "Shared AV"), All = HBW+HBE+HBS+HBO+NHBW+NHBO)
    mode_colors <- c("#FF6961", "#FFB347", "#FDFD96", "#AEC6CF", "#966FD6", "#B19CD9", "#77DD77", "#836953", "#CFCFC4") 
    df$yy <- df[[input$purpose]] 
    plot_ly(df, labels = ~TravelMode, values = ~yy, type = 'pie', sort = FALSE, marker = list(colors = mode_colors)) %>% 
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
            )
  })
  
  # Create a bar chart that displays the other aspatial attributes
  output$charts <- renderPlotly({
    df <- plot_subset() %>% 
      mutate(Alternatives = as.numeric(Alternatives), HBWs = 100*HBW/sum(HBW), HBEs = 100*HBE/sum(HBE), HBSs = 100*HBS/sum(HBS), HBOs = 100*HBO/sum(HBO), 
             NHBWs = 100*NHBW/sum(NHBW), NHBOs = 100*NHBO/sum(NHBO), All = HBW+HBE+HBS+HBO+NHBW+NHBO, Alls = 100*All/sum(All))
    trying <- toString(paste(input$purpose, input$absRel, sep = ""))
    df$yy <- df[[trying]]
    if(input$absRel == ""){
      my_ytitle <- names(aspatial_y_labels[aspatial_y_labels == input$aspatialData])
    } else if (input$absRel == "s"){
      my_ytitle <- paste(names(aspatial_y_labels[aspatial_y_labels == input$aspatialData]), " (%)")
    }
    plot_ly(df, y = ~yy, x = ~Alternatives, type = "bar") %>% 
      layout(yaxis = list(title = my_ytitle),
             xaxis = list(title = names(aspatial_x_labels[aspatial_x_labels == input$aspatialData]))
             )
  })
  
  # Create a reactive that holds the cleaned spatial data
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
  
  # Create a map that displays spatial attributes
  output$map <- renderLeaflet({
    myAttribute <- toString(paste(input$purpose, input$spatialData, sep = ""))
    tmap_leaflet(tm_shape(clean_spatial()) +
                   tm_fill(myAttribute, title = paste(names(spatial_attributes[spatial_attributes == input$spatialData]), 
                                                   names(spatial_units[spatial_units == input$spatialData])), 
                           style = names(spatial_styles[spatial_styles == input$spatialData]), n = input$categories)
      )
  })
  
  # Create a caption for spatial attributes
  output$spatialCaption <- renderText({
    paste(names(spatial_attributes[spatial_attributes == input$spatialData]), " for ", names(purposes[purposes == input$purpose]), " Trips")
  })
  
  # Create a caption for aspatial attributes
  output$aspatialCaption <- renderText({
    paste(names(aspatial_attributes[aspatial_attributes == input$aspatialData]), " for ", names(purposes[purposes == input$purpose]), " Trips")
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)