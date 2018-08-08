# CREATE INPUTS FOR UPLOADING FILES
mitoFilesUI <- wellPanel(
  fileInput(inputId = "resultFiles",
            label = "Upload result files", 
            multiple = TRUE,
            accept = ".csv"),
  checkboxInput(inputId = "scenario",
                label = "Compare scenarios", 
                value = FALSE),
  conditionalPanel("input.scenario == true",
                   fileInput(inputId = "scenarioFiles", 
                             label = "Upload comparison files",
                             multiple = TRUE,
                             accept = ".csv"))
)

# CREATE BUTTONS/INPUTS FOR SELECTING DATA TYPE AND TRIP PURPOSE
mitoAttributeSelectorUI <- wellPanel(
  radioButtons(inputId = "dataType", 
               label = "Select type of data", 
               choices = c("Aspatial", "Spatial"), 
               inline = TRUE),
  br(),
  selectInput(inputId = "purpose", 
              label = "Select trip purpose",
              choices = myPurposeCodes,
              selected = "HBW")
)

# CREATE BUTTONS/INPUTS FOR DETAILED SELECTION
mitoDetailedSelectorUI <- wellPanel(
  # DEFINE WHAT SHOULD BE DISPLAYED WHEN ASPATIAL DATA IS SELECTED
  conditionalPanel("input.dataType == 'Aspatial'",
                   selectInput(inputId = "aspatialData",
                               label = "Selece aspatial attribute",
                               choices = myAspatialCodes,
                               selected = "ModeShare"),
                   br(),
                   checkboxInput(inputId = "absRel",
                                 label = "View as percentage",
                                 value = FALSE)),
  # DEFINE WHAT SHOULD BE DISPLAYED WHEN SPATIAL DATA IS SELECTED
  conditionalPanel("input.dataType == 'Spatial'",
                   selectInput(inputId = "spatialData",
                               label = "Select spatial attribute",
                               choices = mySpatialCodes,
                               selected = "P"),
                   br(),
                   sliderInput(inputId = "categories",
                               label = "Select number of categories", 
                               value = 5, min = 3, max = 10, step = 1),
                   checkboxInput(inputId = "manualStyle",
                                 label = "Define breakpoints", 
                                 value = FALSE)),
  conditionalPanel("input.dataType == 'Spatial' && input.manualStyle == true",
                   textInput(inputId = "breaks", 
                             label = "Enter breakpoints (no space)",
                             value = "-Inf,-100,0,100,Inf",
                             placeholder = "-Inf,-100,0,100,Inf"))
)
