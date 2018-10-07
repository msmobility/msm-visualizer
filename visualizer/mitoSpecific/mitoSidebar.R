### sidebar for organizing mito user inputs

# panel for reading mito files
mitoFileUploadPanel <- wellPanel(
  fileReaderUI("mitoBaseResults"),
  checkboxInput(inputId = "mitoScenario", label = "Compare scenarios", value = FALSE),
  conditionalPanel("input.mitoScenario == true",
                   fileReaderUI("mitoComparisonResults", "Upload comparison files"))
)


# panel for selecting type of mito data to explore
mitoDataTypeSelectorPanel <- wellPanel(
  radioButtons(inputId = "mitoDataType", label = "Select type of data", choices = c("Aspatial", "Spatial"), inline = TRUE),
  conditionalPanel("input.mitoDataType == 'Spatial'",
                   checkboxInput(inputId = "mitoZone", label = "View at zone level", value = FALSE)),
  selectInput("mitoPurpose", "Select trip purpose", mitoPurposes)
) 


# panel for selecting silo attribute to display
mitoAttributeSelectorPanel <- wellPanel(
  
  # conditional panel when aspatial data is selected
  conditionalPanel("input.mitoDataType == 'Aspatial'",
                   selectInput("mitoAspatialLevel", "Select aspatial attribute", mitoAspatialCategory),
                   conditionalPanel("input.mitoScenario == false",
                                    checkboxInput("mitoAbsRel", "View as percentage", FALSE))),
                  
  # conditional panel when spatial data is selected
  conditionalPanel("input.mitoDataType == 'Spatial'",
                   selectInput("mitoSpatialLevel", "Select spatial attribute", mitoSpatialCategory),
                   numericInput("mitoMapCategories", "Enter number of categories", 7, 3, 15, 1),
                   radioButtons("mitoMapStyle", "Select classification style", c("pretty", "equal", "quantile"), inline = TRUE))
)