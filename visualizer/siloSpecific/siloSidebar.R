### sidebar for organizing silo user inputs

# panel for reading silo files
siloFileUploadPanel <- wellPanel(
  fileReaderUI("siloBaseResults"),
  checkboxInput(inputId = "siloScenario", label = "Compare scenarios", value = FALSE),
  conditionalPanel("input.siloScenario == true",
                   fileReaderUI("siloComparisonResults", "Upload comparison files"))
)


# panel for selecting type of silo data to explore
siloDataTypeSelectorPanel <- wellPanel(
  radioButtons(inputId = "siloDataType", label = "Select type of data", choices = c("Aspatial", "Spatial"), inline = TRUE),
  conditionalPanel("input.siloDataType == 'Spatial'",
                   checkboxInput(inputId = "siloZone", label = "View at zone level", value = FALSE),
                   sliderInput(inputId = "siloYear",
                               label = "Select year",
                               min = 2017, max = 2040, value = 2017, step = 1, sep = "",
                               animate = animationOptions(interval = 5000, loop = FALSE)))
) 


# panel for selecting silo attribute to display
siloAttributeSelectorPanel <- wellPanel(
  
  # conditional panel when aspatial data is selected
  conditionalPanel("input.siloDataType == 'Aspatial'",
                   selectInput("siloAspatialLevel", "Select aspatial attribute", siloAspatialCategory),
                   conditionalPanel("input.siloAspatialLevel == 'Households'",
                                    selectInput("siloHhLevel", "Select household attribute", c(siloHHAttributes, "Average rent")),
                                    conditionalPanel("input.siloHhLevel == 'Income'",
                                                     checkboxInput("siloRentIncome", "Switch view", FALSE)),
                                    conditionalPanel("input.siloHhLevel == 'hhByType'",
                                                     checkboxInput("siloSizeIncome", "Switch view", FALSE))),
                   conditionalPanel("input.siloAspatialLevel == 'Persons'",
                                    selectInput("siloPpLevel", "Select person attribute", c(siloPPAttributes, "Migration")),
                                    conditionalPanel("input.siloPpLevel == 'Age'",
                                                     checkboxInput("pyramid", "View as population pyramid", FALSE))),
                   conditionalPanel("input.siloAspatialLevel == 'Dwellings'",
                                    selectInput("siloDdLevel", "Select dwelling attribute", siloDDAttributes)),
                   conditionalPanel("input.siloAspatialLevel == 'Regional'",
                                    selectInput("siloRrLevel", "Select regional attribute", c(siloRegAttributes, "Total jobs")),
                                    conditionalPanel("input.siloRrLevel == 'jobByRegion'",
                                                     checkboxInput("siloSectReg", "Switch view", FALSE))),
                   conditionalPanel("input.siloAspatialLevel == 'Events'",
                                    selectInput("siloEeLevel", "Select event category", c("Household events", "Person events", "Dwelling events")))
                   #checkboxInput("siloAbsRel", "View as percentage", FALSE)
                   ),
  
  # conditional panel when spatial data is selected
  conditionalPanel("input.siloDataType == 'Spatial'",
                   selectInput("siloSpatialLevel", "Select spatial attribute", siloSpatialCategory),
                   conditionalPanel("input.siloSpatialLevel == 'dwellings'",
                                    selectInput("siloDdType", "Select type of dwelling", siloDwellings)),
                   conditionalPanel("input.siloSpatialLevel == 'accessibilities'",
                                    radioButtons("siloAccType", "Select mode of accessibility", siloAccessibilities, inline = TRUE)),
                   conditionalPanel("input.siloSpatialLevel == 'incomes'",
                                    selectInput("siloIncomes", "Select income class", siloIncomeClasses)),
                   conditionalPanel("input.siloScenario == false",
                                    checkboxInput("siloGrowth", "View growth from base year", FALSE)),
                   numericInput("siloMapCategories", "Enter number of categories", 7, 3, 15, 1),
                   radioButtons("siloMapStyle", "Select classification style", c("pretty", "equal", "quantile"), inline = TRUE))
)


