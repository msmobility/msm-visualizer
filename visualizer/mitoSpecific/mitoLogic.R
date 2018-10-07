####### Reactives for processing mito data #######

# server logic for creating mito spatial subset to be displayed in maps
mitoMapSubset <- reactive({
  msAttribute <- paste0(input$mitoPurpose, input$mitoSpatialLevel)
  msBaseFile <- callModule(fileReader, "mitoBaseResults", model = "mito")
  msBaseData <- mitoSpatialCleaner(msBaseFile, input$mitoZone)
  msBaseSelected <- mitoSpatialSelector(msBaseData, input$mitoZone, msAttribute)
  msSelected <- msBaseSelected
  if(input$mitoScenario == TRUE){
    msComparisonFile <- callModule(fileReader, "mitoComparisonResults", model = "mito")
    msComparisonData <- mitoSpatialCleaner(msComparisonFile, input$mitoZone)
    msComparisonSelected <- mitoSpatialSelector(msComparisonData, input$mitoZone, msAttribute)
    msSelected <- mitoJoinSpatial(msBaseSelected, msComparisonSelected)
  }
  msSelected()
})


# server logic for creating mito aspatial subset to be displayed in plots
mitoPlotSubset <- reactive({
  maBaseFile <- callModule(fileReader, "mitoBaseResults", model = "mito")
  maBaseData <- mitoAspatialCleaner(maBaseFile)
  maBaseSelected <- mitoAspatialSelector(maBaseData, input$mitoPurpose, input$mitoAspatialLevel)
  maSelected <- maBaseSelected
  if(input$mitoScenario == TRUE){
    maComparisonFile <- callModule(fileReader, "mitoComparisonResults", model = "mito")
    maComparisonData <- mitoAspatialCleaner(maComparisonFile)
    maComparisonSelected <- mitoAspatialSelector(maComparisonData, input$mitoPurpose, input$mitoAspatialLevel)
    maSelected <- mitoJoinAspatial(maBaseSelected, maComparisonSelected)
  }
  maSelected()
})



####### The functions used in the reactives #######

# function for subsetting data related to the selected mito aspatial attribute
mitoAspatialSelector <- function(myData, myPurpose, myAttribute){
  reactive({
    df <- filter(myData(), Feature == myAttribute) %>%
      select(Alternatives, myPurpose, paste0(myPurpose, "_s"))
    if(myAttribute != "ModeShare"){
      df <- mutate(df, Alternatives = as.numeric(Alternatives))
    } else {
      df <- mutate(df, Alternatives = mapvalues(Alternatives, from = mitoModes,
                                                to = mitoModeNames, warn_missing = FALSE))
    }
    names(df)[1:3] <- c("Key", "Value", "Share") 
    df
  })
}


# function for subsetting data related to the selected mito spatial attribute
mitoSpatialSelector <- function(myData, zoneLevel, myAttribute){
  reactive({
    #attribute <- paste0(purpose, spatialAttribute)
    if(zoneLevel == FALSE){
      df <- myData() %>% select(Cell = AGS, myAttribute, geometry)
    } else {
      df <- myData() %>% select(Cell = Zone, myAttribute, geometry)
    }
    df[[2]] = round(df[[2]], digits = 2)
    df
  })
}


# function for joining two mito spatial data frames for comparison
mitoJoinSpatial <- function(myData1, myData2){
  reactive({
    myData2 <- myData2() %>% st_set_geometry(NULL)
    df <- left_join(myData1(), myData2, by = "Cell", suffix = c("_base", "_comparison")) %>% 
      mutate(Value = 100 * (.[[3]] - .[[2]]) / .[[2]])
    df
  })
}


# function for joining two mito aspatial data frames for comparison
mitoJoinAspatial <- function(myData1, myData2){
  reactive({
    df <- left_join(myData1(), myData2(), by = "Key", suffix = c("_base", "_comparison")) %>% 
      mutate(Value = 100 * (.[[4]] - .[[2]]) / .[[2]])
    df
  })
}
