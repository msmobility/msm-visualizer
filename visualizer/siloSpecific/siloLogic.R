####### Reactives for processing silo data #######

# server logic for creating silo spatial subset to be displayed in maps
siloMapSubset <- reactive({
  if(input$siloSpatialLevel %in% c(siloMainAttributes, "avePrice")){
    ssAttribute <- input$siloSpatialLevel
  } else if (input$siloSpatialLevel == "dwellings"){
    ssAttribute <- input$siloDdType
  } else if (input$siloSpatialLevel == "accessibilities"){
    ssAttribute <- input$siloAccType
  } else if (input$siloSpatialLevel == "incomes"){
    ssAttribute <- input$siloIncomes
  }
  ssBaseFile <- callModule(fileReader, "siloBaseResults", model = "silo")
  ssBaseData <- siloSpatialCleaner(ssBaseFile)
  ssBaseSelected <- siloSpatialSelector(ssBaseData, input$siloZone, ssAttribute, input$siloYear)
  ssBaseGrowthBase <- siloSpatialSelector(ssBaseData, input$siloZone, ssAttribute, 2011)
  ssComparisonFile <- callModule(fileReader, "siloComparisonResults", model = "silo")
  ssComparisonData <- siloSpatialCleaner(ssComparisonFile)
  ssComparisonSelected <- siloSpatialSelector(ssComparisonData, input$siloZone, ssAttribute, input$siloYear)
  ssComparisonGrowthBase <- siloSpatialSelector(ssComparisonData, input$siloZone, ssAttribute, 2011)
  
  ssSelected <- ssBaseSelected
  # for comparison between two datasets
  if(input$siloGrowth == TRUE & input$siloScenario == FALSE){
    ssSelected <- siloJoinSpatial(ssBaseGrowthBase, ssBaseSelected)
  } else if (input$siloScenario == TRUE){
    ssSelected <- siloJoinSpatial(ssBaseSelected, ssComparisonSelected)
  }
  ssSelected()
})


# server logic for creating silo aspatial subset to be displayed in plots
siloPlotSubset <- reactive({
  saBaseFile <- callModule(fileReader, "siloBaseResults", model = "silo")
  saBaseData <- siloAspatialCleaner(saBaseFile)
  saComparisonFile <- callModule(fileReader, "siloComparisonResults", model = "silo")
  saComparisonData <- siloAspatialCleaner(saComparisonFile)
  if(input$siloAspatialLevel == "Households"){
    if(input$siloHhLevel == "Average rent"){
      saBaseSelected <- siloAspatialSelector8(saBaseData, c("Income"), c("Key", siloHouseRents, "Value")) 
      saComparisonSelected <- siloAspatialSelector8(saComparisonData, c("Income"), c("Key", siloHouseRents, "Value")) 
    } else if(input$siloHhLevel == "hhBySize"){
      saBaseSelected <- siloAspatialSelector2(saBaseData, c(input$siloHhLevel), c("Key0", siloHHSizes))
      saComparisonSelected <- siloAspatialSelector2(saComparisonData, c(input$siloHhLevel), c("Key0", siloHHSizes))
    } else if(input$siloHhLevel == "Income"){
      saBaseSelected <- siloAspatialSelector9(saBaseData, c(input$siloHhLevel), c("Key2", siloHouseRents, "Key0"), TRUE)
      saComparisonSelected <- siloAspatialSelector9(saComparisonData, c(input$siloHhLevel), c("Key2", siloHouseRents, "Key0"), TRUE)
    } else if(input$siloHhLevel == "AveHHInc"){
      saBaseSelected <- siloAspatialSelector4(saBaseData, c(input$siloHhLevel), c("Key0", "Average", "Key00", "Median"))
      saComparisonSelected <- siloAspatialSelector4(saComparisonData, c(input$siloHhLevel), c("Key0", "Average", "Key00", "Median"))
    } else if(input$siloHhLevel == "hhByType"){
      saBaseSelected <- siloAspatialSelector5(saBaseData, c(input$siloHhLevel), "inc", "size")
      saComparisonSelected <- siloAspatialSelector5(saComparisonData, c(input$siloHhLevel), "inc", "size")
    } else {
      saBaseSelected <- siloAspatialSelector0(saBaseData, c(input$siloHhLevel))
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, c(input$siloHhLevel))
    }
  } else if (input$siloAspatialLevel == "Persons"){
    if(input$siloPpLevel == "Migration"){
      saBaseSelected <- siloAspatialSelector0(saBaseData, siloMigrants)
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, siloMigrants)
    } else if (input$siloPpLevel %in% c("Age", "laborParticipationRateByAge")){
      saBaseSelected <- siloAspatialSelector6(saBaseData, c(input$siloPpLevel))
      saComparisonSelected <- siloAspatialSelector6(saComparisonData, c(input$siloPpLevel))
    } else {
      saBaseSelected <- siloAspatialSelector0(saBaseData, c(input$siloPpLevel))
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, c(input$siloPpLevel))
    }
  } else if (input$siloAspatialLevel == "Dwellings"){
    if(input$siloDdLevel %in% c("CountOfDD", "AveMonthlyPrice", "AveVacancy")){
      saBaseSelected <- siloAspatialSelector1(saBaseData, c(input$siloDdLevel), c("Key0", "Key", "Value"))
      saComparisonSelected <- siloAspatialSelector1(saComparisonData, c(input$siloDdLevel), c("Key0", "Key", "Value"))
    } else {
      saBaseSelected <- siloAspatialSelector0(saBaseData, c(input$siloDdLevel))
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, c(input$siloDdLevel))
    }
  } else if (input$siloAspatialLevel == "Regional"){
    if (input$siloRrLevel == "Total jobs"){
      saBaseSelected <- siloAspatialSelector1(saBaseData, c("jobByRegion"), c("Key", siloJobTypes, "Value"))
      saComparisonSelected <- siloAspatialSelector1(saComparisonData, c("jobByRegion"), c("Key", siloJobTypes, "Value"))
    } else if (input$siloRrLevel == "jobByRegion"){
      saBaseSelected <- siloAspatialSelector3(saBaseData, c(input$siloRrLevel), c("Key2", siloJobTypes, "Key0"))
      saComparisonSelected <- siloAspatialSelector3(saComparisonData, c(input$siloRrLevel), c("Key2", siloJobTypes, "Key0"))
    } else {
      saBaseSelected <- siloAspatialSelector0(saBaseData, c(input$siloRrLevel))
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, c(input$siloRrLevel))
    }
  } else if (input$siloAspatialLevel == "Events"){
    if(input$siloEeLevel == "Household events"){
      saBaseSelected <- siloAspatialSelector0(saBaseData, siloHHEvents)
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, siloHHEvents)
    } else if (input$siloEeLevel == "Person events"){
      saBaseSelected <- siloAspatialSelector0(saBaseData, siloPPEvents)
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, siloPPEvents)
    } else if (input$siloEeLevel == "Dwelling events"){
      saBaseSelected <- siloAspatialSelector0(saBaseData, siloDDEvents)
      saComparisonSelected <- siloAspatialSelector0(saComparisonData, siloDDEvents)
    } 
  } else if (input$siloAspatialLevel == "Overview"){
    saBaseSelected <- siloAspatialSelector7(saBaseData)
    saComparisonSelected <- siloAspatialSelector7(saComparisonData)
  }
  saSelected <- saBaseSelected

  if(input$siloScenario == TRUE){
    saSelected <- siloJoinAspatial(saBaseSelected, saComparisonSelected)
  } 
  saSelected()
})