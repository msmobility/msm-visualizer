# set the properties and add appropriate headers to the table related to the selected silo attribute
output$siloTable <- DT::renderDataTable({
  if(input$siloDataType == "Aspatial"){
    d <- input$siloScenario
    if (input$siloAspatialLevel == "Overview"){
      headerSiloAspatial(siloPlotSubset, input$siloAspatialLevel, d)
    } else if (input$siloAspatialLevel == "Households"){
      headerSiloAspatial(siloPlotSubset, input$siloHhLevel, d)
    } else if (input$siloAspatialLevel == "Persons"){
      headerSiloAspatial(siloPlotSubset, input$siloPpLevel, d)
    } else if (input$siloAspatialLevel == "Dwellings"){
      headerSiloAspatial(siloPlotSubset, input$siloDdLevel, d)
    } else if (input$siloAspatialLevel == "Regional"){
      headerSiloAspatial(siloPlotSubset, input$siloRrLevel, d)
    } else if (input$siloAspatialLevel == "Events"){
      headerSiloAspatial(siloPlotSubset, input$siloEeLevel, d)
    }
  } else {
    if(input$siloScenario == FALSE & input$siloGrowth == FALSE){
      d <- FALSE
    } else {
      d <- TRUE
    }
    if (input$siloSpatialLevel == "dwellings"){
      headerSiloSpatial(siloMapSubset, input$siloDdType, d)
    } else if (input$siloSpatialLevel == "accessibilities"){
      headerSiloSpatial(siloMapSubset, input$siloAccType, d)
    } else if (input$siloSpatialLevel == "incomes"){
      headerSiloSpatial(siloMapSubset, input$siloIncomes, d)
    } else {
      headerSiloSpatial(siloMapSubset, input$siloSpatialLevel, d)
    }
  }
})



### functions for creating table headers ###

# function to add headers to silo aspatial tables
headerSiloAspatial <- function(data, attribute, change){
  df <- filter(myLabels, myDataType == "siloAspatial" & myItem == attribute & mySwitch == 1)
  myTable <- data()
  if(change == FALSE){
    for (i in 1: ncol(myTable)){
      myColName <- paste0("myColumn", i)
      names(myTable)[i] <- df[[myColName]]
    }
  } else {
    if (ncol(myTable) == 5){
      names(myTable) <- c("Year", df$myColumn2, paste0(df$myColumn3, "_base"), paste0(df$myColumn3, "_comparison"),
                          paste0("% Change in ", df$myColumn3))
    } else if (ncol(myTable) == 6){
      names(myTable) <- c("Year", df$myColumn2, df$myColumn3, paste0(df$myColumn4, "_base"), 
                          paste0(df$myColumn4, "_comparison"), paste0("% Change in ", df$myColumn4))
    }
  }
  msmDataTable(myTable)
}


# function to add headers to silo spatial tables
headerSiloSpatial <- function(data, attribute, change){
  df <- filter(myLabels, myDataType == "siloSpatial" & myItem == attribute)
  myTable <- data() %>% 
    st_set_geometry(NULL)
  if (change == FALSE){
    names(myTable) <- c("Cell", df$myLegend) 
  } else {
    names(myTable) <- c("Cell", "Base", "Comparison", paste0("% Change in ", df$myLegend))
  }
  msmDataTable(myTable)
}