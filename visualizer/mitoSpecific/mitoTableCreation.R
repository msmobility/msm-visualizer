# set the properties and add appropriate headers to the table related to the selected mito attribute
output$mitoTable <- DT::renderDataTable({
  if(input$mitoDataType == "Aspatial"){
    headerMitoAspatial(mitoPlotSubset, input$mitoAspatialLevel, input$mitoScenario)
  } else {
    headerMitoSpatial(mitoMapSubset, input$mitoSpatialLevel, input$mitoScenario)
  }
})



### functions for creating table headers ###

# function to add headers to mito aspatial tables
headerMitoAspatial <- function(data, attribute, change){
  df <- filter(myLabels, myDataType == "mitoAspatial" & myItem == attribute)
  myTable <- data()
  if(change == FALSE){
    names(myTable) <- c(df$myColumn1, df$myColumn2, df$myColumn3)
  } else {
    names(myTable) <- c(df$myColumn1, paste0(df$myColumn2, "_base"), paste0(df$myColumn3, "_base"), paste0(df$myColumn2, "_comparison"), 
                        paste0(df$myColumn3, "_comparison"), paste0("% Change in ", df$myColumn2))
  }
  msmDataTable(myTable)
}


# function to add headers to mito spatial tables
headerMitoSpatial <- function(data, attribute, change){
  df <- filter(myLabels, myDataType == "mitoSpatial" & myItem == attribute)
  myTable <- data() %>% 
    st_set_geometry(NULL)
  if (change == FALSE){
    names(myTable) <- c("Cell", df$myLegend) 
  } else {
    names(myTable) <- c("Cell", "Base", "Comparison", paste0("% Change in ", df$myLegend))
  }
  msmDataTable(myTable)
}