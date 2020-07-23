## SILO logic

## Spatial function for routing options

## Function prepareSiloMap : Call the geographical database and filter according to the parameters in UI
prepareSiloMap <- function (data, thisYear, zone_level, attribute, aggregationType){
  print("Entering to prepareSiloMap")
  ## Conditional to choose between aggregate or keep dissagregate
  if(zone_level == FALSE){

    joinTable <- filter(data ,year == thisYear)
    spatialTable <- left_join(zones, joinTable, by="shp_id")
    
    if(aggregationType == "average") {
      groupedTable <- spatialTable %>%
      group_by(shp_muni)%>% summarize_at(vars(attribute), list(mean))
      groupedTable <-groupedTable%>% mutate(!!attribute := groupedTable[[attribute]])
      
    } else {
      groupedTable <- spatialTable %>%
      group_by(shp_muni)%>% summarize_at(vars(attribute, shp_area), list(sum)) 
      groupedTable <-groupedTable%>% mutate(!!attribute := ((groupedTable[[attribute]] / shp_area) * 1000000 ))
    }

  } else {
    joinTable <- filter(data,year == thisYear)
    groupedTable <- left_join(zones, joinTable, by="shp_id")
    if(aggregationType == "density"){
      groupedTable <-groupedTable%>% mutate(!!attribute := ((groupedTable[[attribute]] / shp_area) * 1000000 ))
    }
    
  }
  print("Finishing prepareSiloMap")
  return(groupedTable)
  
}
compareScenarios<-function(data, data2,attribute){
  comparisonDataBase <- data
  
    comparisonDataBase[[attribute]] <- ((data2[[attribute]] - data[[attribute]])/data[[attribute]])*100
    
  ## Adjust infinite growth values to 1 (when the initial value =0
  comparisonDataBase <- comparisonDataBase %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x),1,x))
  comparisonDataBase <- comparisonDataBase %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x),0,x))
  
  print(comparisonDataBase)
  return(comparisonDataBase)

}


prepareSiloMapLabels <- function (dataLabel, model, feature, isComparison){
  if (feature == "dwellings"){
    
  }else if (feature == "income"){
    
  }else if (feature == "accessibilities"){
    
  }else{
  filteredLabels <- filter(dataLabel, myDataType == model & myItem == feature)
  title <- filteredLabels[1,4]
  legend <-filteredLabels[1,5]
  if(isComparison == TRUE){
  legend <- paste(" % of change in ", legend)
  }
  
  }
  return (c(title,legend))
  return(legend)
}
dummycall <-function(int){
  return(int)
}