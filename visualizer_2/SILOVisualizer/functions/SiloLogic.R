## SILO logic

## Spatial function for routing options

## Function prepareSiloMap : Call the geographical database and filter according to the parameters in UI
prepareSiloMap <- function (data, thisYear, zone_level, attribute, aggregationType){
  ## Conditional to choose between aggregate or keep dissagregate
  if(zone_level == FALSE){

    joinTable <- filter(data ,year == thisYear)
    spatialTable <- left_join(zones, joinTable, by="shp_id")
    
    if(aggregationType == "average") {
      groupedTable <- spatialTable %>%
      group_by(shp_muni)%>% summarize_at(vars(attribute), list(attribute = mean))
      
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
  return(groupedTable)
  
}
prepareSiloMapScenario<-function(data, data2, thisYear, zone_level, attribute, aggregationType){
  if(zone_level == FALSE){
    joinTable <- filter(data ,year == thisYear)
    joinTable2 <-filter(data2, year == thisYear)
    joinScenarios <-left_join(joinTable, joinTable2, by="shp_id")
    
  }else{
    ## Logic without grouping
  }
}

prepareSiloMapLabels <- function (dataLabel, model, feature){
  if (feature == "dwellings"){
    
  }else if (feature == "income"){
    
  }else if (feature == "accessibilities"){
    
  }else{
  filteredLabels <- filter(dataLabel, myDataType == model & myItem == feature)
  title <- filteredLabels[1,4]
  legend <-filteredLabels[1,5]
  }
  return (c(title,legend))
  return(legend)
}
dummycall <-function(int){
  return(int)
}