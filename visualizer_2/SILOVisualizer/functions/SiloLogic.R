## SILO logic

## Spatial function for routing options

## Function prepareSiloMap : Call the geographical database and filter according to the parameters in UI
prepareSiloMap <- function (data, thisYear, zone_level, attribute){
  
  if(zone_level == FALSE){

    joinTable <- filter(data ,year == thisYear)
    spatialTable <- left_join(zones, joinTable, by="shp_id")
    groupedTable <- spatialTable %>%
    group_by(shp_muni)%>% summarize_at(vars(attribute), list(attribute = sum)) 
    
    names(groupedTable)[names(groupedTable) == "attribute"] <- attribute


    print(thisYear)
    print(zone_level)
    print(attribute)
    print(groupedTable)
    
    #operator 

  } else {
    joinTable <- filter(spatialData,year == 2020)
    groupedTable <- left_join(zones, joinTable, by="shp_id")
    
  }
  return(groupedTable)
  
} 
