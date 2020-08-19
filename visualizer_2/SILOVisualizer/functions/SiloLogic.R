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
    
  ## Adjust infinite growth values to 1 (when the initial value =0)
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
siloAspatialOverview <-function(populationTable, dwellingsTable, hhSizeTable){
  population_men <- aggregate(populationTable$men, by=list(popYear= populationTable$year), FUN=sum)
  population_women <- aggregate(populationTable$women, by=list(popYear= populationTable$year), FUN=sum)
  dwellings <- aggregate(dwellingsTable$count, by=list(dwelling= dwellingsTable$year), FUN=sum)
  households <- aggregate(hhSizeTable$count, by=list(households = hhSizeTable$year), FUN = sum)
  
  men <- population_men['x']
  women <- population_women['x']
  households <- households['x']
  dwellings <- dwellings['x']
  population <- data.frame(population_men['popYear'],population_men['x'],population_women['x'],households['x'],dwellings['x'])
  population <- population%>%rename(men = x, women = x.1, households = x.2, dwellings = x.3 )
  population$population <- population$men + population$women
  
  return(population)
}

siloAspatialTableComparator <-function(baseTable, alternativeTable){
  comparisonDataBase <-baseTable
  iterateDataBase <-baseTable
  iterateDataBase[[1]] <- as.character(iterateDataBase[[1]])
  
  #print('Before process')
  #print( sapply(iterateDataBase, mode))
  #print(iterateDataBase)

  for(i in colnames(iterateDataBase)){
    if(typeof(iterateDataBase[[i]]) == 'numeric' | typeof(iterateDataBase[[i]]) == 'double' | typeof(iterateDataBase[[i]]) == 'integer'){
      comparisonDataBase[[i]] <- ((alternativeTable[[i]] - baseTable[[i]])/baseTable[[i]])*100
    }
  }
  #print('after process')
  #print(comparisonDataBase)
  return(comparisonDataBase)
}

siloAspatialHHSizeIncome <- function(hhTable){
  hhTable['hh_size'] = substr(hhTable$type, start = 6, stop = 6)
  hhTable['hh_income'] = substr(hhTable$type, start = 12, stop = 20)
  
  hhTable[hhTable$hh_income == 'LOW', "hh_income"] <- '1_LOW'
  hhTable[hhTable$hh_income == 'MEDIUM', "hh_income"] <- '2_MEDIUM'
  hhTable[hhTable$hh_income == 'HIGH', "hh_income"] <- '3_HIGH'
  hhTable[hhTable$hh_income == 'VERY_HIGH', "hh_income"] <- '4_VERY_HIGH'
  hhTable <- hhTable%>%rename(Year = year, Value = count, Key2 = hh_income, Key =hh_size)
  return(hhTable)
}
siloAspatialRace <- function(hhRaceTable){
  
  dataTable<- hhRaceTable%>%group_by(year)%>%summarize_at(c('shWhite','shBlack','shHispanic','shOther'),sum)

  print(dataTable)
  return(dataTable)
  
}

siloAspatialHHSize <- function(hhTable){
  hhTable['hh_size'] = substr(hhTable$type, start = 6, stop = 6)
  hhTable['hh_income'] = substr(hhTable$type, start = 12, stop = 20)
  print(hhTable)
  hhTable <- hhTable%>%group_by(year, hh_size)%>%summarize_at(c('count'),sum)
  hhTable <- hhTable%>%rename(Households = count, Year = year)
  print(hhTable)
  return(hhTable)
}



dummycall <-function(int){
  return(int)
}