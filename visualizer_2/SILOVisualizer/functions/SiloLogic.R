## SILO logic

## Spatial function for routing options

## Function prepareSiloMap : Call the geographical database and filter according to the parameters in UI
prepareSiloMap <- function (data, thisYear, zoneAgg, attribute, aggregationType, geoZones){
  ## Conditional to choose between aggregate or keep dissaggregate
  if(zoneAgg == "aggregated"){
    joinTable <- filter(data ,year == thisYear)
    spatialTable <- left_join(geoZones, joinTable, by="shp_id")
    
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
    groupedTable <- left_join(geoZones, joinTable, by="shp_id")
    if(aggregationType == "density"){
      groupedTable <-groupedTable%>% mutate(!!attribute := ((groupedTable[[attribute]] / shp_area) * 1000000 ))
    }
    
  }
  groupedTable <- groupedTable[!is.na(groupedTable[[attribute]]), ]
  return(groupedTable)
}
compareScenarios<-function(data, data2,attribute){
  comparisonDataBase <- data
  
    comparisonDataBase[[attribute]] <- ((data2[[attribute]] - data[[attribute]])/data[[attribute]])*100
    
  ## Adjust infinite growth values to 1 (when the initial value =0)
  comparisonDataBase <- comparisonDataBase %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x),1,x))
  comparisonDataBase <- comparisonDataBase %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x),0,x))
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
###############################
### Aspatial Overview

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
  
  population <- population%>%tidyr::pivot_longer(
    cols =c('population','men','women','households','dwellings'),
    names_to = 'Key',
    values_to = 'Value'
  )
  population <- population%>%rename(Year = popYear)
  return(population)
}

siloAspatialTableComparator <-function(baseTable, alternativeTable){
  comparisonDataBase <-baseTable
  iterateDataBase <-baseTable
  iterateDataBase[[1]] <- as.character(iterateDataBase[[1]])
  
  for(i in colnames(iterateDataBase)){
    if(typeof(iterateDataBase[[i]]) == 'numeric' | typeof(iterateDataBase[[i]]) == 'double' | typeof(iterateDataBase[[i]]) == 'integer'){
      comparisonDataBase[[i]] <- ((alternativeTable[[i]] - baseTable[[i]])/baseTable[[i]])*100
    }
  }
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
  dataTable <- dataTable%>%tidyr::pivot_longer(
    cols =c('shWhite','shBlack','shHispanic','shOther'),
    names_to = 'Key',
    values_to = 'Value'
  )
  dataTable <- dataTable%>%rename(Year = year)
  return(dataTable)
  
}
siloAspatialHHCarOwnership <- function(hhTable){
  hhTable <- hhTable%>%rename(Year = year, Key = carOwnershipLevel, Value = households)
  return(hhTable)
}
siloAspatialHHSize <- function(hhTable){
  hhTable['hh_size'] = substr(hhTable$type, start = 6, stop = 6)
  hhTable['hh_income'] = substr(hhTable$type, start = 12, stop = 20)
  hhTable <- hhTable%>%group_by(year, hh_size)%>%summarize_at(c('count'),sum)
  hhTable <- hhTable%>%rename(Year = year, Households = count)
  return(hhTable)
}
siloAspatialHHRentIncome <- function(hhTable){
  hhTable = hhTable %>% tidyr::pivot_longer(
    cols = starts_with('rent_'),
    names_to = 'Key',
    values_to = 'value',
    names_prefix = 'rent_'
  )
  hhTable <- hhTable%>%rename(Key2 = Income, Value = value, Year = year)
  # Rename values to give order in the plot
  
  hhTable[hhTable$Key == '250', 'Key'] <- '01 Income = 250'
  hhTable[hhTable$Key == '500', 'Key'] <- '02 Income = 500'
  hhTable[hhTable$Key == '750', 'Key'] <- '03 Income = 750'
  hhTable[hhTable$Key == '1000', 'Key'] <- '04 Income = 1000'
  hhTable[hhTable$Key == '1250', 'Key'] <- '05 Income = 1250'
  hhTable[hhTable$Key == '1500', 'Key'] <- '06 Income = 1500'
  hhTable[hhTable$Key == '1750', 'Key'] <- '07 Income = 1750'
  hhTable[hhTable$Key == '2000', 'Key'] <- '08 Income = 2000'
  hhTable[hhTable$Key == '2250', 'Key'] <- '09 Income = 2250'
  hhTable[hhTable$Key == '2500', 'Key'] <- '10 Income = 2500'
  hhTable$Key2 <- as.character( hhTable$Key2 )
  return(hhTable)
}
siloAspatialHHAvRent <-function(hhTable){
  hhTable <- hhTable %>% select(year,Income, averageRent)
  hhTable <- hhTable%>%rename(Year = year, Key = Income, Value = averageRent)
  hhTable$Key <- as.character( hhTable$Key )
  return(hhTable)
  
}
siloAspatialPopAge <-function(popTable){
  popTable <-popTable%>%rename(Male = men, Female = women)
  
  popTable <- popTable%>%tidyr::pivot_longer(
    cols =c('Male','Female'),
    names_to = 'Key',
    values_to = 'Value'
  )
  popTable <-popTable%>%rename(Key2 = age, Year = year)
  popTable$Key2 <- as.character( popTable$Key2 )
  return(popTable)
}
siloAspatialPopRace <-function(popTable){
  popTable <-popTable%>%rename(Year = year, Key =	ppByRace, Value =	hh)
  return(popTable)
}
siloAspatialPopParticipation <-function(popTable){
  popTable <-popTable%>%rename(Male = male, Female = female, Key2 = group, Year = year )
  popTable<-popTable%>%tidyr::pivot_longer(
    cols =c(Male,Female),
    names_to = 'Key',
    values_to = 'Value'
  )
  popTable$Key2 <- as.character( popTable$Key2)
  return(popTable)
}
siloAspatialpopMigration <-function(popTable){
  colnames(popTable)<- c("Year", "Key","Value")
  return(popTable)
}
siloAspatialDwellingQuality <-function(dwellTable){
  dwellTable <-dwellTable%>%rename(Year = year, Key = QualityLevel, Value = Dwellings)
  dwellTable$Key <- as.character( dwellTable$Key)
  return(dwellTable)
}
siloAspatialDwellings <-function(dwellTable, column){
  dwellTable <- dwellTable%>%rename(Year = year, Key = type, Value = column)
  dwellTable$Key <- as.character( dwellTable$Key)
  return(dwellTable)
}
siloAspatialEvents <- function(eventsTable, optionsVector){
  eventsTable <-subset(eventsTable, event %in% optionsVector)
  eventsTable <-eventsTable%>%rename(Year = year, Key = event, Value =count)
  eventsTable$Key <-as.character(eventsTable$Key)
  eventsTable <- eventsTable[with(eventsTable, order(Year, Key)),]
  #original_sorted <- original[with(original, order(Year, Key)),]
  return(eventsTable)
}
siloAspatialRegions <- function(regionTable, zonesData, varColumn){

  ## Delete geometry and group area by zone
  #zonesData$geometry <- NULL
  #regionArea <- zonesData %>%select (shp_muni, shp_area) 
  #regionArea <- zonesData %>%group_by(shp_muni) %>% summarise(area = sum(shp_area))
  #print("region Area")
  #print(regionArea)
  #print("Region data ")
  #print(regionTable)
  ## left join data
  #regionTable <- regionTable %>% left_join(regionArea, by = setNames(colnames(regionArea)[1], colnames(regionTable)[2]))
  #print(varColumn)
  #print("joined")
  #print(regionTable)


  regionTable <- regionTable%>% group_by(year) %>% summarise(min = quantile(!!as.name(varColumn), probs = 0,na.rm=TRUE),
                                                             q1 = quantile (!!as.name(varColumn), probs = 0.25,na.rm=TRUE),
                                                             q2 = mean (!!as.name(varColumn),na.rm=TRUE),
                                                             q3 = quantile (!!as.name(varColumn), probs = 0.75,na.rm=TRUE),
                                                             max = quantile(!!as.name(varColumn), probs = 1,na.rm=TRUE),)
  #print("Procesed table")
  #print(regionTable)
  return(regionTable)
}
siloAspatialJobsReg <-function(regionTable){
  regionJobs <- regionTable%>%tidyr::pivot_longer(
    cols = (colnames(regionTable)[3:13]),
    names_to = 'key',
    values_to = 'Value'
  )
  regionJobs <- regionJobs %>% group_by(year,key)%>%summarise(min = quantile(Value, probs = 0),
                                                              q1 = quantile(Value, probs = 0.25),
                                                              q2 = mean (Value, na.rm=TRUE),
                                                              q3 = quantile(Value, probs = 0.75),
                                                              max = quantile(Value, probs = 1))
  regionJobs <-regionJobs%>%rename(Year = year)
  return(regionJobs)
  
}
siloRegionalPlot <- function(regionTable, parameter, zone){
  zone <-substring(zone, 2)
  #zonesFiltered <- subset(zoneSystem, Gemeinde_ID == zone)
  #zone <-zonesFiltered$Region
  if(parameter == 'commuteTime'){
    regionTable <- regionTable%>%filter(aveCommuteDistByRegion %in% zone)
    regionTable <-regionTable%>%rename(Year = year,Key = aveCommuteDistByRegion, Value = minutes)
    regionTable$Year <- as.character(regionTable$Year)
    regionTable$Key <- as.character(regionTable$Key)
  }else if(parameter =='availableLand'){
    regionTable <- regionTable%>%filter(region %in% zone)
    regionTable <-regionTable%>%rename(Year = year,Key = region, Value = land)
    regionTable$Year <- as.character(regionTable$Year)
    regionTable$Key <- as.character(regionTable$Key)
  }else if(parameter == 'jobsByType'){
    regionTable <- regionTable%>%filter(jobByRegion %in% zone)
    columnNames <- colnames(regionTable)
    columnNames <- columnNames[! columnNames %in% c("year","jobByRegion")] 
    ## Iterate to cast to integers
    for(i in columnNames){
      regionTable[[i]] <- as.numeric(regionTable[[i]])
    }
    regionTable <- regionTable%>%tidyr::pivot_longer(
      cols = all_of(columnNames),
      names_to = 'Key',
      values_to = 'Value'
    )
    regionTable <- regionTable%>%rename(Year = year, Key2 = jobByRegion)
    regionTable$Year <- as.character(regionTable$Year)
    regionTable$Key <- as.character(regionTable$Key)  
    regionTable$Key2 <- as.character(regionTable$Key2)
  }
  return(regionTable)
}

dummycall <-function(int){
  return(int)
}
dummycall2 <-function(int){
  return(int)
}