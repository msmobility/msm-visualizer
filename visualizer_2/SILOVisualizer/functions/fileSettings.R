## fileSettings.R
## Reads the visalizerMenuOptions and apply the categories to each

## Read graphic options

menuSettings <- read_excel(paste(here(),"visualizer_2/SILOVisualizer/visualizerMenuOptions.xlsx",sep="/"), sheet = "menu")
configuration <- read_excel(paste(here(),"visualizer_2/SILOVisualizer/visualizerMenuOptions.xlsx",sep="/"), sheet ="config")
## Read labels

myLabels <- read_excel(paste(here(),"visualizer_2/SILOVisualizer/visualizerLabels.xlsx", sep="/"), sheet= "Sheet1" ) 

## Get local parameters
parameters <- unlist(filter(configuration, implementation == 'muc'))
initialYear <- as.numeric(parameters[7])
finalYear <- as.numeric(parameters[8])

## Create lists for aspatial categories

aHH <- unlist(filter(menuSettings, visualization =='aspatial'& attribute_name =='Households') %>%select(category_value))
names(aHH) = unlist(filter(menuSettings, visualization =='aspatial'& attribute_name =='Households')%>%select(category_name))

aPerson <- unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Persons')%>%select(category_value))
names(aPerson) <-unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Persons')%>%select(category_name))

aDwelling <- unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Dwellings')%>%select(category_value))
names(aDwelling) <-unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Dwellings')%>%select(category_name))

aRegional <- unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Regional')%>%select(category_value))
names(aRegional) <-unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Regional')%>%select(category_name))

aEvent <- unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Events')%>%select(category_value))
names(aEvent) <-unlist(filter(menuSettings, visualization == 'aspatial' & attribute_name == 'Events')%>%select(category_name))

## Create lists for spatial categories

sDwelling <- unlist(filter(menuSettings, visualization == 'spatial' & attribute_name == 'Dwellings')%>% select(category_value))
names(sDwelling) <-unlist(filter(menuSettings, visualization =='spatial' & attribute_name == 'Dwellings')%>% select(category_name))

sIncome <- unlist(filter(menuSettings, visualization =='spatial' & attribute_name =='Income')%>%select(category_value))
names(sIncome) <- unlist(filter(menuSettings, visualization =='spatial' & attribute_name == 'Income')%>% select(category_name))

sAccessibility <- unlist(filter(menuSettings, visualization =='spatial' & attribute_name =='Accessibilities')%>%select(category_value))
names(sAccessibility) <- unlist(filter(menuSettings, visualization =='spatial' & attribute_name == 'Accessibilities')%>% select(category_name))

## Read zones areas and spatial data (to be fixed)


zones <- st_read(paste(here(),parameters[9],sep="/"))

print(finalYear)

### Events

hhEvents <- c('MigrationEvent', 'MoveEvent')
perEvents <- c('BirthEvent', 'DeathEvent','DivorceEvent','EducationEvent','EmploymentEvent','LeaveParentsEvent','LicenseEvent','MarriageEvent')
dwellEvents<- c('ConstructionEvent','DemolitionEvent','RenovationEvent')
