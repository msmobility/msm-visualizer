##################################################
## fileSettings.R
## Filesettings contains all the visualizer default values and loads the predefined values and lists
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

zones <- st_read(paste(here(),parameters[9],sep="/"))

################################## Aspatial Categories ##################################

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

### Events

hhEvents <- c('MigrationEvent', 'MoveEvent')
perEvents <- c('BirthEvent', 'DeathEvent','DivorceEvent','EducationEvent','EmploymentEvent','LeaveParentsEvent','LicenseEvent','MarriageEvent')
dwellEvents<- c('ConstructionEvent','DemolitionEvent','RenovationEvent')

################################## File Variables ##################################

fileList = c('aveHhSize.csv','carOwnership.csv',
             'commutingDistance.csv','dwellingQualityLevel.csv',
             'dwellings.csv','eventCounts.csv',
             'hhAveIncome.csv','hhRentAndIncome.csv',
             'hhSize.csv','hhType.csv','jobsBySectorAndRegion.csv',
             'labourParticipationRate.csv','landRegions.csv',
             'persByRace.csv','persMigrants.csv',
             'popYear.csv','regionAvailableLand.csv',
             'regionAvCommutingTime.csv','resultFileSpatial.csv'
)
fileNames = c('aveHhSize','c_owne','com_di','dwelQu','dwelli','eventc','hhAvIn','hhReIn','hhSize','hhType','regJoS',
              'laPaRa','lanReg','perRac','perMig','popYea','regAvL','regCoT','spatialData')
dataVec <-list()
