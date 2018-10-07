### Variables and functions for SILO and MITO visualizers

## Variables required for Silo Aspatial visualization
siloAspatialCategory <- c("Overview", "Households", "Persons", "Dwellings", "Regional", "Events")

siloHHAttributes <- c("hhByType", "hhByRace", "hhBySize", "AveHHSize", "AveHHInc", "carOwnershipLevel", "Income")
names(siloHHAttributes) <- c("By size and income", "By race", "By size", "Average household size",
                            "Household income", "Car ownership level", "By rent and income")

siloPPAttributes <- c("Age", "ppByRace", "laborParticipationRateByAge")
names(siloPPAttributes) <- c("By age and gender", "By race", "Labor participation rate")

siloDDAttributes <- c("QualityLevel", "CountOfDD", "AveMonthlyPrice", "AveVacancy")
names(siloDDAttributes) <- c("By quality level", "By type", "Average monthly price", "Average vacancy")

siloRegAttributes <- c("aveCommuteDistByRegion", "Available", "jobByRegion")
names(siloRegAttributes) <- c("Average commute distance", "Available land", "Jobs by sector and region")

siloMigrants <- c("InmigrantsPP", "OutmigrantsPP")

siloHHEvents <- c("AddedCar", "RelinquishedCar", "MoveEvent", "SwitchedToAV", "MigrationEvent")

siloPPEvents <- c("BirthEvent", "EducationEvent", "LeaveParentsEvent", "EmploymentEvent", "LicenseEvent", 
                  "MarriageEvent", "DivorceEvent", "DeathEvent", "BirthDayEvent")

siloDDEvents <- c("ConstructionEvent", "RenovationEvent", "DemolitionEvent")

siloAspatialAll <- c(siloHHAttributes, siloPPAttributes, siloDDAttributes, siloRegAttributes,
                     siloHHEvents, siloPPEvents, siloDDEvents, siloMigrants)

siloAspatialWithHeaders <- c(siloPPAttributes, siloRegAttributes, "hhByType", "hhByRace", "Income",
                             "carOwnershipLevel", "QualityLevel")

# create headers for rent categories and job types (column headers have to start with an alphabet)
siloHouseRents <- c("ii250", "ii500", "ii750", "i1000", "i1250", "i1500", "i1750", "i2000", "i2250", "i2500")
siloJobTypes <- c("Agriculture", "Manufacturing", "Utility", "Construction", "Retail", "Transportation", 
                  "Finance", "Real Estate", "Administration", "Services")
siloHHSizes <- c("ii1", "ii2", "ii3", "ii4", "ii5", "ii6", "ii7", "ii8", "ii9", "i10")

# variables to recode income values for appropriate ordering in animated plot legends
siloIncomesNum <- c(10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
siloIncomesChar <- c(" 10000", " 20000", " 30000", " 40000", " 50000", " 60000",
                     " 70000", " 80000", " 90000", "100000")

## Variables required for Silo Spatial visualization
siloDwellings <- c("dd_SFD", "dd_SFA", "dd_MF234", "dd_MF5plus", "dd_MH")
names(siloDwellings) <- c("Single family detached", "Single family attached", "Multi-family <= 4 floors",
                          "Multi-family > 4 floors")

siloIncomeClasses <- c("hhInc_<20000", "hhInc_>20000", "hhInc_>40000", "hhInc_>60000")
names(siloIncomeClasses) <- c("Income < 20K", "Income > 20K", "Income > 40K", "Income > 60K")

siloAccessibilities <- c("autoAccessibility", "transitAccessibility")
names(siloAccessibilities) <- c("By car", "By transit")

siloMainAttributes <- c("population", "households", "jobs", "availLand")
names(siloMainAttributes) <- c("Population", "Households", "Jobs", "Available land")

siloSpatialCategory <- c(siloMainAttributes, "avePrice", "dwellings", "accessibilities", "incomes")
names(siloSpatialCategory) <- c(names(siloMainAttributes), "Average price", "Dwellings", "Accessibilities", "Incomes")


# Variables required for Mito Visualization
mitoPurposes <- c("HBW", "HBE", "HBS", "HBO", "NHBW", "NHBO")
names(mitoPurposes) <- c("Home-based Work", "Home-based Education", "Home-based Shopping", "Home-based Other",
                         "Non-home-based Work", "Non-home-based Other")

mitoAspatialCategory <- c("PPbyTrips", "HHbyTrips", "Distance", "Time", "ModeShare")
names(mitoAspatialCategory) <- c("Persons by number of trips", "Households by number of trips", 
                                 "Travel distance distribution", "Travel time distribution", "Mode share")

mitoSpatialCategory <- c("P", "A", "TTB", "AvDist", "AvTime")
names(mitoSpatialCategory) <- c("Produced trips per sq.km", "Attracted trips per sq.km", "Average travel time budget", 
                                "Average distance travelled", "Average time travelled")

# variables to recode travel modes into appropriate names
mitoModeNames <- c("Car Driver", "Car Passenger", "Bicycle", "Bus", "Train", 
                 "Tram/Metro", "Walk", "Private AV", "Shared AV")
mitoModes <- c("autoDriver", "autoPassenger", "bicycle", "bus", "train", 
                 "tramOrMetro", "walk", "privateAV", "sharedAV")
