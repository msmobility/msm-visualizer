# Define variables to be used in the application
myModeNames <- c("Car Driver", "Car Passenger", "Bicycle", "Bus", "Train", 
                 "Tram/Metro", "Walk", "Private AV", "Shared AV")
myModeCodes <- c("autoDriver", "autoPassenger", "bicycle", "bus", "train", 
                 "tramOrMetro", "walk", "privateAV", "sharedAV")
myYLabels <- c("Number of Persons", "Number of Households", "Frequency", "Frequency", "Mode Share")
myXLabels <- c("Number of Trips", "Number of Trips", "Travel Distance (km)", "Travel Time (minutes)", "Mode")
myAspatial <- c("Persons by Number of Trips", "Households by Number of Trips", "Travel Distance Distribution",
                "Travel Time Distribution", "Mode Share")
myAspatialCodes <- c("PPbyTrips", "HHbyTrips", "Distance", "Time", "ModeShare")
mySpatial <- c("Produced Trips per sq.km", "Attracted Trips per sq.km", "Average Travel Time Budget (Minutes)", 
               "Average Distance Travelled (Km)", "Average Time Travelled (Minutes)")
mySpatialCodes <- c("P", "A", "TTB", "AvDist", "AvTime")
myPurposeNames <- c("Home-based Work", "Home-based Education", "Home-based Shopping", "Home-based Other",
                    "Non-home-based Work", "Non-home-based Other")
myPurposeCodes <- c("HBW", "HBE", "HBS", "HBO", "NHBW", "NHBO")
mySpatialStyles <- c("quantile", "quantile", "pretty", "pretty", "pretty")

# Assign names to vectors
names(myAspatialCodes) <- myAspatial
names(mySpatialCodes) <- mySpatial
names(myPurposeCodes) <- myPurposeNames

# Define a function that returns the name of the selected attribute
myNaming <- function(myNames, myCodes, mySelect){
  names(myCodes) <- myNames
  return (names(myCodes[myCodes == mySelect]))
}