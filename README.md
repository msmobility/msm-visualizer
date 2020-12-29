# MSM-visualizer V 2.0

R and shiny based tool to visualize results produced by the integrated land-use/transport models SILO.
## Main functions

Visualize spatial and aspatial model variables and compare scenarios. The zone-related variables can be visualized using the "click on map" function. The scenarios can be changed in order to compare different scenarios.

## Run and open examples
Open the msm-visualizer.R and then open the visualizer_2/ui.r. After that, click on "Run app"

Once the application is open, look for the desire example in the use_cases folder and select one scenario folder. To compare scenarios, click the checkbox "Compare scenrios" and select the folder corresponding o the second scenario.

If you change the implementation, the map will automatically change.
## Available examples
You can explore the tool with the two available use cases:
 - Munich
 - Kagawa
## Load your own use-case
You can also load your own scenarios based on SILO outputs. To load your own use case, please follow the next steps:
#### Folder structure
- Create your own implementation folder in the use_cases folder. It is recommendable to use a 3-letter name (f.e. muc)
- In your new folder, create a new folder for each scenario you want to visualize, paste the output .csv files (the full list of .csv files is in the "scenarios" section)
- In the implementation folder, create a new folder called "zone_shapefile", and paste your zones shapefile

#### Shapefiles
The shapefile must be named as "zone_system.shp" and must contain at least the columns:
- shp_id
- shp_muni
- shp_area
  
Any question regarding the reference system or columns data type please refeer to the examples
#### Scenarios

This visualizer works with the new SILO output style, even though not all the tables are needed, the recommended csv files are:

- aveHhSize.csv
- carOwnership.csv
- commutingDistance.csv
- dwellingQualityLevel.csv
- dwellings.csv
- eventCounts.csv
- hhAveIncome.csv
- hhRentAndIncome.csv
- hhSize.csv
- hhType.csv
- jobsBySectorAndRegion.csv
- labourParticipationRate.csv
- landRegions.csv
- persByRace.csv
- persMigrants.csv
- popYear.csv
- regionAvailableLand.csv
- regionAvCommutingTime.csv
- resultFileSpatial.csv

Please check the examples to get details about the data structure.
## Recommendations
If you want to use the scenario tool, make sure to have the same .csv file types in each scenario and also have the same time horizon. 
## What if i need help?

Please contact Rafael Muñoz by slack (Rafael Muñoz) or email: rafael.nieto@tum.de. He will contact you as soon as possible.
