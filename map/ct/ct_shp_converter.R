pacman::p_load(sf, here)

#This script converts a shapefile into the format required to run the visualizer. 
#There are no requirements regarding the coordinate system
#The shapefile MUST contain the following fields with exact the following names
# shp_area is the are in sq meters of the zone (can be calculated using the function sf::st_area())
# shp_id is the id of the zone
# shp_muni is the municipality/region/county/etc. use for the aggregation of zones for visualization purposes
# 
# (A fourth variable is added in the R dataframe called data in this script - it is the geometry of each feature)
# 
# Some files did not work properly showing "topology errors" while running the visualizer. This has been solved simplyfing the shapefile 
# using the sf::st_simplify(), although the consequences of this have not been analyzed yet. 
#
# Please adapt the code below to the specific case

original_file = paste(here(), "/map/ct/zones.shp", sep="")

data = st_read(original_file)

data$shp_area = sf::st_area(data)
data$shp_id = data$ID_cell
data$shp_muni = data$ID_city

data= data %>% select(shp_id, shp_muni, shp_area)


final_file = paste(here(), "/map/ct/zones_ct.shp", sep="")

sf::write_sf(data, final_file)
