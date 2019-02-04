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

original_file = paste(here(), "/map/mstm/zones_mstm_100026.shp", sep="")

data = st_read(original_file)

data$shp_area = data$AREA_SQMI * 1.6^2 * 1e6  # in sq_m
data$shp_id = data$SMZRMZ
data$shp_muni = data$STCOFIPS

data= data %>% select(shp_id, shp_muni, shp_area)

#for some reasons I need to simplify the shapefile without known consequences (maybe islands are removed)
data = data %>% sf::st_simplify()

final_file = paste(here(), "/map/mstm/zones_mstm_100026_clean.shp", sep="")

sf::write_sf(data, final_file)

