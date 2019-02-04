pacman::p_load(sf, here)

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

