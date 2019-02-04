pacman::p_load(sf, here)

original_file = paste(here(), "/map/muc/zonesNew.shp", sep="")


data = st_read(original_file)

data$shp_area = sf::st_area(data)
data$shp_id = data$id
data$shp_muni = data$AGS


data= data %>% select(shp_id, shp_muni, shp_area)


final_file = paste(here(), "/map/muc/zones_31468.shp", sep="")

sf::write_sf(data, final_file)
