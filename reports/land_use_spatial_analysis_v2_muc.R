pacman::p_load(readr, dplyr, ggplot2, reshape, plotly, sf, leaflet, tmap, tidyr)


folder = "examples/muc/"

scenarios = c("av0", "av2")
scenario_names = c("Sc1", "Sc2")


names = data.frame(code = scenarios, name = scenario_names)
file_name = "/resultFileSpatial_2.csv"

file_name_2 = "/siloResults/commutingDistance.csv"

shapefile_name = "map/muc/zones_31468.shp"

shp = st_read(shapefile_name)

zones_regions = read_csv("examples/muc/zoneSystem.csv")
zones_regions = zones_regions %>% select(zone = Zone, region = Region, lkr = Landkreis_ID)
spatial = data.frame()

shp = shp %>% left_join(zones_regions, by=c("shp_id" = "zone"))
shp_regions = shp %>% group_by(region) %>% summarise()


lkrs_core = c(9162, 9761, 9161, 9163, 9261)
    
zones_urban = zones_regions %>% mutate (urban = if_else(lkr %in% lkrs_core, 1, 0))
zones_urban = zones_urban %>% select(zone, urban)

commuting_distance = data.frame()
events = data.frame()

for (scenario in scenarios){
  scenario_name = (names %>% filter(code == scenario))$name
  this_spatial = read_csv(paste(folder,scenario,file_name,sep=""))
  this_spatial$scenario = scenario_name
  spatial = spatial %>% bind_rows(this_spatial)
  this_commuting_distance = read_csv(paste(folder,scenario,file_name_2,sep = ""))
  this_commuting_distance$scenario = scenario_name
  commuting_distance = commuting_distance %>% bind_rows(this_commuting_distance)
  this_events = read_csv(paste(folder,scenario,"/siloResults/eventCounts.csv",sep = ""))
  this_events$scenario = scenario_name
  events = events %>% bind_rows(this_events)
  
}

spatial = spatial %>% left_join(zones_regions, by = "zone")
spatial = spatial %>% left_join(zones_urban, by = "zone")
spatial$dd = spatial$dd_SFD + spatial$dd_SFA + spatial$dd_MF234 + spatial$dd_MF5plus + spatial$dd_MH

#population & jobs by region at the base year and the final year
selected_years = c(2010, 2040)

# df_1 = spatial %>% filter(year %in% selected_years) %>% group_by(scenario,year,region) %>% summarize(pp = sum(population), jj = sum(jobs))
# 
# ggplot(df_1, aes(group=year, x =as.factor(region), y = pp, fill = as.factor(year))) +
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   facet_grid(.~scenario) + 
#   scale_fill_manual(values = c("#c77c7c", "#721e1e"))
# 
# ggplot(df_1, aes(group=scenario, x =as.factor(region), y = pp, fill = as.factor(scenario))) +
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   facet_grid(.~year)
# 
# 
# ggplot(df_1, aes(group=year, x =as.factor(region), y = jj, fill = as.factor(year))) +
#   geom_bar(stat = "identity", position = position_dodge()) + 
#   facet_grid(.~scenario) + 
#   scale_fill_manual(values = c("#c77c7c", "#721e1e"))


df_20 = spatial %>% group_by(scenario,year) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd=sum(dd), 
            hh = sum(households),
            acc = weighted.mean(autoAccessibility, population))

ggplot(df_20, aes(x=year, y = pp, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  ggtitle("Population")

ggplot(df_20, aes(x=year, y = dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  ggtitle("Dwellings")


df_2 = spatial %>% group_by(scenario,year,urban) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd=sum(dd), 
            hh = sum(households),
            acc = weighted.mean(autoAccessibility, population))

ggplot(df_2, aes(x=year, y = pp, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban) +
  ggtitle("Population")

ggplot(df_2, aes(x=year, y = hh, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban)  +
  ggtitle("Households")

ggplot(df_2, aes(x=year, y = pp/hh, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban) +
  ggtitle("Household size")

ggplot(df_2, aes(x=year, y = dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban) +
  ggtitle("Number of dwellings")

ggplot(df_2, aes(x=year, y = dd-hh, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban, scales = "free") +
  ggtitle("Number of vacant dwellings")
#ggplotly()


ggplot(df_2, aes(x=year, y = price, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban) +
  ggtitle("Average dwelling price")

ggplot(df_2, aes(x=year, y = 1 - hh/dd, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_grid(.~urban) +
  ggtitle("Global vacancy rate")

ggplot(df_2, aes(x=year, y = acc, color = as.factor(scenario))) +
  geom_path(size = 1) + 
  facet_wrap(.~urban) +
  ggtitle("Average auto accessibility (employment-calculation and population-averaging)")
#ggplotly()



df_4 = spatial %>% group_by(scenario,year,region) %>%
  summarize(pp = sum(population),
            jj = sum(jobs),
            price = weighted.mean(avePrice,dd),
            dd = sum(dd), hh = sum(households),
            acc = weighted.mean(autoAccessibility, jobs))

df_5 = commuting_distance %>% left_join(df_4, by = c("region", "year", "scenario"))

region_types = spatial %>% filter(year == 2011) %>%  select(region, urban) %>% unique()

df_5 = df_5 %>% left_join(region_types, by = "region")  

df_10 = df_5 %>% group_by(year, scenario, urban) %>% summarize(avgTime = weighted.mean(time, pp))

ggplot(df_10, aes(x=year, y = avgTime, color = scenario)) +
  geom_path() +
  facet_wrap(.~urban) + 
  ggtitle("Global average commuting distance by origin region")
#ggplotly()





df_6 = df_5 %>% filter (year == 2049) %>%
  group_by(region, scenario) %>% summarise(time = time) %>% spread(key = scenario, value = time)

df_40 = df_4 %>% filter (year == 2049) %>%
  select(scenario, pp, region)  



df_40 = df_40 %>% 
  spread(key = scenario, value = pp )




#do maps:
# shp_regions = shp %>% group_by(region) %>% summarise()
# shp_regions = shp_regions %>% left_join(df_40, by = "region")
# shp_regions = shp_regions %>% 
#   mutate(rel_penalty_200_25 = penalty_200_25 - base) %>% 
#   mutate(rel_oneCar = oneCar - base) %>% 
#   #mutate(rel_restrict_development_out_of_cores = restrict_development_out_of_cores - base) %>% 
#   mutate(rel_restrict_development_out_of_cores_growth = restrict_development_out_of_cores_growth - base) %>% 
#   mutate(rel_reduce_local_to_20 = reduce_local_to_20 - base) %>% 
#   mutate(rel_reduce_highway_to_70 = reduce_highway_to_70 - base)
#   
# p  = tm_basemap(leaflet::providers$CartoDB) + tm_shape(shp_regions) + tm_borders()
#                   
# for (scenario in scenario_names){
#   if (scenario != "base"){
#       p =  p + tm_shape(shp_regions, name = scenario) +
#         tm_fill(col =as.character(paste("rel_", scenario, sep = ""))) +
#                   tm_borders()
#   }
#  
# }
# tmap_leaflet(p)
# ggplot(df_4, aes(x=year, y = pp, color = as.factor(scenario))) +
#   geom_path(size = 1) + 
#   facet_wrap(.~region, ncol = 3, scales = "free")


ggplot(events %>% filter(event == "ConstructionEvent" |
                           event == "MoveEvent"|
                           event == "RenovationEvent"), aes(x=year, y=count, color = scenario)) + geom_line() + 
  facet_wrap(.~event, ncol = 3, scales = "free") + 
  theme(legend.position = "bottom")
ggplotly()

