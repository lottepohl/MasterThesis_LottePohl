# Script to get the acoustic detection data in the format for actel and RSP analysis

library(tidyverse)

rm(list = ls())

dir_path <- getwd() # "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

# source(paste0(dir_path, "/functions.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/03_wrangle_data/wrangle_acoustic_data.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_human_activities.R"))
rm(wrecks)

                                 

# 1. prepare biometrics file ####

Biometrics <- masterias_info %>% dplyr::select(release_date_time, tag_serial_number, acoustic_tag_id, length1, weight, wild_or_hatchery, Release.site, sex, life_stage, release_latitude, release_longitude) %>%
  mutate(length1 = length1 * 10, weight = weight * 1000, wild_or_hatchery = ifelse(wild_or_hatchery == "Wild", "wild", wild_or_hatchery), life_stage = ifelse(life_stage == "mature", "adult", life_stage),
         Signal = str_trunc(acoustic_tag_id, 4, "left", ellipsis = "") %>% as.integer(),
         Code.space = str_trunc(acoustic_tag_id, 8, "right", ellipsis = "")) %>% # convert from cm to mm, from kg to g
  rename(Release.date = release_date_time, Serial.nr = tag_serial_number, Length.mm = length1, Weight.g = weight, Group = sex, Life.stage = life_stage)



# 2. prepare spatial file ####

Spatial <- close_stations %>% mutate(Section = ifelse(deploy_longitude < WS1_lon_threshold, "BPNS", "Westerschelde"),
                                                 Type = "Hydrophone") %>%
  dplyr::select(station_name, deploy_latitude, deploy_longitude, Section, Array, Type) %>%
  rename(Station.name = station_name, Latitude = deploy_latitude, Longitude = deploy_longitude) %>%
  full_join(Biometrics %>% group_by(Release.site) %>% summarise(Latitude = mean(release_latitude), Longitude = mean(release_longitude)) %>% mutate(Array = ifelse(Release.site == "Westerschelde", "WS1", "coast"), Type = "Release") %>% rename(Station.name = Release.site))

# 3. prepare deployments file ####
Deployments <- deployments %>% filter(station_name %in% Spatial$Station.name) %>%
  dplyr::select(station_name, receiver_id, deploy_date_time, recover_date_time, deployment_id) %>%
  rename(Receiver = receiver_id, Station.name = station_name, Start = deploy_date_time, Stop = recover_date_time)

# 4. prepare detections file ####  

Detections <- masterias_detections_clean %>% dplyr::select(date_time, receiver_id, acoustic_tag_id, tag_serial_number, detection_id) %>% 
  mutate(Signal = str_trunc(acoustic_tag_id, 4, "left", ellipsis = "") %>% as.integer(),
         CodeSpace = str_trunc(acoustic_tag_id, 8, "right", ellipsis = ""), 
         Sensor.Value = NA, Sensor.Unit = NA,
         receiver_id = str_trunc(receiver_id, 6, "left", ellipsis = "") %>% as.integer()) %>%
  rename(Timestamp = date_time, Receiver = receiver_id)

# 5. prepare spatial.txt file ####

spatial <- "offshore -- coast -- WS1 -- WS2"
dot <- c("offshore -- coast -- WS1 -- WS2
         offshore -- WS1")

# dot <- 
#   "River0 -- River1 -- River3 -- River4 -- River5 -- River6 -- Fjord1 -- Fjord2 -- Sea1
# River1 -- River2 -- River3"


# Timestamp	Receiver	CodeSpace	Signal	Sensor.Value	Sensor.Unit
# 2018-01-01 11:30:00	11111	A12-3456	1234	10.0	T
# 2018-01-01 11:33:00	22222	A12-3456	1235	5.3	D
# 2018-01-01 11:34:00	33333	R64K	203	13.6	T
  
# Receiver	Station.name	Start	Stop
# 11111	River East	2018-01-01 11:30:00	2018-05-03 09:30:00
# 22222	River West	2018-01-01 11:33:00	2018-05-04 08:33:00
# 33333	Estuary	2018-01-01 11:34:00	2018-05-05 12:00:00

# change tz to local tz
# attr(Biometrics$Release.date, "tzone") <- "Europe/Brussels"

# map to see stations ####
# leaflet() %>% addTiles() %>%
#   addPolylines(data = cables,
#                color = "grey") %>%
#   addPolygons(data = windfarms_polygons,
#               color = "orange") %>%
#   addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
#                    weight = 0,
#                    radius = 3,
#                    fillOpacity = 1,
#                    lat = ~latitude,
#                    fillColor = "red",
#                    lng = ~longitude,
#                    label = ~Naam) %>%
#   addCircleMarkers(data = masterias_stations,
#                    lat = ~deploy_latitude,
#                    lng = ~deploy_longitude,
#                    label = ~station_name,
#                    labelOptions = labelOptions(noHide = TRUE)) %>%
#   addCircleMarkers(data = close_stations,
#                    weight = 0,
#                    radius = 3,
#                    fillOpacity = 1,
#                    lat = ~deploy_latitude,
#                    fillColor = "black",
#                    lng = ~deploy_longitude,
#                    label = ~station_name) %>%
#   leafem::addMouseCoordinates()


# spatial example ####
# Station.name	Latitude	Longitude	Section Array	Type
# River East	8.411     	40.411  	River  	A1  	Hydrophone
# River West	8.521      	40.521  	River 	A1	  Hydrophone
# Estuary	    8.402	     	40.402  	River	  A3	  Hydrophone
# Site A    	8.442     	40.442 	        	A1    	Release
# Site B    	8.442	     	40.442	        	A2    	Release
