# Script to change and wrangle the acoustic detection data

library(lubridate)
library(dplyr)
library(sf)
library(suncalc)
library(readxl)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path <- paste0(dir_path, "/00_data/acoustic_detections/")
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()
# source(paste0(dir_path, "/01_code/01_save_data/load_acoustic_data_make_from_ETN.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R"))

# 1. INFO AND DETECTIONS ####

masterias_info <- clean_animal_info(masterias_info, masterias_detections) %>%
  mutate(Release.site = ifelse(release_latitude < 51.55, "Westerschelde", "Oosterschelde"))

# make dateyear and time column in masterias_detections
masterias_detections <- masterias_detections %>% mutate(tag_serial_number = tag_serial_number %>% as.character(),
                                                        time = date_time %>% as.Date("%H:%M", tz = "UTC"),
                                                        month = date_time %>% lubridate::month(),
                                                        month_name = month.abb[month] %>% factor(levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")),
                                                        station_name = ifelse(station_name == "bpns-Cpowerreefballs-CPOD", "bpns-CPowerReefballs", station_name))
# 
# masterias_detections$time <- masterias_detections$date_time %>% as.Date("%H:%M", tz = "UTC")
# # masterias_detections$season <- ifelse(month(masterias_detections$date_time) >=4 & month(masterias_detections$date_time) < 11, "summer", "winter") # only summer detections anyway
# # include months
# masterias_detections$month <- masterias_detections$date_time %>% lubridate::month()
# masterias_detections$month_name <- 
# 
# # input sex and release_date_time into detections dataframe
# masterias_detections$tag_serial_number <- masterias_detections$tag_serial_number %>% as.character()

masterias_detections <- masterias_detections %>% left_join(masterias_info %>% dplyr::select(tag_serial_number, sex, release_dateyear), by = "tag_serial_number")

# remove tags with only 1 detection 
detection_summary <- masterias_detections %>% mutate(count = 1) %>% group_by(tag_serial_number) %>% summarise(n_detect = sum(count))
# input into masterias_info
masterias_info <- masterias_info %>% left_join(detection_summary, by = "tag_serial_number")

tag_serial_number_omit <- detection_summary %>% filter(n_detect == 1) %>% dplyr::select(tag_serial_number) 

masterias_detections_clean <- masterias_detections %>% filter(!tag_serial_number %in% tag_serial_number_omit$tag_serial_number) #%>%
#   # calculate pressure and temp values
#   left_join(tag_info %>% dplyr::select(sensor_type, acoustic_tag_id, sensor_intercept, sensor_slope), by = "acoustic_tag_id") %>%
#   mutate(temp_c = ifelse(sensor_type == "temperature", (sensor_value * sensor_s)))
# masterias_depth_temp %>% group_by(tag_serial_number) %>% summarise(min_depth = min(depth_m), max_depth = max(depth_m)) %>% View()

# remove deployments outside of detection time frame
deployments <- deployments %>% filter(deploy_date_time <= masterias_detections_clean$date_time %>% max()) %>%
  filter(!recover_date_time %>% is.na())
  # filter(recover_date_time <= masterias_detections_clean$date_time %>% max()) # %>% 
  # filter(deploy_date_time > masterias_detections_clean$date_time %>% min()) # thats wrong!

summary_masterias_length <- masterias_info %>% group_by(sex) %>% summarise(mean_length = length1 %>% mean(), sd_length = length1 %>% sd())
masterias_info_detected <- masterias_info %>% filter(!n_detect %>% is.na())


# 2. RECEIVER STATIONS ####

open_stations <- deployments %>%
  distinct(station_name, deploy_latitude, deploy_longitude) %>%
  mutate(station_name = ifelse(station_name == "bpns-Buitenratel", "bpns-buitenratel", station_name))
# BPNS-cpower reefballs is not in the open deployments, neither is BPNSlottobuoy!! FOLLOW UP!

close_stations <- open_stations %>% 
  filter(between(deploy_latitude, min_lat, max_lat)) %>% 
  filter(between(deploy_longitude, min_lon, max_lon)) %>%
  filter(deploy_longitude < 3.9 & deploy_latitude > 51.1) %>%
  filter(deploy_latitude < 51.9) %>%
  full_join(masterias_detections_clean %>% group_by(station_name) %>% summarise(deploy_latitude = mean(deploy_latitude), deploy_longitude = mean(deploy_longitude)), 
            by = c('station_name', 'deploy_latitude', 'deploy_longitude')) %>%
  group_by(station_name) %>% 
  summarise(deploy_latitude = mean(deploy_latitude), deploy_longitude = mean(deploy_longitude)) %>%
  mutate(st_as_sf(., coords = c("deploy_longitude", "deploy_latitude"), crs = st_crs(coastline)) %>% dplyr::select(geometry))

## 2.a. distance to shoreline and categorisation into coast and offshore ####

WS1_lon_threshold <- 3.38
WS2_lon_threshold <- 3.6

coast_distances <- st_distance(close_stations$geometry, coastline) %>%
  apply(., 1, min)
close_stations <- close_stations %>% mutate(dist_to_coast = coast_distances / 1000,
                                            area = ifelse(deploy_longitude < WS1_lon_threshold, "BPNS", ifelse(deploy_longitude < WS2_lon_threshold, "WS1", "WS2")),
                                            Array = ifelse(area != "BPNS", area, ifelse(dist_to_coast > 20, "offshore", "coast")))

# leaflet() %>% addTiles() %>%
#   addPolylines(data = coastline, weight = 1) %>%
#   addPolygons(data = BPNS) %>%
#   addCircleMarkers(data = close_stations,
#                    lat = ~deploy_latitude,
#                    lng = ~deploy_longitude,
#                    weight = 0,
#                    fillOpacity = 1,
#                    fillColor = "black",
#                    radius = 3,
#                    label = ~paste0("station: ", station_name, ", coast dist: ", dist_to_coast)) %>%
#   leafem::addMouseCoordinates() %>%
#   addScaleBar(position = "topright",
#               options = scaleBarOptions(imperial = F, maxWidth = 300))


# 3. AREAS ####

masterias_stations <- close_stations %>% filter(station_name %in% masterias_detections_clean$station_name)


# include areas in masterias_detections as well
masterias_detections_clean <- masterias_detections_clean %>% left_join(masterias_stations %>% dplyr::select(station_name, area), by = "station_name") 

# # areas work
# col_fun_rd <- ggsci::pal_ucscgb()(3)
# pal_rd <- leaflet::colorFactor(col_fun_rd, domain = masterias_stations$area)
# 
# map_base %>% addCircleMarkers(data = masterias_stations,
#                               lat = ~deploy_latitude,
#                               lng = ~deploy_longitude,
#                               label = ~station_name, 
#                               color = ~pal_rd(area))


# maybe less resolution: spring (april - may), early summer (jun - jul), late summer (aug - sept), autumn (october)

detections_station_season_sex <- 
  masterias_detections_clean %>%
  group_by(station_name, month, sex) %>%
  count() %>%
  rename(detections_count = n) %>%
  arrange(desc(detections_count))
# detections_station_season_sex %>% View()

masterias_ind_per_station_season_sex <- 
  masterias_detections_clean %>%
  distinct(station_name, tag_serial_number, month, sex)%>%
  group_by(station_name, month, sex) %>%
  count() %>%
  rename(individuals_count = n) %>%
  arrange(desc(individuals_count))
# masterias_ind_per_station_season_sex %>% View()

masterias_station_month_sex <- masterias_ind_per_station_season_sex %>% left_join(detections_station_season_sex, by = c("station_name", "sex", "month"), multiple = "all") %>% left_join(masterias_detections_clean %>% group_by(station_name) %>% dplyr::select(station_name, deploy_latitude, deploy_longitude), by = "station_name", multiple = "all")

masterias_station_month_sex <- masterias_ind_per_station_season_sex %>% left_join(detections_station_season_sex, by = c("station_name", "sex", "month"), multiple = "all") %>% left_join(masterias_stations, by = "station_name", multiple = "all")
# masterias_station_month_sex %>% View()

masterias_station_month_sex <- masterias_station_month_sex %>% mutate(month_name = month.abb[month])
masterias_station_month_sex$month_name <- masterias_station_month_sex$month_name %>% factor(levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))

# summarise detections per sex
abs_detections <- masterias_station_month_sex %>% group_by(sex) %>% summarise(abs_detect = sum(detections_count))
abs_individuals <- masterias_detections_clean %>% group_by(sex) %>% summarise(abs_ind = tag_serial_number %>% unique() %>% length())
abs_detect_ind <- abs_detections %>% left_join(abs_individuals, by = "sex")


# find individuals per area per month
ind_area_month <- masterias_detections_clean %>% group_by(sex, area, month) %>% summarise(individuals_count = tag_serial_number %>% unique() %>% length())
ind_area_month <- ind_area_month %>% mutate(month_name = month.abb[month])
ind_area_month$month_name <- ind_area_month$month_name %>% factor(levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))

# 4. calc depth and temp from sensor val ####

tag_info_vemco <- readxl::read_xls(paste0(dir_path, "/00_data/acoustic_detections/TagSheet_25226_20180427.xls"), sheet = "Tag Summary") %>%
  rename(acoustic_tag_id = `VUE Tag ID`, sensor_intercept = Intercept, sensor_slope = Slope)

detection_sensor_info <- masterias_detections_clean %>% 
  dplyr::select(detection_id, date_time, tag_serial_number, acoustic_tag_id, station_name, deploy_latitude, deploy_longitude, sensor_value, sex) %>%
  left_join(tag_info_vemco %>%
              dplyr::select(acoustic_tag_id, sensor_intercept, sensor_slope),
            by = "acoustic_tag_id") %>%
  left_join(tag_info %>% dplyr::select(acoustic_tag_id, sensor_type, sensor_unit), 
            by = "acoustic_tag_id") %>%
  mutate(parameter = (sensor_value * sensor_slope) + sensor_intercept)

time_bins <- suncalc::getSunlightTimes(data = detection_sensor_info %>% 
                                         dplyr::select(date_time, deploy_latitude, deploy_longitude, tag_serial_number) %>% 
                                         mutate(date_time = date_time %>% as.Date()) %>%
                                         dplyr::distinct(date_time, deploy_latitude, deploy_longitude) %>%
                                         rename(date = date_time, lat = deploy_latitude, lon = deploy_longitude),
                                       keep = c("sunrise", "sunset", "dawn", "dusk")
                                       # add one day before and after time series, possible improvement: dont hard code lat, lon and date
                                       # tibble::add_row(.before = 1, date = "2018-07-18" %>% as.Date(), lat = 51.61220, lon = 3.64631481, tag_serial_number = "1293295") %>%
                                       # tibble::add_row(date = "2019-11-16" %>% as.Date(), lat = 52.59358, lon = 2.391523, tag_serial_number = "1293321"),
)

detections_tempdepth_daynight <- detection_sensor_info %>% 
  mutate(date = date_time %>% as.Date()) %>%
  left_join(time_bins, by = join_by(date, deploy_latitude == lat, deploy_longitude == lon)) %>%
  mutate(day = ifelse(date_time %>% between(sunrise, sunset), 1, 0),
         day = day %>% as.factor()) %>%
  dplyr::select(!c(dusk, dawn))

# 5. Remove Temporary files ####

rm(masterias_ind_per_station_season_sex, detections_station_season_sex,
   abs_detections, abs_individuals,
   # WS1_lon_threshold, WS2_lon_threshold,
   open_stations, # deployments,
   masterias_detections, detection_summary,
   abs_detect_ind,
   summary_masterias_length)

# 6. SAVE FILES ####

save_data(close_stations, data_path)
save_data(deployments, data_path)
save_data(ind_area_month, data_path)
save_data(masterias_detections_clean, data_path)
save_data(detections_tempdepth_daynight, data_path)
save_data(masterias_info, data_path)
save_data(masterias_station_month_sex, data_path)
save_data(masterias_stations, data_path)
save_data(tag_serial_number_omit, data_path)
