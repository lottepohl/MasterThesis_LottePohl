# Script to prepare the acoustic detections to serve as a dataset for the OpenSeaLab Hackathon

# 0. workspace ####
library(dplyr)
library(lubridate)
library(car)
library(readr)
# library(plotly)
# library(pracma)
# library(psdr)
# library(ggplot2)
# library(StreamMetabolism)
# library(suncalc)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path_acoustic <- "C:/Users/lotte.pohl/Documents/github_repos/OpenSeaLabHackathon_SmoothSharks/00_data/01_acoustic_detections/"
data_path_dst <- "C:/Users/lotte.pohl/Documents/github_repos/OpenSeaLabHackathon_SmoothSharks/00_data/02_shark308_datastoragetag/"
# source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_geolocation_output.R"))

# 1. prepare data ####

## sharks ####
sharks_info <- masterias_info %>% 
  filter(sex == "f", !tag_serial_number %in% tag_serial_number_omit, n_detect != 0) %>% 
  dplyr::select(tag_serial_number, scientific_name, sex,
                capture_date_time, capture_latitude, capture_longitude, capture_method, capture_depth,
                release_date_time, release_latitude, release_longitude, recapture_date_time, 
                length1, length1_unit, weight, weight_unit, life_stage, wild_or_hatchery,
                surgery_date_time, tagging_methodology, sedative, comments, n_detect
                )

sharks_detections <- masterias_detections_clean %>%
  filter(tag_serial_number %in% sharks_info$tag_serial_number,
         date_time %>% lubridate::year() == "2019") %>%
  dplyr::select(detection_id, date_time, tag_serial_number, receiver_id, station_name, 
                deploy_latitude, deploy_longitude, acoustic_tag_id, sensor_type, parameter, sensor_unit, day, sunrise, sunset
                )


shark_308_locations <- masterias_dst_geolocation_output %>% 
  filter(tag_serial_number == "1293308", date_time > "2019-05-12" %>% as.POSIXct(tz = "UTC")) %>%
  dplyr::select(!c(date, sex, life_stage, length1, weight)) %>%
  rename(latitude = detection_latitude, longitude = detection_longitude, latitude_mean = detection_latitude_mean, longitude_mean = detection_longitude_mean,
         latitude_mode = detection_latitude_mode, longitude_mode = detection_longitude_mode)

shark_308_temp_depth_log <- masterias_depth_temp %>% 
  filter(tag_serial_number == "1293308", date_time %>% lubridate::date() >= "2019-05-12" %>% as.POSIXct(tz = "UTC")) %>%
  dplyr::select(!t)
  
## acoustic receivers

receiver_stations_info <- close_stations %>% filter(area != "BPNS") %>% dplyr::select(!Array)

receiver_deployments <- deployments %>% 
  filter(station_name %in% receiver_stations_info$station_name,
         deploy_date_time < sharks_detections$date_time %>% max(),
         recover_date_time > sharks_detections$date_time %>% min()) %>%
  dplyr::select(deployment_id, receiver_id, station_name, deploy_date_time, deploy_latitude, deploy_longitude,
                mooring_type, recover_date_time)

  
# 2. save data ####

save_data(data = sharks_info, folder = data_path_acoustic)
readr::write_csv(x = sharks_info, file = paste0(data_path_acoustic, "sharks_info.csv"))

save_data(data = sharks_detections, folder = data_path_acoustic)
readr::write_csv(x = sharks_detections, file = paste0(data_path_acoustic, "sharks_detections.csv"))

save_data(data = receiver_stations_info, folder = data_path_acoustic)
readr::write_csv(x = receiver_stations_info, file = paste0(data_path_acoustic, "receiver_stations_info.csv"))

save_data(data = receiver_deployments, folder = data_path_acoustic)
readr::write_csv(x = receiver_deployments, file = paste0(data_path_acoustic, "receiver_deployments.csv"))

save_data(data = shark_308_locations, folder = data_path_dst)
readr::write_csv(x = shark_308_locations, file = paste0(data_path_dst, "shark_308_locations.csv"))

save_data(data = shark_308_temp_depth_log, folder = data_path_dst)
readr::write_csv(x = shark_308_temp_depth_log, file = paste0(data_path_dst, "shark_308_temp_depth_log.csv"))
