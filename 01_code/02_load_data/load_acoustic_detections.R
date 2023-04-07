# script to load the acoustic, wrangled data into the workspace

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path <- paste0("C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/00_data/acoustic_detections/")

# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()
# source(paste0(dir_path, "/02_scripts/03_wrangle_data/wrangle_acoustic_data.R"))
# source(paste0(dir_path, "/02_scripts/03_wrangle_data/get_depth_from_sensorval_acousticdetections.R"))


masterias_station_month_sex <- load_data("masterias_station_month_sex", data_path)
close_stations <- load_data("close_stations", data_path)
deployments <- load_data("deployments", data_path)
ind_area_month <- load_data("ind_area_month", data_path)
detections_tempdepth_daynight <- load_data("detections_tempdepth_daynight", data_path)
masterias_info <- load_data("masterias_info", data_path)
masterias_station_month_sex <- load_data("masterias_station_month_sex", data_path)
masterias_stations <- load_data("masterias_stations", data_path)
tag_serial_number_omit <- load_data("tag_serial_number_omit", data_path)
tag_info <- load_data("tag_info", data_path)

masterias_detections_clean <- load_data("masterias_detections_clean", data_path)
detections_tempdepth_daynight <- load_data("detections_tempdepth_daynight", data_path)

masterias_detections_clean <- masterias_detections_clean %>% dplyr::select(!sensor_unit) %>% left_join(detections_tempdepth_daynight %>% dplyr::select(!c(date_time, tag_serial_number, acoustic_tag_id, station_name, deploy_latitude, deploy_longitude, sensor_value, sex)), 
                                                                         by = join_by(detection_id))
