# Script to make metadata info for the thesis manuscript

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(tidyr)
library(utils)
library(scales)
library(gridExtra)
library(pracma)
library(oce)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
# paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_dst_geolocation_output.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_models.R") %>% base::source()

## set path were all figures are saved ####
data_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/raw_metadata2/")


# 1. acoustic detections ####

## raw data ####
acoustic_detections_raw_data <- detections_tempdepth_daynight %>% 
  dplyr::select(detection_id, date_time, tag_serial_number, scientific_name, station_name, deploy_latitude, deploy_longitude, parameter, sensor_unit, sensor_type, acoustic_tag_id)


save_data(data = acoustic_detections_raw_data, folder = data_path)
utils::write.csv(x = acoustic_detections_raw_data, file = paste0(data_path, "acoustic_detections_raw_data.csv"), row.names = F)


## metadata ####

acoustic_detections_metadata <- tibble(variable = c("detection\\_id", "date\\_time", "tag\\_serial\\_number", "scientific\\_name", "station\\_name", "deploy\\_latitude", "deploy\\_longitude", "parameter",
                                                    "sensor\\_unit", "sensor\\_type", "acoustic\\_tag\\_id"),
                                       explanation = c("Unique identifier of the detection event.", "Date and time in YYYY-MM-DD HH:MM:SS format. In UTC time.", 
                                                       "The serial number that is unique to the Acoustic Data Storage Tag.",
                                                       "Scientific name of the animal that carries the tag.",
                                                       "Name of the station where the deployment of the receiver takes place. Related to a specific latitude and longitude.",
                                                       "Latitude of the actual deployment location, in decimal degrees. Note: in the southern hemisphere all latitudes must be negative.",
                                                       "Longitude of the actual deployment location, in decimal degrees. Note: in the western hemisphere all longitudes must be negative.",
                                                       "Value of one sensor transmitted to the acoustic receiver at the time of detection.",
                                                       "Unit of the sensor at stake.",
                                                       "Type of tag sensor. Predefined options: pressure, temperature, acceleration.",
                                                       "Unique identifier of each sensor within the acoustic tag. One ID for the pressure sensor, and one ID for the temperature sensor per ADST."))

# acoustic_detections_metadata <- 
#   base::data.frame(variable = colnames(acoustic_detections_raw_data),
#                                                 explanation = NA) %>%
#   tidyr::pivot_wider(names_from = variable, values_from = explanation) %>%
#   dplyr::mutate(detection_id = "Unique identifier of the detection event.",
#                 date_time = "Date and time in YYYY-MM-DD HH:MM:SS format. In UTC time.",
#                 tag_serial_number = "The serial number that is unique to the Acoustic Data Storage Tag.",
#                 scientific_name = "Scientific name of the animal that carries the tag.",
#                 station_name = "Name of the station where the deployment of the receiver takes place, Related to a specific latitude and longitude.",
#                 deploy_latitude = "Latitude of the actual deployment location, in decimal degrees. Note: in the southern hemisphere all latitudes must be negative.",
#                 deploy_longitude = "Longitude of the actual deployment location, in decimal degrees. Note: in the western hemisphere all longitudes must be negative.",
#                 parameter = "Value of one sensor transmitted to the acoustic receiver at the time of detection.",
#                 sensor_unit = "Unit of the sensor at stake.",
#                 sensor_type = "Type of tag sensor. Predefined options: pressure, temperature, acceleration.",
#                 acoustic_tag_id = "Unique identifier of each sensor within the acoustic tag. One ID for the pressure sensor, and one ID for the temperature sensor per ADST.") %>%
#   base::t() %>% as.data.frame() %>% dplyr::rename(explanation = V1) %>%
#   dplyr::mutate(variable = row.names(.),
#                 variable = gsub("_", "\\_", variable)) %>% 
#   as.tibble()

save_data(data = acoustic_detections_metadata, folder = data_path)
utils::write.csv(x = acoustic_detections_metadata, file = paste0(data_path, "acoustic_detections_metadata.csv"), row.names = T)

# 2. raw depth and temp log ####
## raw data ####
DST_logs_raw_data <- masterias_depth_temp %>% dplyr::select(!t)

save_data(data = DST_logs_raw_data, folder = data_path)
utils::write.csv(x = DST_logs_raw_data, file = paste0(data_path, "DST_logs_raw_data.csv"), row.names = F)

## metadata ####

DST_logs_metadata <- tibble(variable = c("date\\_time", "tag\\_serial\\_number", "depth\\_m", "temp\\_c"),
                                       explanation = c("Date and time in YYYY-MM-DD HH:MM:SS format. In UTC time.",
                                                       "The serial number that is unique to the Acoustic Data Storage Tag.",
                                                       "The recorded depth of the depth sensor in Metres.",
                                                       "The recorded temperature of the temperature sensor in degrees Celcius."))


# DST_logs_metadata <- base::data.frame(variable = colnames(DST_logs_raw_data),
#                                                  explanation = NA) %>%
#   tidyr::pivot_wider(names_from = variable, values_from = explanation) %>%
#   dplyr::mutate(date_time = "Date and time in YYYY-MM-DD HH:MM:SS format. In UTC time.",
#                 tag_serial_number = "The serial number that is unique to the Acoustic Data Storage Tag.",
#                 depth_m = "The recorded depth of the depth sensor in Metres.",
#                 temp_c= "The recorded temperature of the temperature sensor in degrees Celcius.") %>%
#   base::t() %>% as.data.frame() %>% dplyr::rename(explanation = V1) %>%
#   dplyr::mutate(variable = row.names(.)) %>% as.tibble()

save_data(data = DST_logs_metadata, folder = data_path)
utils::write.csv(x = DST_logs_metadata, file = paste0(data_path, "DST_logs_metadata.csv"), row.names = T)


# 3. geolocation output ####
## raw data ####

geolocation_output_raw_data <- masterias_dst_geolocation_output %>% 
  dplyr::select(tag_serial_number, date_time, detection_latitude, detection_longitude)

save_data(data = geolocation_output_raw_data, folder = data_path)
utils::write.csv(x = geolocation_output_raw_data, file = paste0(data_path, "geolocation_output_raw_data.csv"), row.names = F)

## metadata ####

geolocation_output_metadata <- tibble(variable = c("date\\_time", "tag\\_serial\\_number", "detection\\_latitude", "detection\\_longitude"),
                                       explanation = c("Date and time in YYYY-MM-DD format. In UTC time.",
                                                       "The serial number that is unique to the Acoustic Data Storage Tag.",
                                                       "Latitude that was calculated as the 'most probable track' in the geolocation model. See Goossens et al., (2023) and Woillez et al., (2016) for details.",
                                                       "Longitude that was calculated as the 'most probable track' in the geolocation model. See Goossens et al., (2023) and Woillez et al., (2016) for details."))


# geolocation_output_metadata <- base::data.frame(variable = colnames(geolocation_output_raw_data),
#                                       explanation = NA) %>%
#   tidyr::pivot_wider(names_from = variable, values_from = explanation) %>%
#   dplyr::mutate(date_time = "Date and time in YYYY-MM-DD format. In UTC time.",
#                 tag_serial_number = "The serial number that is unique to the Acoustic Data Storage Tag.",
#                 detection_latitude = "Latitude that was calculated as the 'most probable track' in the geolocation model. See Goossens et al., (2023) and Woillez et al., (2016) for details.",
#                 detection_longitude = "Longitude that was calculated as the 'most probable track' in the geolocation model. See Goossens et al., (2023) and Woillez et al., (2016) for details.") %>%
#   base::t() %>% as.data.frame() %>% dplyr::rename(explanation = V1) %>%
#   dplyr::mutate(variable = row.names(.)) %>% as.tibble()

save_data(data = geolocation_output_metadata, folder = data_path)
utils::write.csv(x = geolocation_output_metadata, file = paste0(data_path, "geolocation_output_metadata.csv"), row.names = T)

# 4. tagged animal info ####
## raw data ####
tagged_individuals_raw_data <- masterias_info %>% dplyr::select(tag_serial_number, sex, length1, weight, release_date_time, release_latitude, release_longitude, recapture_date_time) %>%
  dplyr::mutate(length1 = length1 / 100)

save_data(data = tagged_individuals_raw_data, folder = data_path)
utils::write.csv(x = tagged_individuals_raw_data, file = paste0(data_path, "tagged_individuals_raw_data.csv"), row.names = F)  

## metadata ####

tagged_individuals_metadata <- tibble(variable = c("release\\_date\\_time", "tag\\_serial\\_number","release\\_latitude", "release\\_longitude", "sex", "weight", "length1", "recapture\\_date\\_time"),
                                       explanation = c("Date and time of the release of the animal, in YYYY-MM-DD HH:MM:SS format and UTC time.",
                                                       "The serial number that is unique to the Acoustic Data Storage Tag.",
                                                       "Longitude of release location, in decimal degress. Note: in the western hemisphere all longitudes must be negative.",
                                                       "Longitude of release location, in decimal degress. Note: in the western hemisphere all longitudes must be negative.",
                                                       "Sex of the animal. f = female, m = male.",
                                                       "Bodymass of the animal carrying the tag. In Kilogram.",
                                                       "Total length of animal carrying the tag. In Metres.",
                                                       "If applicable, date of the recapture of the tagged animal. In YYYY-MM-DD format, in UTC time."))


# tagged_individuals_metadata <- base::data.frame(variable = colnames(tagged_individuals_raw_data),
#                                                 explanation = NA) %>%
#   tidyr::pivot_wider(names_from = variable, values_from = explanation) %>%
#   dplyr::mutate(release_date_time = "Date and time of the release of the animal, in YYYY-MM-DD HH:MM:SS format and UTC time.",
#                 tag_serial_number = "The serial number that is unique to the Acoustic Data Storage Tag.",deploy_latitude = "Latitude of the actual deployment location, in decimal degress. Note: in the southern hemisphere all latitudes must be negative.",
#                 release_longitude = "Longitude of release location, in decimal degress. Note: in the western hemisphere all longitudes must be negative.",
#                 release_latitude = "Latitude of release location, in decimal degress. Note: in the western hemisphere all longitudes must be negative.",
#                 sex = "Sex of the animal. f = female, m = male.",
#                 weight = "Bodymass of the animal carrying the tag. In Kilogram.",
#                 length1 = "Total length of animal carrying the tag. In Metres.",
#                 recapture_date_time = "If applicable, date of the recapture of the tagged animal. In YYYY-MM-DD format, in UTC time.") %>%
#   base::t() %>% as.data.frame() %>% dplyr::rename(explanation = V1) %>%
#   dplyr::mutate(variable = row.names(.)) %>% as.tibble()

save_data(data = tagged_individuals_metadata, folder = data_path)
utils::write.csv(x = tagged_individuals_metadata, file = paste0(data_path, "tagged_individuals_metadata.csv"), row.names = T)
  
  