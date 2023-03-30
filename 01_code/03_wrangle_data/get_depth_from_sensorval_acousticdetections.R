# script to calculate depth from the acoustic detections
# 0. workspace ####
library(dplyr)
library(lubridate)
library(car)
library(vegan)
library(ggpubr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(ggplot2)
# library(MASS)
library(plotly)
library(readxl)
library(suncalc)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# plot_path <- paste0(dir_path, "/04_analysis_results/dst_pca_kmeans/")
# source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))

tag_info_vemco <- read_xls(paste0(dir_path, "/00_data/acoustic_detections/TagSheet_25226_20180427.xls"), sheet = "Tag Summary") %>%
  rename(acoustic_tag_id = `VUE Tag ID`, sensor_intercept = Intercept, sensor_slope = Slope)

detection_sensor_info <- masterias_detections_clean %>%
  dplyr::select(!c(sensor_unit, sensor2_value, sensor2_unit, signal_to_noise_ratio, qc_flag, sensor_intercept, sensor_slope, sensor_type)) %>%
  # dplyr::select(detection_id, receiver_id, date_time, tag_serial_number, area, month, acoustic_tag_id, station_name, deploy_latitude, deploy_longitude, sensor_value, sex) %>%
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

rm(detections_tempdepth_daynight)

detections_tempdepth_daynight <- detection_sensor_info %>% 
  mutate(date = date_time %>% as.Date()) %>%
  left_join(time_bins, by = join_by(date, deploy_latitude == lat, deploy_longitude == lon, sunrise, sunset)) %>%
  mutate(day = ifelse(date_time %>% between(sunrise, sunset), 1, 0),
         day = day %>% as.factor()) %>%
  dplyr::select(!c(dusk, dawn))

# p <- ggplot(data = detections_tempdepth_daynight %>% filter(sensor_type == "pressure"), aes(x = date, y = -parameter, colour = tag_serial_number)) +
#   geom_point() +
#   labs(y = "depth in m", x = "detection date and time") +
#   theme_minimal() #%>%
# p %>%  plotly::ggplotly()
  # facet_grid(vars(tag_serial_number), scales = "free") #%>%
  # plotly::ggplotly()

# save data ####

save_data(data = detections_tempdepth_daynight, folder = paste0(dir_path, "/00_data/acoustic_detections/"))
