# script to plot and visualise the acoustic detection data with the corrected temp and depth values

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

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/02_scripts/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_dst_summarystatistics.R"))

# test <- masterias_detections_clean %>% dplyr::select(!sensor_unit) %>% left_join(masterias_detections_clean %>% dplyr::select(!c(date_time, tag_serial_number, acoustic_tag_id, station_name, deploy_latitude, deploy_longitude, sensor_value, sex)), 
#                                                  by = join_by(detection_id))
# test <- masterias_detections_clean %>% full_join(masterias_detections_clean)


p_acoustic_depth <- ggplot() +
  geom_point(data = masterias_detections_clean %>% filter(sensor_type == "pressure", area == "WS1"), aes(x = date, y = -parameter),
             colour = "black", size = 1.5) +
  geom_point(data = masterias_detections_clean %>% filter(sensor_type == "pressure"), 
             aes(x = date, y = -parameter, colour = tag_serial_number),
             size = 1) +
  labs(y = "depth in m", x = "detection date and time") +
  theme_minimal() #%>%
p_acoustic_depth %>%  plotly::ggplotly()


p308_summer_dst_acoustic <- ggplot() +
 geom_point(data = masterias_depth_temp_summary %>% filter(tag_serial_number == "1293308",
                                                           date %>% as.Date() %>% between("2019-05-01" %>% as.Date(),"2019-10-10" %>% as.Date()),
                                                           row_number() %% 5 == 0),
            aes(x = date_time, y = -depth_m), alpha = 0.35) + #, colour = day
  labs(y = "depth in m", x = "detection date and time") +
  geom_point(data = masterias_detections_clean %>% filter(sensor_type == "pressure", tag_serial_number == "1293308", date_time %>% lubridate::year() == 2019), 
             aes(x = date_time, y = -parameter, colour = day)) +
  theme_minimal() #%>%
p308_summer_dst_acoustic %>%  plotly::ggplotly()

test <- masterias_depth_temp_summary %>% filter(tag_serial_number == "1293308",
                                                 date %>% as.Date() %>% between("2019-05-01" %>% as.Date(),"2019-10-10" %>% as.Date()),
                                                row_number() %% 5 == 0) #%>%
  # dplyr::slice_sample(n = 10000)

test$date %>% class()
