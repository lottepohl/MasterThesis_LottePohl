# Script to explore the behaviour of M. asterias females that were acoustically detected in the Westerschelde
# Author: Lotte Pohl

rm(list = ls())

library(tidyverse)
library(lubridate)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/02_scripts/03_wrangle_data/wrangle_acoustic_data.R"))
source(paste0(dir_path, "/02_scripts/05_plots_maps/base_map.R"))

# Functions ####
make_detection_details <- function(tag_serial_num_chr, years){
  detection_details <- masterias_detections_clean %>% 
    filter(tag_serial_number == tag_serial_num_chr,
           paste0("20", detection_year) %in% years) %>% 
    group_by(station_name, month) %>% 
    summarise(first_detect = date_time %>% min(), last_detect = date_time %>% max(),
              detection_date = first_detect %>% lubridate::date() %>% as.POSIXct(),
              detection_period_h = difftime(last_detect, first_detect, units = "hours") %>% as.numeric() %>% round(digits = 2),
              # detection_period_h = as.numeric(last_detect - first_detect) %>% round(digits = 2),
              latitude = mean(deploy_latitude) + runif(1, min = -0.003, max = 0.003),
              longitude = mean(deploy_longitude) + runif(1, min = -0.003, max = 0.003)) %>%
    arrange(detection_date)
  return(detection_details)
}

make_WS_summer_map <- function(detection_details, legend_dates){
  
  pal <- colorNumeric(palette = "inferno", domain = detection_details$detection_date)
  
  map_acoustic <- base_map %>%
    setView(lng = 3.34, lat = 51.44, zoom = 11) %>%
    ### add tag #####
  addPolylines(data = detection_details,
               lat = ~latitude,
               lng = ~longitude,
               color = "grey",
               weight = 3,
               opacity = 0.6,
               # label = ~tag_serial_number,
               group = "tag") %>%
    addCircleMarkers(data = detection_details,
                     lat = ~latitude,
                     lng = ~longitude,
                     fillColor = ~pal(detection_date),
                     radius = 5,
                     weight = 0,
                     fillOpacity = 0.8,
                     label = ~paste0(station_name, ", ", format(first_detect,"%F"), " (", detection_period_h, " h)"),
                     group = "tag") %>%
    hideGroup(c("areas", "wrecks, OWFs, cables")) %>%
    addLegend(position = "topleft",
              colors = legend_dates %>% as.POSIXct() %>% pal(),
              labels = legend_dates, opacity = 1,
              title = "Dates")
  map_acoustic %>% return()
  
}

# TAG 308 ####
detection_details_308 <- make_detection_details("1293308", "2019")
legend_dates_308 = c("2019-05-10", "2019-06-01", "2019-07-01", "2019-08-01", "2019-09-01", "2019-10-01")
map_acoustic_308 <- make_WS_summer_map(detection_details_308, legend_dates = legend_dates_308)
map_acoustic_308

# TAG 297 GOOD DATA ####
detection_details_297_2018 <- make_detection_details("1293297", "2018")
detection_details_297_2018 %>% View()
legend_dates_297_2018 = c("2018-08-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-10-01", "2018-10-25")
map_acoustic_297_2018 <- make_WS_summer_map(detection_details_297_2018, legend_dates = legend_dates_297_2018)
map_acoustic_297_2018

detection_details_297_2019 <- make_detection_details("1293297", "2019")
# detection_details_297_2019 %>% View()
legend_dates_297_2019 = c("2019-04-26", "2019-05-25", "2019-06-25", "2019-07-25", "2019-09-01", "2019-09-07", "2019-09-25")
map_acoustic_297_2019 <- make_WS_summer_map(detection_details_297_2019, legend_dates = legend_dates_297_2019)
map_acoustic_297_2019

# TAG 299 GOOD DATA ####

detection_details_299_2018 <- make_detection_details("1293299", "2018")
# detection_details_299_2018 %>% View()
legend_dates_299_2018 = c("2018-08-05", "2018-08-15", "2018-09-01", "2018-09-15", "2018-09-25")
map_acoustic_299_2018 <- make_WS_summer_map(detection_details_299_2018, legend_dates = legend_dates_299_2018)
map_acoustic_299_2018

detection_details_299_2019 <- make_detection_details("1293299", "2019")
# detection_details_299_2019 %>% View()
legend_dates_299_2019 = c("2019-04-26", "2019-05-25", "2019-06-25", "2019-07-25", "2019-08-25", "2019-09-07", "2019-09-25", "2019-10-30")
map_acoustic_299_2019 <- make_WS_summer_map(detection_details_299_2019, legend_dates = legend_dates_299_2019)
map_acoustic_299_2019

# TAG 298 not much data ####
detection_details_298 <- make_detection_details("1293298", c("2018", "2019"))
legend_dates_298 = c("2018-08-03", "2018-09-01", "2018-09-15", "2019-05-11", "2019-06-26")
map_acoustic_298 <- make_WS_summer_map(detection_details_298, legend_dates = legend_dates_298)
map_acoustic_298

# TAG 302 not much data ####
detection_details_302 <- make_detection_details("1293302", c("2018", "2019"))
# detection_details_302 %>% View()
legend_dates_302 = c("2018-08-04", "2018-08-11", "2019-04-30", "2019-08-23")
map_acoustic_302 <- make_WS_summer_map(detection_details_302, legend_dates = legend_dates_302)
map_acoustic_302

# TAG 303 GOOD DATA ####
detection_details_303 <- make_detection_details("1293303", "2020")
detection_details_303 %>% View()
legend_dates_303 = c("2020-04-25", "2020-05-15", "2020-06-01", "2020-06-09")
map_acoustic_303 <- make_WS_summer_map(detection_details_303, legend_dates = legend_dates_303)
map_acoustic_303

# detection_details_303_2018 <- make_detection_details("1293303", "2018")
# detection_details_303_2018 %>% View()
# legend_dates_303_2018 = c("2018-08-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-09-25")
# map_acoustic_303_2018 <- make_WS_summer_map(detection_details_303_2018, legend_dates = legend_dates_303_2018)
# map_acoustic_303_2018
# 
# detection_details_303_2019 <- make_detection_details("1293303", "2019")
# legend_dates_303_2019 = c("2019-08-24", "2019-09-07", "2019-09-22")
# map_acoustic_303_2019 <- make_WS_summer_map(detection_details_303_2019, legend_dates = legend_dates_303_2019)
# map_acoustic_303_2019

# TAG 300 GOOD DATA ####
detection_details_300 <- make_detection_details("1293300", "2018")
# detection_details_300 %>% View()
legend_dates_300 = c("2018-08-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-10-01", "2018-10-15", "2018-10-26")
map_acoustic_300 <- make_WS_summer_map(detection_details_300, legend_dates = legend_dates_300)
map_acoustic_300

# TAG 307 GOOD DATA ####
detection_details_307_2018 <- make_detection_details("1293307", "2018")
detection_details_307_2018 %>% View()
legend_dates_307_2018 = c("2018-08-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-09-25")
map_acoustic_307_2018 <- make_WS_summer_map(detection_details_307_2018, legend_dates = legend_dates_307_2018)
map_acoustic_307_2018

detection_details_307_2019 <- make_detection_details("1293307", "2019")
legend_dates_307_2019 = c("2019-08-24", "2019-09-07", "2019-09-22")
map_acoustic_307_2019 <- make_WS_summer_map(detection_details_307_2019, legend_dates = legend_dates_307_2019)
map_acoustic_307_2019


# TAG 313 somewhat GOOD DATA ####
# detection_details_313 <- make_detection_details("1293313")
# # detection_details_313 %>% View()
# legend_dates_313 = c("2019-07-19", "2019-08-15", "2019-10-01", "2020-05-28", "2020-07-05")
# map_acoustic_313 <- make_WS_summer_map(tag_serial_num_chr = "1293313", legend_dates = legend_dates_313)
# map_acoustic_313

detection_details_313_2019 <- make_detection_details("1293313", "2019")
# detection_details_313 %>% View()
legend_dates_313_2019 = c("2019-07-20", "2019-08-15", "2019-09-01", "2019-09-15", "2019-10-01")
map_acoustic_313_2019 <- make_WS_summer_map(detection_details_313_2019, legend_dates = legend_dates_313_2019)
map_acoustic_313_2019

detection_details_313_2020 <- make_detection_details("1293313", "2020")
# detection_details_313_2020 %>% View()
legend_dates_313_2020 = c("2020-05-27", "2020-06-15", "2020-07-05")
map_acoustic_313_2020 <- make_WS_summer_map(detection_details_313_2020, legend_dates = legend_dates_313_2020)
map_acoustic_313_2020


# TAG 318 not much data ####

detection_details_318 <- make_detection_details("1293318", "2019")
# detection_details_318 %>% View()
legend_dates_318 = c("2019-06-17", "2019-07-02", "2019-09-24")
map_acoustic_318 <- make_WS_summer_map(detection_details_318, legend_dates = legend_dates_318)
map_acoustic_318

# TAG 314 ALMOST NO DATA ####
detection_details_314 <- make_detection_details("1293314", "2018")
detection_details_314 %>% View()
legend_dates_314 = c("2018-07-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-09-25")
map_acoustic_314 <- make_WS_summer_map(detection_details_314, legend_dates = legend_dates_314)
map_acoustic_314

# TAG 317 ALMOST NO DATA ####
detection_details_317 <- make_detection_details("1293317", c("2018", "2019"))
detection_details_317 %>% View()
legend_dates_317 = c("2018-07-04", "2018-08-15", "2018-09-01", "2018-09-15", "2018-09-25")
map_acoustic_317 <- make_WS_summer_map(detection_details_317, legend_dates = legend_dates_317)
map_acoustic_317

# TAG 319 ALMOST NO DATA ####
detection_details_319 <- make_detection_details("1293319", c("2018", "2019"))
detection_details_319 %>% View()

# TAG 309 ALMOST NO DATA ####
detection_details_309 <- make_detection_details("1293309", c("2018", "2019"))
detection_details_309 %>% View()
