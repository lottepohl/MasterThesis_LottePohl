# functions for the analysis of the DST depth and temp logs

# libraries
library(tibble)
library(lubridate)
library(tidyverse)


get_release_time <- function(tag_serial_num){
  release_time <- masterias_info %>% filter(tag_serial_number == tag_serial_num %>% as.character()) %>% dplyr::select(release_date_time) %>% pull()
  return(release_time)
}

get_stop_time <- function(tag_serial_num, dst_output){
  stop_time <- dst_output %>% filter(tag_serial_number == tag_serial_num) %>% dplyr::select(date_time) %>% pull() %>% max()
  return(stop_time)
}

# cut off depth log 10 days after the DST modelling stopped
clean_presslog <- function(presslog, release_date_time, stop_time, cutoff_days){ 
  presslog$date_time <- presslog$...4 %>% as.POSIXct(format = "%m/%d/%Y %H:%M:%S", tz = "UTC") %>% lubridate::floor_date(unit = "minutes")
  presslog_clean <- presslog %>% filter(date_time >= release_date_time & date_time <= stop_time + lubridate::days(cutoff_days)) %>% dplyr::select(date_time, ...6) %>% rename(depth_m = ...6)
  presslog_clean$depth_m <- presslog_clean$depth_m %>% as.numeric()
  return(presslog_clean)
}

# cut off depth log 10 days after the DST modelling stopped
clean_templog <- function(templog, release_date_time, stop_time, cutoff_days){
  templog$date_time <- templog$...4 %>% as.POSIXct(format = "%m/%d/%Y %H:%M:%S", tz = "UTC") %>% lubridate::ceiling_date(unit = "minutes")
  templog_clean <- templog %>% filter(date_time >= release_date_time & date_time <= stop_time + lubridate::days(cutoff_days)) %>% dplyr::select(date_time, ...6) %>% rename(temp_c = ...6)
  templog_clean$temp_c <- templog_clean$temp_c %>% as.numeric()
  return(templog_clean)
}

merge_press_temp <- function(presslog_clean, templog_clean){
  press_temp <- presslog_clean %>% left_join(templog_clean, by = "date_time") %>% fill(temp_c)
  return(press_temp)
}

create_press_temp <- function(presslog, templog, tag_serial_number, cutoff_days, dst_output){
  release_time <- get_release_time(tag_serial_number)
  stop_time <- get_stop_time(tag_serial_number, dst_output)
  presslog_clean <- clean_presslog(presslog, release_time, stop_time, cutoff_days)
  templog_clean <- clean_templog(templog, release_time, stop_time, cutoff_days)
  press_temp <- merge_press_temp(presslog_clean, templog_clean)
  return(press_temp)
}
