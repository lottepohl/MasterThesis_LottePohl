# Script to make tables for the thesis manuscript

# workspace ####

library(dplyr)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(tibble)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" 
tables_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/tables/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()


# table 1: list of abbreviations ####

abbreviations_list <- tibble::tibble(Abbreviation = c("ADST", "DST", "DVM"),
                         Explanation= c("Acoustic Data Storage Tag",
                                        "Data Storage Tag",
                                        "Diel Vertical Migration"))

# table 2: release locations ####

release_locations <- masterias_info %>% mutate(date = release_date_time %>% lubridate::date(),
                                          release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Western Scheldt")) %>% 
  group_by(release_loc) %>% reframe(lat = release_latitude %>% mean(),
                                    lng = release_longitude %>% mean()) #%>%
  # mutate(label = c(1,2))

# table 3: tagged animal summary ####

acoustic_days_liberty <- detections_tempdepth_daynight %>% group_by(tag_serial_number) %>%
  summarise(days_detected = date_time %>% lubridate::date() %>% unique() %>% length(),
            hours_detected = paste0(date_time %>% lubridate::date(), '-', date_time %>% lubridate::hour()) %>% unique() %>% length(),
            date_last_detected = date_time %>% lubridate::date() %>% max())

tagged_animal_info <- masterias_info %>% 
  mutate(release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Western Scheldt")) %>%
  dplyr::select(tag_serial_number, sex, length1, release_date_time, n_detect, release_loc) %>% #, capture_method
  mutate(release_date_time = lubridate::date(release_date_time)) %>%
  left_join(acoustic_days_liberty, by = "tag_serial_number") %>%
  mutate(days_at_liberty = base::difftime(date_last_detected, release_date_time, tz = "UTC", units = "days") %>% as.numeric()) %>%
  tidyr::replace_na(replace = list(n_detect = 0,
                                   days_detected = 0,
                                   hours_detected = 0,
                                   date_last_detected = NA,
                                   days_at_liberty = 0)) %>% #look for solution to replace all columns but one with NA
  relocate(release_loc, .after = days_at_liberty)

  
# tagged_animal_info %>% View()


# include capture method y or n?

# table 5: dst summary ####

dst_summary <- masterias_depth_date %>% 
  # mutate(tag_serial_number = tag_serial_number %>% as.numeric()) %>%
  dplyr::group_by(tag_serial_number) %>%
  summarise(release_date = min(date) - lubridate::days(8),
            death_date = date %>% max(),
            time_at_liberty = base::difftime(release_date, death_date, units = "days")%>% abs() %>% as.numeric()) %>%
  arrange(time_at_liberty) %>%
  left_join(masterias_info %>%
              dplyr::select(tag_serial_number, sex, recapture_date_time), 
            by = "tag_serial_number") %>%
  mutate(release_date = release_date %>% as.Date(),
         death_date = death_date %>% as.Date(),
         recapture_date_time = recapture_date_time %>% as.Date())
# include max and min depth, and max and min temp?


# save tables #####
save_data(data = dst_summary, folder = tables_path)
save_data(data = tagged_animal_info, folder = tables_path)
save_data(data = release_locations, folder = tables_path)
save_data(data = abbreviations_list, folder = tables_path)

