# Script to make tables for the thesis manuscript

# workspace ####

library(dplyr)
library(tidyverse)
library(leaflet)
library(ggplot2)
library(tibble)

rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" 

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()


# table 1: list of abbreviations ####

abbrev <- tibble::tibble(Abbreviation = c("ADST", "DST", "DVM"),
                         Explanation= c("Acoustic Data Storage Tag",
                                        "Data Storage Tag",
                                        "Diel Vertical Migration"))

# table 2: release locations ####

release_locations <- masterias_info %>% mutate(date = release_date_time %>% lubridate::date(),
                                          release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Westerschelde")) %>% 
  group_by(release_loc) %>% reframe(lat = release_latitude %>% median(),
                                    lng = release_longitude %>% median()) #%>%
  # mutate(label = c(1,2))

# table 3: tagged animal summary ####

acoustic_days_liberty <- detections_tempdepth_daynight %>% group_by(tag_serial_number) %>%
  summarise(release_date = date_time %>% lubridate::date() %>% min(),
            date_last_detected = date_time %>% lubridate::date() %>% max(),
            days_at_liberty = base::difftime(date_last_detected, release_date, tz = "UTC", units = "days") %>% as.numeric())

tagged_animal_info <- masterias_info %>% 
  dplyr::select(tag_serial_number, sex, length1, release_date_time, n_detect) %>% #, capture_method
  mutate(release_date_time = lubridate::date(release_date_time),
         n_detect = ifelse(n_detect %>% is.na(), 0, n_detect))

tagged_animal_info %>% View()



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
              dplyr::select(tag_serial_number, sex), 
            by = "tag_serial_number") %>%
  mutate(release_date = release_date %>% as.Date(),
         death_date = death_date %>% as.Date())
# include max and min depth, and max and min temp?


# save tables #####
save_data(data = dst_summary, folder = paste0(dir_path, "/02_results/dst_summary/"))

