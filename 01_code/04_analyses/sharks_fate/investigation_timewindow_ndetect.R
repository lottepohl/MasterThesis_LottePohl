
# see notebook, 18.3.23
library(dplyr)
library(tidyverse)
# library(lubridate)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))

masterias_info2 <- masterias_info %>% 
  dplyr::select(tag_serial_number, capture_date_time, capture_location, release_date_time, length1, sex, surgery_date_time, n_detect) %>%
  mutate(time_window = difftime(release_date_time, capture_date_time, units = "mins") %>% as.numeric(),
         time_window = ifelse(time_window > 60, 30, time_window),
         n_detect = n_detect %>% tidyr::replace_na(0))

masterias_info2 %>% filter(tag_serial_number %in% c("1293295", "123319", "1293322", "1293304", "1293310", "1293312")) %>% View()

# masterias_info$capture_date_time[1] - lubridate::minutes(30)
ggplot(data = masterias_info2) + geom_point(aes(x = time_window, y = n_detect))
