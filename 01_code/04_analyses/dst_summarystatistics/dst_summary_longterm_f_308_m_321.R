# Script to calculate summary statistics for the two longterm dst datasets per period

# make dataframe with year(2018/2019), season(summer/winter), daytime(day/night/NA), parameter(depth_range/max_horizontal_distance/...), value 

# 0. workspace ####
library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(psdr)
library(ggplot2)
library(StreamMetabolism)
library(suncalc)

rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/04_analysis_results/dst_summary/")
# source(paste0(dir_path, "/02_scripts/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_dst_summarystatistics.R"))

# NOT NEEDED RIGHT NOW, MAYBE DELETE

masterias_long_dst_date <- masterias_depth_date %>% 
  filter(tag_serial_number %in% c("1293321", "1293308")) #%>% #filter out dusk and dawn: not used right now


masterias_depth_daynight <- masterias_depth_temp_summary %>% group_by(date, tag_serial_number, day, dusk, dawn) %>%
  summarise(depth_mean = mean(depth_m, na.rm = T), 
            depth_sd = sd(depth_m, na.rm = T),
            depth_median = median(depth_m),
            depth_min = min(depth_m, na.rm = T),
            depth_max = max(depth_m, na.rm = T),
            depth_range = depth_max - depth_min,
            vertical_speed_min = min(vertical_speed_m_min, na.rm = T),
            vertical_speed_max = max(vertical_speed_m_min, na.rm = T),
            vertical_speed_range = vertical_speed_max - vertical_speed_min,
            vertical_speed_mean = mean(vertical_speed_m_min, na.rm = T),
            vertical_speed_sd = sd(vertical_speed_m_min, na.rm = T),
            vertical_speed_median = median(vertical_speed_m_min, na.rm = T)) %>%
  mutate(day = day %>% as.factor(),
         dusk = dusk %>% as.factor(),
         dawn = dawn %>% as.factor())

masterias_depth_daynight_activity <- masterias_depth_daynight %>% 
  filter(tag_serial_number %in% c("1293321", "1293308"), dusk == 0 & dawn == 0) %>% #filter out dusk and dawn: not used right now
  left_join(dst_longterm_periods,
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = date, y_lower = start_date, y_upper = end_date))) %>%
  dplyr::select(!c(start_date, end_date))

masterias_depth_daynight_activity <- masterias_depth_daynight_activity %>%
  full_join(masterias_depth_daynight_activity %>% 
              group_by(tag_serial_number, date) %>% 
              reframe(depth_mean = sum(depth_mean)/2,
                      depth_sd = sum(depth_sd)/2,
                      depth_median = sum(depth_median)/2,
                      depth_min = min(depth_min),
                      depth_max = max(depth_max),
                      depth_range = depth_max - depth_min,
                      vertical_speed_mean = sum(vertical_speed_mean)/2,
                      vertical_speed_sd = sum(vertical_speed_sd)/2,
                      vertical_speed_median = sum(vertical_speed_median)/2,
                      vertical_speed_min = min(vertical_speed_min),
                      vertical_speed_max = max(vertical_speed_max),
                      vertical_speed_range = vertical_speed_max - vertical_speed_min,
                      activity = unique(activity),
                      year = unique(year),
                      day = "full",
                      dusk = NA,
                      dawn = NA)) %>%
  mutate(month = date %>% lubridate::month(),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month))