# Script to calculate summary statistics of the dst depth logs

# 1. workspace ####
library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(psdr)
library(ggplot2)
library(StreamMetabolism)
library(suncalc)
library(sf)
library(zoo)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))

source(paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_geolocation_output.R"))
# source(paste0(dir_path, "/01_code/04_analyses/FFT/calculate_fft_psd.R"))
# source(paste0(dir_path, "/01_code/05_plots_maps/plot_fft_periodogram.R"))
# source(paste0(dir_path, "/01_code/05_plots_maps/plot_dst_pressure_temp.R"))


# manual separation of overwintering, migrating and oversummering periods of longterm dst tracks

dst_longterm_periods <- tibble(tag_serial_number = c(rep("1293308", times = 6), rep("1293321", times = 8)),
                               start_date = c("2018-08-02", "2018-08-22", "2018-10-10", "2019-03-23", "2019-05-13", "2019-08-04", 
                                              "2018-07-18", "2018-08-21", "2018-10-23", "2019-04-22", "2019-05-11", "2019-08-31", "2019-09-26", "2019-11-17") %>% 
                                 as.POSIXct(tz = "UTC"),
                               activity = c("summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "recording_end",
                                            "summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "winter_migration", "winter_residency", "recording_end")) %>%
  mutate(year = start_date %>% lubridate::year(),
         end_date = start_date %>% lead() %>% replace_na("2019-11-16" %>% as.POSIXct(tz = "UTC")),
         # start_date = start_date - lubridate::days(1), # wronnnng!
         start_week = start_date %>% lubridate::week(),
         start_month = start_date %>% lubridate::month(),
         start_week = sprintf("%02d", start_week %>% as.numeric()),
         start_month = sprintf("%02d", start_month %>% as.numeric()),
         start_weekyear = paste0(year, "-", start_week),
         start_monthyear = paste0(year, "-", start_month),
         end_date = end_date - lubridate::days(1),
         end_week = end_date %>% lubridate::week(),
         end_month = end_date %>% lubridate::month(),
         end_week = sprintf("%02d", end_week %>% as.numeric()),
         end_month = sprintf("%02d", end_month %>% as.numeric()),
         end_weekyear = paste0(year, "-", end_week),
         end_monthyear = paste0(year, "-", end_month))

death_dates <- tibble(
  tag_serial_number = c("1293295", "1293319", "1293322", "1293304", "1293310", "1293312", "1293308", "1293321"),
  death_date = c("2018-08-19", "2018-08-08", "2018-08-08", "2019-07-21", "2019-08-13", "2019-08-07", "2019-12-12", "2019-12-12"))

# 2. day/night, dusk/dawn with `masterias_dst_geolocation_output` ####
masterias_depth_temp_summary <- masterias_depth_temp %>% 
  mutate(dateyear = lubridate::date(date_time)) %>% 
  left_join(masterias_dst_geolocation_output %>% 
              dplyr::select(tag_serial_number, date_time, detection_latitude, detection_longitude, sex, length1),
            by = join_by(tag_serial_number == tag_serial_number, dateyear == date_time), multiple = "all") %>%
  dplyr::select(!t) %>% rename(date = dateyear) %>%
  mutate(week = date %>% lubridate::week(),
         year = date %>% lubridate::year(),
         month = date %>% lubridate::month(),
         month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         week = sprintf("%02d", week %>% as.numeric()),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month),
         weekyear = paste0(year, "-", week),
         time = date_time %>% format("%H:%M:%S")) %>%
  left_join(death_dates, by = "tag_serial_number") %>%
  ungroup() %>% group_by(tag_serial_number) %>%
  filter(date > (min(date) + lubridate::days(7)), # filter out first week of data
         date < death_date) #filter out dates after death
  # filter(!is.na(detection_latitude)) # get rid of vals after the DST model was stopped

# rm(masterias_depth_temp)

time_bins <- suncalc::getSunlightTimes(data = masterias_dst_geolocation_output %>% 
                                    dplyr::select(date_time, detection_latitude, detection_longitude, tag_serial_number) %>% 
                                    mutate(date_time = date_time %>% as.Date()) %>%
                                    rename(date = date_time, lat = detection_latitude, lon = detection_longitude) %>%
                                    # add one day before and after time series, possible improvement: dont hard code lat, lon and date
                                    tibble::add_row(.before = 1, date = "2018-07-18" %>% as.Date(), lat = 51.61220, lon = 3.64631481, tag_serial_number = "1293295") %>%
                                    tibble::add_row(date = "2019-11-16" %>% as.Date(), lat = 52.59358, lon = 2.391523, tag_serial_number = "1293321"),
                                  keep = c("sunrise", "sunset", "dawn", "dusk")
                                  ) %>%
  # add tag serial number so we can join that later with the sharks
  left_join(masterias_dst_geolocation_output %>% dplyr::select(tag_serial_number, detection_latitude, detection_longitude),
  by = join_by(lat == detection_latitude, lon == detection_longitude), multiple = "all")  %>%
  mutate(tag_serial_number = ifelse(is.na(tag_serial_number), ifelse(date == "2018-07-18", "1293295", "1293321") ,tag_serial_number)) %>%
  group_by(tag_serial_number) %>%
  rename(day_start = sunrise, day_end = sunset, dusk_end = dusk, dawn_start = dawn) %>%
  arrange(tag_serial_number, date) %>%
  dplyr::distinct()

# make time bin flags in depthlog df
## if day == 0 that means that it's night
masterias_depth_temp_summary <- masterias_depth_temp_summary %>% left_join(time_bins, by = join_by(date == date, detection_latitude == lat, detection_longitude == lon, tag_serial_number == tag_serial_number), multiple = "all") %>%
  mutate(day = ifelse(date_time %>% dplyr::between(day_start, day_end), 1, 0),
         dusk = ifelse(date_time %>% dplyr::between(day_end, dusk_end), 1, 0),
         dawn = ifelse(date_time %>% dplyr::between(dawn_start, day_start), 1, 0),
         date_reduction = ifelse(day == 0 & lubridate::hour(date_time) < 12, 1, 0),
         date_24hcycle = date - lubridate::days(date_reduction),
         vertical_speed_m_min = abs(lead(depth_m, order_by = date_time) - depth_m) / 2) %>%
  dplyr::select(!date_reduction) %>%
  mutate(day = day %>% as.factor()) %>%
  ungroup()

# 3. summary day/night ####

# hour of max and min depth

# masterias_depth_temp_summary %>% group_by(tag_serial_number) %>%
#   summarise(start_date = min(date),
#             end_date = max(date)) %>% View()

# test <- masterias_depth_temp_summary %>% filter(tag_serial_number == "1293295")
# 
# test %>% group_by(date) %>%
#   summarise(time_depth_max = test %>% group_by(date) %>% filter(depth_m == max(depth_m)) %>%
#               slice_max(depth_m) %>%
#               select(date_time) %>% pull(),
#             hour_depth_max = time_depth_max %>% lubridate::hour())
# test %>% group_by(date, day) %>%
#   summarize(time_depth_max = date_time[which.max(depth_m)],
#             hour_depth_max = time_depth_max %>% lubridate::hour())
# 
# test %>%
#   filter(depth_m == max(depth_m)) %>%
#   slice_max(depth_m) %>%
#   select(date_time) %>% pull()# %>% lubridate::hour()


# masterias_depth_temp_summary %>% group_by(date, tag_serial_number, day, dusk, dawn) %>%
#   summarize(hour_of_max_depth = masterias_depth_temp_summary %>% filter(depth_m == depth_m %>% max()) %>% select(date) %>% pull() %>% lubridate::hour())
#   # summarise(hour_depth_max = lubridate::hour() %>% where(depth_m == depth_m %>% max())) %>% View()

masterias_depth_daynight <- masterias_depth_temp_summary %>% group_by(date, tag_serial_number, day, dusk, dawn) %>%
  reframe(depth_mean = mean(depth_m, na.rm = T), 
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
            vertical_speed_median = median(vertical_speed_m_min, na.rm = T),
            hour_depth_max = date_time[which.max(depth_m)] %>% lubridate::hour() %>% as.factor(), #find out how to order the numbers correctly #levels = c("0", "1"," 2", "3", "4",'5',"6"," 7", "8", "9", "10", "11", "12", "13" ,"14" ,"15", "16", "17", "18", "1", "20", "21", "22", "23", "24")
            hour_depth_min = date_time[which.min(depth_m)] %>% lubridate::hour() %>% as.factor(),
            hour_vertical_speed_max = date_time[which.max(vertical_speed_m_min)] %>% lubridate::hour() %>%as.factor()) %>%
  mutate(day = day %>% as.factor(),
         dusk = dusk %>% as.factor(),
         dawn = dawn %>% as.factor(),
         week = date %>% lubridate::week(),
         year = date %>% lubridate::year(),
         month = date %>% lubridate::month(),
         month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         week = sprintf("%02d", week %>% as.numeric()),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month),
         weekyear = paste0(year, "-", week),
         monthday = paste0(month, "-", date %>% lubridate::day()))  %>%
  left_join(dst_longterm_periods, # get manually separated activities and periods
            by = join_by(tag_serial_number,year,
                         between(x = date, y_lower = start_date, y_upper = end_date))) %>%
  dplyr::select(!c(start_weekyear, end_weekyear, start_week, start_month, end_week, end_month, end_monthyear, start_monthyear, end_date, start_date))

## 3.1. summary date ####

masterias_depth_date <- masterias_depth_temp_summary %>% group_by(date, tag_serial_number) %>%
  summarise(depth_mean = mean(depth_m, na.rm = T), 
            depth_sd = sd(depth_m, na.rm = T),
            depth_var = var(depth_m, na.rm = T),
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
  mutate(day = "full") %>%
  left_join(dst_longterm_periods, # get manually separated activities and periods
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = date, y_lower = start_date, y_upper = end_date))) %>%
  dplyr::select(!c(start_weekyear, end_weekyear, start_week, start_month, end_week, end_month, end_monthyear, start_monthyear, end_date, start_date))


# 4. summary week ####

masterias_depth_week <- masterias_depth_daynight %>%
  mutate(week = date %>% lubridate::week(),
         year = date %>% lubridate::year(),
         month = date %>% lubridate::month()) %>% #,
  # month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  group_by(tag_serial_number, year, week, day) %>%
  summarise(depth_mean = mean(depth_mean, na.rm = T),
            depth_sd = sd(depth_sd, na.rm = T),
            depth_median = median(depth_median),
            depth_min = min(depth_min, na.rm = T),
            depth_max = max(depth_max, na.rm = T),
            depth_range_mean = mean(depth_range, na.rm = T),
            depth_range_sd = sd(depth_range, na.rm = T),
            vertical_speed_min = min(vertical_speed_min, na.rm = T),
            vertical_speed_max = max(vertical_speed_max, na.rm = T),
            vertical_speed_mean = mean(vertical_speed_mean, na.rm = T),
            vertical_speed_sd = sd(vertical_speed_sd, na.rm = T),
            vertical_speed_median = median(vertical_speed_median, na.rm = T),
            vertical_speed_range_mean = mean(vertical_speed_range, na.rm = T),
            vertical_speed_range_sd = sd(vertical_speed_range, na.rm = T),
            vertical_speed_range_median = median(vertical_speed_range, na.rm = T),
            month = month %>% min()) %>%
  mutate(month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         week = sprintf("%02d", week %>% as.numeric()),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month),
         weekyear = paste0(year, "-", week)) %>%
  left_join(dst_longterm_periods, # get manually separated activities and periods
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = weekyear, y_lower = start_weekyear, y_upper = end_weekyear),
                         year)) %>%
  dplyr::select(!c(start_weekyear, end_weekyear, start_week, start_month, end_week, end_month, end_monthyear, start_monthyear, end_date, start_date))




# 5. summary month ####

masterias_depth_month <- masterias_depth_daynight %>%
  mutate(month = date %>% lubridate::month(),
         year = date %>% lubridate::year()) %>%
  # ungroup() %>%
  group_by(tag_serial_number, year, month) %>%
  summarise(
    # depth_mean = mean(depth_mean), 
    #         depth_sd = sd(depth_sd),
    #         depth_min = min(depth_min),
    #         depth_max = max(depth_max),
    #         depth_range_mean = mean(depth_range),
    #         depth_range_sd = sd(depth_range),
            
            depth_mean = mean(depth_mean, na.rm = T),
            depth_sd = sd(depth_sd, na.rm = T),
            depth_median = median(depth_median),
            depth_min = min(depth_min, na.rm = T),
            depth_max = max(depth_max, na.rm = T),
            depth_range_mean = mean(depth_range, na.rm = T),
            depth_range_sd = sd(depth_range, na.rm = T),
            vertical_speed_min = min(vertical_speed_min, na.rm = T),
            vertical_speed_max = max(vertical_speed_max, na.rm = T),
            vertical_speed_mean = mean(vertical_speed_mean, na.rm = T),
            vertical_speed_sd = sd(vertical_speed_sd, na.rm = T),
            vertical_speed_median = median(vertical_speed_median, na.rm = T),
            vertical_speed_range_mean = mean(vertical_speed_range, na.rm = T),
            vertical_speed_range_sd = sd(vertical_speed_range, na.rm = T),
            vertical_speed_range_median = median(vertical_speed_range, na.rm = T),
            month = month %>% min()) %>%
  mutate(month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month)) %>%
  left_join(dst_longterm_periods, # get manually separated activities and periods
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = monthyear, y_lower = start_monthyear, y_upper = end_monthyear),
                         year)) %>%
  dplyr::select(!c(start_weekyear, end_weekyear, start_week, start_month, end_week, end_month, end_monthyear, start_monthyear, end_date, start_date))

# daily summaries ####
## 6. vertical movement behaviour ####

# summary of depth median and mean per night and day
masterias_DVM_sum <- masterias_depth_temp_summary %>% group_by(tag_serial_number, date_24hcycle, day) %>%
  summarise(depth_med = median(depth_m),
            depth_mean = mean(depth_m)) %>%
  pivot_wider(names_from = day,
              values_from = c(depth_med, depth_mean)) %>%
  rename(depth_med_day = depth_med_1, depth_med_night = depth_med_0, depth_mean_day = depth_mean_1, depth_mean_night = depth_mean_0)

depth_wilcox <- masterias_depth_temp_summary %>% dplyr::select(tag_serial_number, date_24hcycle, day, depth_m) %>% 
  group_by(tag_serial_number, date_24hcycle, day) %>% 
  mutate(depth_id = row_number()) %>%
  pivot_wider(names_from = day, values_from = depth_m, id_cols = c("tag_serial_number", "depth_id", "date_24hcycle")) %>%
  rename(depth_m_day = "1", depth_m_night = "0") %>%
  dplyr::select(!depth_id)


sum_wilcox <- depth_wilcox %>% group_by(tag_serial_number, date_24hcycle) %>%
  summarise(p_val_wilcox =ifelse(depth_m_day %>% is.na() %>% all() | depth_m_night %>% is.na() %>% all(),
                                 NA,
                                 wilcox.test(depth_m_day, depth_m_night)$p.value #wilcox.test(depth_m_day, depth_m_night)$p.value
  )) 


## 7. horizontal distance ####

# join dst geolocation output with DVM sum dataframe
masterias_DVM_sum <- masterias_DVM_sum %>% 
  left_join(masterias_dst_geolocation_output %>% 
              dplyr::select(tag_serial_number, date_time, detection_latitude, detection_longitude),
            by = join_by(date_24hcycle == date_time, tag_serial_number == tag_serial_number)) %>%
  mutate(location_currentday = c(detection_latitude, detection_longitude) %>%
           st_point() %>% st_sfc() %>% st_set_crs(4326),
         horizontal_distance_m = NA)

# masterias_DVM_sum <- masterias_DVM_sum %>% mutate(horizontal_distance = NA)

for(i in 2:nrow(masterias_DVM_sum)){
  masterias_DVM_sum$horizontal_distance_m[i] <- st_distance(masterias_DVM_sum$location_currentday[i], masterias_DVM_sum$location_currentday[i-1], by_element = TRUE) %>%
    as.numeric()
}

# masterias_DVM_sum <- masterias_DVM_sum %>% rename(horizontal_distance_m = horizontal_distance)
# empty <- st_as_sfc("POINT(EMPTY)")
# 
# df %>%
#   mutate(
#     elapsed_time = lead(timestamp) - timestamp,
#     distance_to_next = sf::st_distance(
#       geometry, 
#       lead(geometry, default = empty), 
#       by_element = TRUE)
#   )
#   
#   a <- st_sfc(st_point(c(51.44, 3.5))) %>% 
#   st_set_crs(4326)
# 
# b <- st_sfc(st_point(c(51.44, 3.57))) %>% 
#   st_set_crs(4326)
# 
# st_distance(a,b) %>% as.numeric()

# Units: [m]
# [,1]
# [1,] 671910.7

# join the sum df and the outcome of the wilcox test
#### DVM day ####
masterias_DVM_sum_day <- masterias_DVM_sum %>% left_join(sum_wilcox, by = c("date_24hcycle", "tag_serial_number")) %>% ungroup() %>%
  mutate(vertical_movement = ifelse(p_val_wilcox < 0.05,
                                    ifelse(depth_med_day > depth_med_night,
                                           "DVM",
                                           "rDVM"),
                                    "nVM")) %>%
  filter(!is.na(vertical_movement))

#### DVM week ####

masterias_DVM_sum_week <- masterias_DVM_sum_day %>%
  mutate(week = date_24hcycle %>% lubridate::week(), # %>% as.numeric() %>% sprintf("%02d")
         year = date_24hcycle %>% lubridate::year(),
         month = date_24hcycle %>% lubridate::month()) %>% #,
  # month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  group_by(tag_serial_number, year, week, vertical_movement) %>%
  summarise(count = n(),
            month = month %>% min()) %>%
  mutate(month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         week = sprintf("%02d", week %>% as.numeric()),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month),
         weekyear = paste0(year, "-", week)) #%>%
# pivot_wider(names_from = vertical_movement, values_from = count, values_fill = 0) 
  
# TODO: add Month for each week
masterias_dist_sum_week <- masterias_DVM_sum_day %>%
  mutate(week = date_24hcycle %>% lubridate::week(), # %>% as.numeric() %>% sprintf("%02d")
         year = date_24hcycle %>% lubridate::year(),
         month = date_24hcycle %>% lubridate::month()) %>% #,
  # month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  group_by(tag_serial_number, year, week) %>%
  summarise(hor_distance_mean = horizontal_distance_m %>% mean(na.rm = T),
            hor_distance_sd = horizontal_distance_m %>% sd(na.rm = T),
            hor_distance_min = horizontal_distance_m %>% min(na.rm = T),
            hor_distance_max = horizontal_distance_m %>% max(na.rm = T),
            hor_distance_median = horizontal_distance_m %>% median(na.rm = T),
            hor_distance_range = hor_distance_max - hor_distance_min,
            hor_distance_total = sum(horizontal_distance_m))
# todo: merge df of DVM and distance
  
  #### DVM month ####

masterias_DVM_sum_month <- masterias_DVM_sum_week %>%
  ungroup() %>%
  group_by(tag_serial_number, month, year, vertical_movement) %>%
  summarise(count = sum(count)) %>%
  mutate(month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month))
# %>%
#   mutate(week = sprintf("%02d", week %>% as.numeric()),
#          weekyear = paste0(year, "-", week))
# %>% 
# pivot_wider(names_from = vertical_movement, values_from = count, values_fill = 0)
# TODO: add Month for each week

# long term dst tacks (308 and 321) ####
long_dst_date <- masterias_depth_date %>% 
  filter(tag_serial_number %in% c("1293321", "1293308")) %>%
  group_by(tag_serial_number) %>%
  mutate(depth_range_change = depth_range - dplyr::lead(depth_range),
         depth_median_change = depth_median - dplyr::lead(depth_median),
         depth_median_change2 = depth_median_change - dplyr::lead(depth_median_change),
         depth_min_change = depth_min - dplyr::lead(depth_min),
         depth_max_change = depth_max - dplyr::lead(depth_max),
         depth_median_roll3 = zoo::rollmean(depth_median, k = 3, fill = NA),
         depth_median_roll7 = zoo::rollmean(depth_median, k = 7, fill = NA),### rolling median and daily change
         depth_max_roll3 = zoo::rollmean(depth_max, k = 3, fill = NA),
         depth_min_roll3 = zoo::rollmean(depth_min, k = 3, fill = NA),
         depth_range_roll3 = zoo::rollmean(depth_range, k = 3, fill = NA),
         depth_range_roll7 = zoo::rollmean(depth_range, k = 7, fill = NA),
         depth_range_change_roll3 = zoo::rollmean(depth_range_change, k = 3, fill = NA),
         depth_range_change_roll7 = zoo::rollmean(depth_range_change, k = 7, fill = NA),
         depth_median_change_roll3 = zoo::rollmean(depth_median_change, k = 3, fill = NA),
         depth_median_change2_roll3 = zoo::rollmean(depth_median_change2, k = 3, fill = NA),
         depth_median_change_roll7 = zoo::rollmean(depth_median_change, k = 7, fill = NA),
         # depth_median_change_roll7 = depth_median_roll7 - dplyr::lead(depth_median_roll7),
         depth_min_change_roll3 = zoo::rollmean(depth_min_change, k = 3, fill = NA),
         depth_max_change_roll3 = zoo::rollmean(depth_max_change, k = 3, fill = NA),
         t_days = seq(from = 1, to = n()),
         t_days = sprintf("%03d", t_days %>% as.numeric()),
         depth_median_change_sgolay = depth_median_change %>% signal::sgolayfilt(p = 7, n = 9),
         depth_range_change_sgolay = depth_range_change %>% signal::sgolayfilt(p = 7, n = 9),
         depth_median_sgolay = depth_median %>% signal::sgolayfilt(p = 1, n = 5),
         depth_range_sgolay = depth_range %>% signal::sgolayfilt(p = 5, n = 7),
         depth_max_sgolay = depth_max %>% signal::sgolayfilt(p = 1, n = 5),
         depth_min_sgolay = depth_min %>% signal::sgolayfilt(p = 1, n = 5)) %>%
  # tidyr::drop_na() %>%
  ungroup()


long_dst_daynight <- masterias_depth_daynight %>% 
  ungroup() %>%
  filter(tag_serial_number %in% c("1293321", "1293308"), dawn == 0 & dusk == 0) %>%
  dplyr::select(!c(dawn, dusk)) %>%
  group_by(tag_serial_number, day) %>%
  mutate(depth_median_roll3 = zoo::rollmean(depth_median, k = 3, fill = NA), ### rolling median and daily change
         depth_max_roll3 = zoo::rollmean(depth_max, k = 3, fill = NA),
         depth_min_roll3 = zoo::rollmean(depth_min, k = 3, fill = NA),
         depth_range_roll3 = zoo::rollmean(depth_range, k = 3, fill = NA)) %>%
  tidyr::drop_na()

# # total summary for dst data ####
# 
# dst_summary <- masterias_depth_date %>% 
#   # mutate(tag_serial_number = tag_serial_number %>% as.numeric()) %>%
#   dplyr::group_by(tag_serial_number) %>%
#   summarise(release_date = min(date) - lubridate::days(8),
#          death_date = date %>% max(),
#          time_at_liberty = base::difftime(release_date, death_date, units = "days")%>% abs() %>% as.numeric()) %>%
#   arrange(time_at_liberty) %>%
#   left_join(masterias_info %>%
#               dplyr::select(tag_serial_number, sex), 
#             by = "tag_serial_number") %>%
#   mutate(release_date = release_date %>% as.Date(),
#          death_date = death_date %>% as.Date())


# save data ####

save_data(data = masterias_depth_temp_summary, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_depth_daynight, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_depth_date, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_depth_week, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_depth_month, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = long_dst_date, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = long_dst_daynight, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_DVM_sum_day, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_DVM_sum_week, folder = paste0(dir_path, "/02_results/dst_summary/"))
save_data(data = masterias_DVM_sum_month, folder = paste0(dir_path, "/02_results/dst_summary/"))

# save_data(data = dst_summary, folder = paste0(dir_path, "/02_results/dst_summary/"))
rm(sum_wilcox, depth_wilcox)


# old ####

# ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number == "1293308")) +
#   geom_point(aes(x = date_time, y = -depth_m, colour = temp_c)) +
#   labs(title = "tag 308 (female)", x = "date", y = "depth in m") +
#   theme_dark()

# ??????whats going on here????

plot_depthtemp_308 <- ggplot() +
  geom_point(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"), aes(x = date_time, y = -depth_m, color = temp_c)) +
  scale_y_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60, -70)) +
  labs(y = "Depth in m", x = "Date", color = "Temperature in C", title = "tag 308 (female)") +
  scale_colour_distiller(palette ="RdYlBu") +
  theme_dark()
plot_depthtemp_321 <- ggplot() +
  geom_point(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = -depth_m, color = temp_c), size = 1) +
  scale_y_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60, -70)) +
  labs(y = "Depth in m", x = "Date", color = "Temperature in C", title = "tag 321 (male)") +
  scale_colour_distiller(palette ="RdYlBu") +
  theme_dark()

ggsave(plot = plot_depthtemp_321, filename = paste0(dir_path, "/plot_depthtemp_321.png"))
ggsave(plot = plot_depthtemp_308, filename = paste0(dir_path, "/plot_depthtemp_308.png"))

plot_depthtemp_308
plot_depthtemp_321

plot_tempdepth_308 <- ggplot() +
  geom_point(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"), aes(x = date_time, y = temp_c, color = -depth_m), size = 1) +
  scale_y_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60, -70)) +
  labs(y = "Temperature in C", x = "Date", color = "Depth in m", title = "tag 308 (female)") +
  scale_colour_distiller(palette ="RdYlBu") +
  theme_dark()

plot_tempdepth_321 <- ggplot() +
  geom_point(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = temp_c, color = -depth_m), size = 1) +
  scale_y_continuous(breaks = c(0, -10, -20, -30, -40, -50, -60, -70)) +
  labs(y = "Temperature in C", x = "Date", color = "Depth in m", title = "tag 321 (male)") +
  scale_colour_distiller(palette ="RdYlBu") +
  theme_dark()

ggsave(plot = plot_tempdepth_308, filename = paste0(dir_path, "/plot_tempdepth_308.png"))
ggsave(plot = plot_tempdepth_321, filename = paste0(dir_path, "/plot_plot_tempdepth_321.png"))



# masterias_depth_week <- masterias_depth_temp %>%
#   mutate(week = date %>% lubridate::week(),
#          year = date %>% lubridate::year(), 
#          month = date %>% lubridate::month()) %>% #,
#          # month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
#   group_by(tag_serial_number, year, week) %>%
#   summarise(depth_mean = mean(depth_m), 
#             depth_sd = sd(depth_m),
#             depth_min = min(depth_m),
#             depth_max = max(depth_m),
#             depth_range = depth_max - depth_min,
#             month = month %>% min(),
#             month_name = month.abb[month] %>% factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))


# check what sd I want to calculate
# test <- masterias_depth_daynight %>% 
#   filter(tag_serial_number == "1293308") %>%
#   mutate(month = date %>% lubridate::month(),
#          year = date %>% lubridate::year())
# 
# test <- test %>% 
#   group_by(tag_serial_number, year, month) %>%
#   summarise(depth_mean = mean(depth_mean), 
#             depth_sd_sd = sd(depth_sd),
#             depth_sd_mean = sd(depth_mean),
#             depth_min = min(depth_min),
#             depth_max = max(depth_max),
#             depth_range_mean = mean(depth_range),
#             depth_range_sd = sd(depth_range))
# 
# test2 <- tibble(x = c(2,2,2,4, 5, 6, 7, 9, 11), day = c(1,1,1,2,2,2,3,3,3))
# test2_day <- test2 %>% group_by(day) %>% summarise(mean = mean(x), sd = sd(x))
# test2_all1 <- test2_day %>% summarise(mean = mean(mean), sd = sd(sd))
# test2_all2 <- test2 %>% summarise(mean = mean(x), sd = sd(x))


### with test subset #
# 
# test <- masterias_depth_temp_DVM %>% ungroup() %>%
#   filter(date_24hcycle %>% as.character() %in% c("2018-07-20", "2018-07-29", "2018-08-10") & tag_serial_number == "1293295") #%>% #date_24hcycle == "2018-07-22" %>% as.Date() & 
# 
# # summary of depth median and mean per night and day
# test_sum2 <- test %>% group_by(date_24hcycle, day) %>%
#   summarise(depth_med = median(depth_m),
#             depth_mean = mean(depth_m)) %>%
#   pivot_wider(names_from = day,
#               values_from = c(depth_med, depth_mean)) %>%
#   rename(depth_med_day = depth_med_1, depth_med_night = depth_med_0, depth_mean_day = depth_mean_1, depth_mean_night = depth_mean_0)
# 
# depth_wilcox <- test %>% dplyr::select(date_24hcycle, day, depth_m) %>% 
#   group_by(date_24hcycle, day) %>% 
#   mutate(depth_id = row_number()) %>%
#   pivot_wider(names_from = day, values_from = depth_m, id_cols = c("depth_id", "date_24hcycle")) %>%
#   rename(depth_m_day = "1", depth_m_night = "0") %>%
#   dplyr::select(!depth_id)
# 
# sum_wilcox <- depth_wilcox %>% group_by(date_24hcycle) %>%
#   summarise(p_val_wilcox = wilcox.test(depth_m_day, depth_m_night)$p.value)
# 
# # join the sum df and the outcome of the wilcox test
# test_sum2 <- test_sum2 %>% left_join(sum_wilcox, by = "date_24hcycle") %>% ungroup() %>%
#   mutate(vertical_movement = ifelse(p_val_wilcox < 0.05,
#                                     ifelse(depth_med_day > depth_med_night,
#                                            "DVM",
#                                            "rDVM"),
#                                     "nVM"))



# p_val_wilcox = wilcox.test(test %>% filter(day == 1) %>% dplyr::select(depth_m) %>% pull(), 
#                            test %>% filter(day == 0) %>% dplyr::select(depth_m) %>% pull())$p.value,
# %>% 
#   mutate(vertical_movement = ifelse(p_val_wilcox < 0.05,
#                                     ifelse(depth_med_day > depth_med_night,
#                                            "DVM",
#                                            "rDVM"),
#                                     "nVM"))


# test <- tibble(week = c(23,23,23,24,24),
#                vertical_mov = c("DVM", "nVM", "rDVM", "DVM", "DVM"))
# 
# test_sum <- test %>% group_by(week, vertical_mov) %>% summarise(count = n()) %>% 
#   pivot_wider(names_from = vertical_mov, values_from = count, values_fill = 0)
#   



# test_sum <- test %>% group_by(date_24hcycle) %>%
#   summarise(p_val_wilcox = wilcox.test(test %>% filter(day == 1) %>% dplyr::select(depth_m) %>% pull(), 
#                                        test %>% filter(day == 0) %>% dplyr::select(depth_m) %>% pull())$p.value,
#             depth_med_day = stats::median(test %>% filter(day == 1) %>% dplyr::select(depth_m) %>% pull()),
#             depth_med_night = stats::median(test %>% filter(day == 0) %>% dplyr::select(depth_m) %>% pull()),
#             depth_mean_day = mean(test %>% filter(day == 1) %>% dplyr::select(depth_m) %>% pull()),
#             depth_mean_night = mean(test %>% filter(day == 0) %>% dplyr::select(depth_m) %>% pull())) %>% 
#   mutate(vertical_movement = ifelse(p_val_wilcox < 0.05,
#                                     ifelse(depth_med_day > depth_med_night,
#                                            "DVM",
#                                            "rDVM"),
#                                     "nVM"))
# 
# result <- wilcox.test(test %>% filter(day == 1) %>% dplyr::select(depth_m) %>% pull(), 
#                       test %>% filter(day == 0) %>% dplyr::select(depth_m) %>% pull())$p.value

# 7. vertical speed 
# 
# 
# test <- masterias_depth_temp %>% filter(tag_serial_number == "1293295") %>% dplyr::select(date_time, depth_m)
# 
# test2 <- test %>% mutate(lead = dplyr::lead(depth_m, order_by = date_time),
#                          vertical_speed_m_min = abs(lead(depth_m, order_by = date_time) - depth_m) / 2)
# 
# masterias_depth_temp_vertspeed <- masterias_depth_temp %>% 
#   mutate(vertical_speed_m_min = abs(lead(depth_m, order_by = date_time) - depth_m) / 2)



# test <- depth_wilcox %>% filter(tag_serial_number == "1293308") %>% ungroup() %>% group_by(tag_serial_number, date_24hcycle) %>%
#   summarise(test = ifelse(!depth_m_night %>% is.na() %>% all(),
#                      "wilc test possible", #wilcox.test(depth_m_day, depth_m_night)$p.value
#                      NA)) #%>% distinct()
#   

# mutate( test = 
#                                  ifelse(!depth_m_day %>% is.na() & !depth_m_night %>% is.na(),
#                                         "yes",
#                                         NA))



# change dates to fit 24h cycle (i.e. make time before sunrise of a day part of the day before)

# masterias_depth_temp_DVM <- masterias_depth_temp %>%
#   mutate(date_reduction = ifelse(day == 0 & lubridate::hour(date_time) < 12,
#                                  1, 0),
#          date_24hcycle = date - lubridate::days(date_reduction)) %>%
#   dplyr::select(!date_reduction) %>%
#   ungroup()


# # test 
# test <-  masterias_depth_temp %>% ungroup() %>%
#   filter(date %>% as.character() %in% c("2018-08-25", "2018-08-26") & tag_serial_number == "1293295") #%>% #date_24hcycle == "2018-07-22" %>% as.Date() & 
# 
# test2 <- test %>% 
#   mutate(detection_latitude = 
#            ifelse(detection_latitude %>% is.na(),
#                   detection_latitude[date == date %>% max() - lubridate::days(1)],
#                   detection_latitude),
#          detection_longitude = 
#            ifelse(detection_longitude %>% is.na(),
#                   detection_longitude[date == date %>% max() - lubridate::days(1)],
#                   detection_longitude),
#          sex = sex %>% max(na.rm = T),
#          length1 = length1 %>% max(na.rm = T)
#          )