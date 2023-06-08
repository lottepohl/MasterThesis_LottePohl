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
paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_models.R") %>% base::source()

# table 1: list of abbreviations ####

abbreviations_list <- base::data.frame(Abbreviation = c("ADST", "DST", "BPNS", "f", "m", "CWT", "ETN", "PBARN", "HMM", "TL", "FT", "FFT", "UTC", "RQ", "ICES"),
                         Explanation= c("Acoustic Data Storage Tag",
                                        "Data Storage Tag",
                                        "Belgian Part of the North Sea",
                                        "female",
                                        "male",
                                        "Continuous Wavelet Transform",
                                        "European Tracking Network",
                                        "Permanent Belgian Receiver Network",
                                        "Hidden Markov Model",
                                        "Total Length",
                                        "Fourier Transformation",
                                        "Fast Fourier Transform",
                                        "Coordinated Universal Time",
                                        "Research Question",
                                        "International Council for the Exploration of the Sea")) %>%
  dplyr::arrange(Abbreviation)

# table 2: release locations ####

release_locations <- masterias_info %>% mutate(date = release_date_time %>% lubridate::date(),
                                          release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Western Scheldt")) %>% 
  group_by(release_loc) %>% reframe(lat = release_latitude %>% mean(),
                                    lng = release_longitude %>% mean()) #%>%
  # mutate(label = c(1,2))

# table 5: dst summary ####

dst_summary <- masterias_depth_date %>% 
  dplyr::group_by(tag_serial_number) %>%
  summarise(release_date = min(date) - lubridate::days(8),
            death_date = date %>% max(),
            days_at_liberty = base::difftime(release_date, death_date, units = "days")%>% abs() %>% as.numeric(),
            depth_min_total = min(depth_min),
            depth_max_total = max(depth_max),
            temp_min_total = min(temp_min),
            temp_max_total = min(temp_max),
            date_depth_max = date[which.max(depth_max)],
            date_depth_min = date[which.min(depth_min)],
            date_temp_max = date[which.max(temp_max)],
            date_temp_min = date[which.min(temp_min)]) %>%
  arrange(days_at_liberty) %>%
  left_join(masterias_info %>%
              dplyr::select(tag_serial_number, sex, recapture_date_time), 
            by = "tag_serial_number") %>%
  mutate(release_date = release_date %>% as.Date(),
         death_date = death_date %>% as.Date(),
         recapture_date_time = recapture_date_time %>% as.Date())
# include max and min depth, and max and min temp?

# tagged_animal_info <- tagged_animal_info %>% dplyr::full_join(
#   dst_summary %>% 
#     dplyr::select(tag_serial_number, days_at_liberty) %>%
#     dplyr::rename(days_at_liberty = days_at_liberty)) %>% View()



# table 3: tagged animal summary ####

acoustic_days_liberty <- detections_tempdepth_daynight %>% group_by(tag_serial_number) %>%
  summarise(days_detected = date_time %>% lubridate::date() %>% unique() %>% length(),
            hours_detected = paste0(date_time %>% lubridate::date(), '-', date_time %>% lubridate::hour()) %>% unique() %>% length(),
            date_last_detected = date_time %>% lubridate::date() %>% max(),
            residency_index = (days_detected / 518) %>% round(digits = 3)) #518 days == tag battery lifetime

tagged_animal_info <- masterias_info %>% 
  mutate(release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Western Scheldt")) %>%
  dplyr::select(tag_serial_number, sex, length1, weight, release_date_time, n_detect, release_loc) %>% #, capture_method
  mutate(release_date_time = lubridate::date(release_date_time)
         # ,days_at_liberty = 0
         ) %>%
  left_join(acoustic_days_liberty, by = "tag_serial_number") %>%
  # left_join(dst_summary %>% dplyr::select(tag_serial_number, days_at_liberty)) #%>%
  mutate(
    # days_at_liberty = ifelse(days_at_liberty == 0, 
    #                               base::difftime(date_last_detected, release_date_time, tz = "UTC", units = "days") %>% as.numeric(), 
    #                               days_at_liberty),
         days_at_liberty = base::difftime(date_last_detected, release_date_time, tz = "UTC", units = "days") %>% as.numeric(),
         days_at_liberty = ifelse(n_detect == 1,  1, days_at_liberty),
         days_detected = ifelse(n_detect == 1, 1, days_detected),
         length1 = length1 / 100) %>%
  tidyr::replace_na(replace = list(n_detect = 0,
                                   days_detected = 0,
                                   hours_detected = 0,
                                   date_last_detected = NA,
                                   days_at_liberty = 0)) %>% #look for solution to replace all columns but one with NA
  relocate(release_loc, .after = days_at_liberty)

for(i in 1:nrow(tagged_animal_info)){
  tag_serial_num <- tagged_animal_info$tag_serial_number[i]
  tagged_animal_info$days_at_liberty[i] <- max(tagged_animal_info$days_at_liberty[i],
                                               dst_summary$days_at_liberty[dst_summary$tag_serial_number == tag_serial_num])
}

# tagged_animal_info %>% View()

# return rate

# sharks_returned <- tagged_animal_info %>% #dplyr::filter(n_detect > 1) %>%
#   mutate(returned = lubridate::year(tagged_animal_info$date_last_detected) - lubridate::year(tagged_animal_info$release_date_time)) %>%
#   dplyr::filter(returned == 1) %>% nrow()
# 
# 
# tagged_animal_info %>% dplyr::filter(!date_last_detected %>% is.na()) %>% nrow()
# 
#       
           
           # base::difftime(lubridate::year(release_date_time), lubridate::year(date_last_detected),))
         
         
# unique(c(lubridate::year(release_date_time), lubridate::year(date_last_detected))) %>% nrow()) %>% View()

# table 4: detections summary ####


detections_month <- detections_tempdepth_daynight %>%
  group_by(month_name) %>%
  summarise(n_detect = n(),
            percent = round(100 * (n_detect / nrow(detections_tempdepth_daynight)), digits = 2),
            # n_ind = tag_serial_number %>% unique() %>% length(),
            n_male = tag_serial_number[sex == "m"] %>% unique() %>% length(),
            n_female = tag_serial_number[sex == "f"] %>% unique() %>% length())
   



detections_month <- detections_tempdepth_daynight %>% 
  group_by(month_name, sex) %>%
  summarise(n_detect = n(),
            n_ind = tag_serial_number %>% unique() %>% length()) %>%
  pivot_wider(names_from = sex, values_from = c(n_detect, n_ind), values_fill = 0)

# to check how many females and males are detected in which month and area
detections_month_area <- detections_tempdepth_daynight %>% 
  group_by(month_name, sex, area) %>%
  summarise(n_detect = n(),
            n_ind = tag_serial_number %>% unique() %>% length()) %>%
  pivot_wider(names_from = sex, values_from = c(n_detect, n_ind), values_fill = 0)

# include capture method y or n?

# table 6: lm moonphase ####

lm_sum_308_depthmedian_day_night <- tibble(daytime = c("day", "night"),
                           intercept = c(-lm_308_day_depthmedian_moonfraq$coefficients[[1]], -lm_308_night_depthmedian_moonfraq$coefficients[[1]]),
                           slope = c(-lm_308_day_depthmedian_moonfraq$coefficients[[2]], -lm_308_night_depthmedian_moonfraq$coefficients[[2]]),
                           adj.r.squared = c(lm_308_day_depthmedian_moonfraq$adj.r.squared, lm_308_night_depthmedian_moonfraq$adj.r.squared))

lm_sum_321_depthmedian_day_night <- tibble(daytime = c("day", "night"),
                                       intercept = c(-lm_321_day_depthmedian_moonfraq$coefficients[[1]], -lm_321_night_depthmedian_moonfraq$coefficients[[1]]),
                                       slope = c(-lm_321_day_depthmedian_moonfraq$coefficients[[2]], -lm_321_night_depthmedian_moonfraq$coefficients[[2]]),
                                       adj.r.squared = c(lm_321_day_depthmedian_moonfraq$adj.r.squared, lm_321_night_depthmedian_moonfraq$adj.r.squared))

# table 7 and 8: acoustic detections summary ####

detections_sum_station <- detections_tempdepth_daynight %>% 
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                station_name = gsub("bpns-", "", station_name),
                station_name = factor(station_name, levels = station_names_order)) %>% 
  mutate(month_year = as.POSIXct(paste0(lubridate::year(date_time), '-', lubridate::month(date_time), '-16')),
         month_year_chr = paste0(lubridate::year(date_time), '-', date_time %>% format("%b"))) %>%
  group_by(station_name, month_year, sex) %>%
  # group_by(area, month_year, sex) %>%
  summarise(n_detect = n(),
            area = area %>% unique(),
            month_year_chr = month_year_chr %>% unique(), 
            n_ind = tag_serial_number %>% unique() %>% length())


detections_OG102019 <- detections_tempdepth_daynight %>% 
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                tag_serial_number = tag_serial_number %>%
                  stringr::str_trunc(width = 3, side = "left", ellipsis = "")
                # station_name = gsub("bpns-", "", station_name),
                # station_name = factor(station_name, levels = station_names_order)
  ) %>% 
  # mutate(month_year = as.POSIXct(paste0(lubridate::year(date_time), '-', lubridate::month(date_time), '-17')),
  #        month_year_chr = paste0(lubridate::year(date_time), '-', date_time %>% format("%b"))) %>%
  dplyr::filter(station_name == "OG10",
                lubridate::year(date_time) == "2019") %>%
  group_by(tag_serial_number, date) %>%
  summarise(n_detect = n(),
            depth_median = median(parameter[sensor_type == "pressure"]),
            sex = sex %>% unique()) %>%
  mutate(date = date %>% as.POSIXct(tz = "utc")) %>%
  ungroup()

# table 9: RI of females in OG10 in 2019 ####

detections_OG102019 <- detections_tempdepth_daynight %>% 
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                tag_serial_number = tag_serial_number %>%
                  stringr::str_trunc(width = 3, side = "left", ellipsis = "")
                # station_name = gsub("bpns-", "", station_name),
                # station_name = factor(station_name, levels = station_names_order)
  ) %>% 
  # mutate(month_year = as.POSIXct(paste0(lubridate::year(date_time), '-', lubridate::month(date_time), '-17')),
  #        month_year_chr = paste0(lubridate::year(date_time), '-', date_time %>% format("%b"))) %>%
  dplyr::filter(station_name == "OG10",
                lubridate::year(date_time) == "2019") %>%
  group_by(tag_serial_number, date) %>%
  summarise(n_detect = n(),
            depth_median = median(parameter[sensor_type == "pressure"]),
            sex = sex %>% unique()) %>%
  mutate(date = date %>% as.POSIXct(tz = "utc")) %>%
  ungroup()

OG10_2019_RI <- detections_OG102019 %>% group_by(tag_serial_number) %>%
  summarise(RI = n() / (base::difftime(detections_OG102019$date %>% max(), detections_OG102019$date %>% min(), unit = "days") %>% as.numeric))

# ggplot(data = OG10_2019_RI, aes(x = tag_serial_number, y = RI)) +
#   geom_point()

# table 10. short term DST summary ####

short_DST_summary <- masterias_depth_temp %>% 
  dplyr::filter(!tag_serial_number %in% c("1293308", "1293321")) %>% 
  group_by(tag_serial_number) %>%
  summarise(depth_max = depth_m %>% max(na.rm = T),
            depth_min = depth_m %>% min(na.rm = T)) %>%
  left_join(tagged_animal_info %>% dplyr::select(tag_serial_number, days_at_liberty), by = "tag_serial_number") %>%
  ungroup() %>%
  summarise(mean_max_depth = depth_max %>% mean(na.rm = T),
            sd_max_depth = depth_max %>% sd(na.rm = T),
            mean_days_at_liberty = days_at_liberty %>% mean(na.rm = T),
            sd_days_at_liberty = days_at_liberty %>% sd(na.rm = T))

short_DST_summary %>% View()

# statistical tests ####

## depth per day and night ####

daynight_depth_308_ttest <- stats::t.test(x = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 1) %>% dplyr::select(depth_min_sgolay), 
                                          y = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 0) %>% dplyr::select(depth_min_sgolay), 
                                          alternative = "greater")

daynight_depth_321_ttest <- stats::t.test(x = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 1) %>% dplyr::select(depth_min_sgolay), 
                                          y = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 0) %>% dplyr::select(depth_min_sgolay), 
                                          alternative = "greater")

# ggplot(mapping = aes(x = date, y = -depth_median_sgolay)) +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 1), colour = "red") +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 0), colour = "blue") +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 1), colour = "orange", mapping = aes(x = date, y = -depth_min_sgolay)) +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293308", day == 0), colour = "lightblue", mapping = aes(x = date, y = -depth_min_sgolay))


# ggplot(mapping = aes(x = date, y = -depth_median_sgolay)) +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 1), colour = "red") +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 0), colour = "blue") +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 1), colour = "orange", mapping = aes(x = date, y = -depth_min_sgolay)) +
#   geom_line(data = long_dst_daynight %>% dplyr::filter(tag_serial_number == "1293321", day == 0), colour = "lightblue", mapping = aes(x = date, y = -depth_min_sgolay))

# 6. change periods ####

var_list <- c("depth_median_sgolay", "depth_max_sgolay", "depth_min_sgolay")

get_change_periods <- function(rulsif_result, var = var_list, tag_serial_num_short, all_data, time_vector = "date"){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  # # add rulsif_result$step length days of data from the last day
  # pad_data_end <- tibble(date = seq(from = all_data$date %>% max() + lubridate::days(1), to = (all_data$date %>% max()) + lubridate::days(rulsif_result$step), by = "day")) %>%
  #   mutate(depth_median_sgolay = (mean(all_data$depth_median_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5),
  #          depth_max_sgolay = (mean(all_data$depth_max_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5),
  #          depth_min_sgolay = (mean(all_data$depth_min_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5))
  # 
  # pad_data_start <- tibble(date = seq(from = all_data$date %>% min() - lubridate::days(rulsif_result$step), to = (all_data$date %>% min()) - lubridate::days(1), by = "day")) %>%
  #   mutate(depth_median_sgolay = (mean(all_data$depth_median_sgolay[1:10]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5),
  #          depth_max_sgolay = (mean(all_data$depth_max_sgolay[1:10]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5),
  #          depth_min_sgolay = (mean(all_data$depth_min_sgolay[1:10]) + rnorm(n = rulsif_result$step)) %>% signal::sgolayfilt(p = 1, n = 5))
  # 
  # # pad data
  # all_data <- all_data %>%
  #   full_join(pad_data_start, by = join_by(date, depth_median_sgolay, depth_max_sgolay, depth_min_sgolay), multiple = "all") %>%
  #   full_join(pad_data_end, by = join_by(date, depth_median_sgolay, depth_max_sgolay, depth_min_sgolay), multiple = "all") %>% 
  #   arrange(date)
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  var_df <- all_data %>% dplyr::select(var %>% all_of()) 
  var_df <- -var_df
  
  # change_points
  c_points <- rulsif_result$change_points %>% 
    as.data.frame() %>% 
    `colnames<-`("r_num") %>%
    mutate(c_point = TRUE)
  
  df_c_points <- dates %>% 
    mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
    left_join(c_points, by = "r_num") %>%
    dplyr::filter(c_point == TRUE) %>%
    dplyr::select(date) %>%
    dplyr::mutate(week = date %>% lubridate::week(),
                  year = date %>% lubridate::year(),
                  CP_period = 1) %>%
    mutate(week_diff = (week - dplyr::lag(week, default = week[1])) %>% abs())
  
  for(i in 2:nrow(df_c_points)){
    if(df_c_points$week_diff[i] <= 1){
      df_c_points$CP_period[i] <- df_c_points$CP_period[i-1]
    }else{df_c_points$CP_period[i] <- df_c_points$CP_period[i-1] + 1}
  }
  
  df_c_points_week <- df_c_points %>% 
    # dplyr::ungroup() %>%
    dplyr::group_by(CP_period) %>%
    dplyr::mutate(start_date = min(date),
                  end_date = max(date)) %>%
    dplyr::select(CP_period, start_date, end_date) %>%
    # mutate(CP_period = CP_period %>% as.factor()) %>%
    distinct()%>%
    mutate(duration = base::difftime(end_date + lubridate::days(1), start_date, units = "days") %>% 
             as.numeric()) %>%
    dplyr::filter(duration > 3) %>% #only keep the periods that last more than 3 days
    ungroup() %>%
    dplyr::mutate(CP_period_new = 1:n(), #make new CP_periods count
                  step = rulsif_result$step) %>%
    dplyr::relocate(CP_period_new, .before = CP_period)
  
  return(df_c_points_week)
  
}

# tag 308 ####
rulsif_308_table_2_5percent <- get_change_periods(rulsif_result = rulsif_308_res_2_5percent, 
                         tag_serial_num_short = "308",
                         all_data = long_dst_date)

rulsif_308_table_5percent <- get_change_periods(rulsif_result = rulsif_308_res_5percent, 
                                                  tag_serial_num_short = "308",
                                                  all_data = long_dst_date)

rulsif_308_table_10percent <- get_change_periods(rulsif_result = rulsif_308_res_10percent, 
                                                tag_serial_num_short = "308",
                                                all_data = long_dst_date)

# tag 321 ####
rulsif_321_table_2_5percent <- get_change_periods(rulsif_result = rulsif_321_res_2_5percent, 
                                                  tag_serial_num_short = "321",
                                                  all_data = long_dst_date)

rulsif_321_table_5percent <- get_change_periods(rulsif_result = rulsif_321_res_5percent, 
                                                tag_serial_num_short = "321",
                                                all_data = long_dst_date)

rulsif_321_table_10percent <- get_change_periods(rulsif_result = rulsif_321_res_10percent, 
                                                 tag_serial_num_short = "321",
                                                 all_data = long_dst_date)

# check normality of depths
# 
# depth_308 <- masterias_depth_temp %>% ungroup() %>% filter(tag_serial_number == "1293308", ) %>% dplyr::select(depth_m) %>% pull()
# depth_308_shapiro_normality <- shapiro.test(depth_308)$p.value


# save tables #####
save_data(data = dst_summary, folder = tables_path)
save_data(data = tagged_animal_info, folder = tables_path)
save_data(data = release_locations, folder = tables_path)
save_data(data = abbreviations_list, folder = tables_path)
save_data(data = detections_month, folder = tables_path)
save_data(data = short_DST_summary, folder = tables_path)

save_data(data = lm_sum_308_depthmedian_day_night, folder = tables_path)
save_data(data = lm_sum_321_depthmedian_day_night, folder = tables_path)

save_data(data = detections_sum_station, folder = tables_path)
save_data(data = detections_OG102019, folder = tables_path)
save_data(data = OG10_2019_RI, folder = tables_path)

save_data(data = rulsif_308_table_2_5percent, folder = tables_path)
save_data(data = rulsif_308_table_5percent, folder = tables_path)
save_data(data = rulsif_308_table_10percent, folder = tables_path)

save_data(data = rulsif_321_table_2_5percent, folder = tables_path)
save_data(data = rulsif_321_table_5percent, folder = tables_path)
save_data(data = rulsif_321_table_10percent, folder = tables_path)

# statistical tests

save_data(data = daynight_depth_308_ttest, folder = tables_path)
save_data(data = daynight_depth_321_ttest, folder = tables_path)



# detections_tempdepth_daynight %>% dplyr::filter(area %in% c("WS1", "WS2"), sex == "f") %>% dplyr::select(tag_serial_number) %>% unique() %>% nrow()
