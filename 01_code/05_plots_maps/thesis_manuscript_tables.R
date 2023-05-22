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

# include capture method y or n?

# table 5: dst summary ####

dst_summary <- masterias_depth_date %>% 
  dplyr::group_by(tag_serial_number) %>%
  summarise(release_date = min(date) - lubridate::days(8),
            death_date = date %>% max(),
            time_at_liberty = base::difftime(release_date, death_date, units = "days")%>% abs() %>% as.numeric(),
            depth_min_total = min(depth_min),
            depth_max_total = max(depth_max),
            temp_min_total = min(temp_min),
            temp_max_total = min(temp_max),
            date_depth_max = date[which.max(depth_max)],
            date_depth_min = date[which.min(depth_min)],
            date_temp_max = date[which.max(temp_max)],
            date_temp_min = date[which.min(temp_min)]) %>%
  arrange(time_at_liberty) %>%
  left_join(masterias_info %>%
              dplyr::select(tag_serial_number, sex, recapture_date_time), 
            by = "tag_serial_number") %>%
  mutate(release_date = release_date %>% as.Date(),
         death_date = death_date %>% as.Date(),
         recapture_date_time = recapture_date_time %>% as.Date())
# include max and min depth, and max and min temp?

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

# save tables #####
save_data(data = dst_summary, folder = tables_path)
save_data(data = tagged_animal_info, folder = tables_path)
save_data(data = release_locations, folder = tables_path)
save_data(data = abbreviations_list, folder = tables_path)
save_data(data = detections_month, folder = tables_path)

save_data(data = rulsif_308_table_2_5percent, folder = tables_path)
save_data(data = rulsif_308_table_5percent, folder = tables_path)
save_data(data = rulsif_308_table_10percent, folder = tables_path)

save_data(data = rulsif_321_table_2_5percent, folder = tables_path)
save_data(data = rulsif_321_table_5percent, folder = tables_path)
save_data(data = rulsif_321_table_10percent, folder = tables_path)


