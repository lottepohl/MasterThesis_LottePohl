# Script to plot the summary statistics of the dst depth logs

# 0. workspace ####
library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(tidyverse)
library(psdr)
library(ggplot2)
library(StreamMetabolism)
library(suncalc)
library(sf)
library(forcats)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/02_results/dst_summary/")
# source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R"))
source(paste0(dir_path, "/01_code/06_functions/ggplot_geom_split_violin.R"))

# wrangle datasets ####

masterias_DVM_sum_day <- masterias_DVM_sum_day %>%
  group_by(tag_serial_number) %>% 
  mutate(t_days = seq(from = 1, to = n()),
         t_days = sprintf("%03d", t_days %>% as.numeric()),
         t_days = t_days %>% as.numeric()) %>%
  ungroup()

long_dst_date <- long_dst_date %>%
  mutate(t_days = t_days %>% as.numeric())

# all dsts ####

summer_masterias_depth_temp <- masterias_depth_temp_summary %>% 
  filter(month %in% c("07", "08")) %>%
  mutate(hour = date_time %>% lubridate::hour() %>% as.factor())


summer_masterias_depth_daynight <- masterias_depth_daynight %>% 
  filter(month %in% c("07", "08"), dusk == 0 & dawn == 0) #%>%
  # mutate(hour_depth_min = hour_depth_min %>% as.factor(),
  #        hour_depth_max = hour_depth_min %>% as.factor())

summer_masterias_DVM_daily <- masterias_DVM_sum_day %>%
  filter(date_24hcycle %>% lubridate::month() %in% c("7", "8")) %>%
  mutate(day = date_24hcycle %>% lubridate::day(),
         month = date_24hcycle %>% lubridate::month(),
         day = sprintf("%02d", day %>% as.numeric()),
         month = sprintf("%02d", month %>% as.numeric()),
         monthday = paste0(month, "-", day)) %>%
  group_by(monthday, vertical_movement) %>%
  summarise(count = n())

summer_masterias_DVM_weekly <- masterias_DVM_sum_week %>%
  filter(month %in% c("07", "08"))

# p <- ggplot(data = summer_masterias_depth_temp %>% filter(tag_serial_number == "1293295", row_number() %% 5 == 0, dusk == 0 & dawn == 0)) +
#   geom_point(aes(x = time, y = -depth_m, colour = day)) +
#   scale_x_discrete(breaks = 10) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
#   # facet_grid()
# 
# p %>% ggplotly

### depth per hour of the day ####
p_summer_depth_per_hour <- ggplot(data = summer_masterias_depth_temp %>% group_by(hour, tag_serial_number)) +
  geom_boxplot(aes(x = hour, y = -depth_m)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(y = "depth in m", x = "hour of the day", title = "all dst sharks, July & August")

# p_summer_depth_per_hour %>% ggplotly()

### DVM per day ####

p_summer_dvm_daily <- ggplot(data = summer_masterias_DVM_daily, aes(x = monthday, y = count, fill = vertical_movement)) +
  geom_bar(position = "fill", width = 1, stat = "identity") + 
  labs(fill = "vertical movement behaviour", y = "percentage", x = "date", title = "all dst sharks, July & August") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  # ggplot(data = summer_masterias_DVM_daily) +
  # geom_bar(stat = "identity", position = "stack", aes(x = date_24hcycle, fill = vertical_movement))

# p_summer_dvm_daily

### hour at max/min depth ####

p_hour_maxmindepth <- ggplot(data = summer_masterias_depth_daynight) +
  geom_point(aes(x = monthday, y = hour_depth_max, colour = tag_serial_number)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_hour_maxmindepth 

# tag 308 & 321 ####

## raw depth logs ####

p_308_depth_raw <- ggplot(data = masterias_depth_temp_summary %>% 
                            filter(tag_serial_number == "1293308",
                                   row_number() %% 30 == 0 #,vertical_speed_m_min < 5
                                   )) +
  geom_point(aes(x = date_time, y = -depth_m, colour = temp_c)) +
  scale_colour_distiller(palette ="RdYlBu") +
  scale_y_continuous(expand = c(0,0)) +
  labs(tite = "depth log tag 308 (female)", x = "date", y = "depth in m", colour = "temperature in C")+
  theme_dark()

 # p_308_depth_raw #%>% ggplotly()


p_321_depth_raw <- ggplot(data = masterias_depth_temp_summary %>% 
                            filter(tag_serial_number == "1293321",
                                   row_number() %% 30 == 0 #, vertical_speed_m_min < 5
                                   )) +
  scale_y_continuous(expand = c(0,0)) +
  geom_point(aes(x = date_time, y = -depth_m, colour = temp_c)) +
  scale_colour_distiller(palette ="RdYlBu") +
  labs(tite = "depth log tag 321 (male)", x = "date", y = "depth in m", colour = "temperature in C")+
theme_dark()

# p_321_depth_raw # %>% ggplotly()

## ribbon: median depth, max and min ####

p_308_depth_median_range_change_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293308")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "DVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "DVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "rDVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "rDVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "nVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "nVM")) +
  geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
  geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
  # changes
  # geom_line(aes(x = date, y = depth_range_change_roll3 %>% abs(), colour = "change of daily range")) +
  # geom_line(aes(x = date, y = (depth_median_change) + 10, colour = "change of daily median raw")) + # %>% abs()
  geom_line(aes(x = date, y = (depth_median_change_roll3) + 10, colour = "change of daily median")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change2_roll3) + 25, colour = "change of change of daily median")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change2_roll3  %>% abs()) + 25, colour = "change of change of daily median abs")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change_roll3 %>% abs()) + 10, colour = "change of daily range abs")) +
  # settings
  theme_minimal() + 
  labs(title = 'Tag 308 (female)', x = "date", y = "depth in m") + 
  scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of change of daily median" = "lightblue", "change of daily median raw" = "orange",
                                            "change of change of daily median abs" = "darkgreen", "change of daily range abs" = "purple")) +
  scale_fill_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                          "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal")
# scale_fill_manual(name = "", values = c("daily range" = "red"))

# p_308_depth_median_range_change_ribbon #%>% plotly::ggplotly()


# p_308_depth_median_range_change_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293308")) +
#   geom_bar(data = masterias_DVM_sum_day %>%
#              filter(tag_serial_number == "1293308"#, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
#              ),
#            aes(x = date_24hcycle, fill = vertical_movement)) +
#   geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
#   # changes
#   # geom_line(aes(x = date, y = depth_range_change_roll3 %>% abs(), colour = "change of daily range")) +
#   geom_line(aes(x = date, y = (depth_median_change_roll3) + 10, colour = "change of daily median")) + # %>% abs()
#   # settings
#   theme_minimal() + 
#   scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue")) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
#   scale_x_continuous(breaks = unique(long_dst_date$date %>% lubridate::week()), minor_breaks = NULL) +
#   theme_classic() +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_line(linewidth = 0.2, linetype = "dotted"))
# # scale_fill_manual(name = "", values = c("daily range" = "red"))
# 
# p_308_depth_median_range_change_ribbon %>% plotly::ggplotly()


p_t_308_depth_median_range_change_ribbon <- ggplot(data = long_dst_date %>% ungroup() %>% filter(tag_serial_number == "1293308")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "DVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "rDVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "nVM")) +
  geom_line(aes(x = t_days, y = -depth_median_roll3, colour = "daily median")) +
  geom_ribbon(aes(x = t_days, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
  geom_line(aes(x = t_days, y = (depth_median_change_roll3) + 10, colour = "change of daily median")) + # %>% abs()
  theme_minimal() + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(title = 'Tag 308 (female)', x = "t in days", y = "depth in m") + 
  scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of change of daily median" = "lightblue", "change of daily median raw" = "orange",
                                            "change of change of daily median abs" = "darkgreen", "change of daily range abs" = "purple")) +
  scale_fill_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                          "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# p_t_308_depth_median_range_change_ribbon

p_321_depth_median_range_change_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293321")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "DVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "DVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "rDVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "rDVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "nVM"), #, date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = date_24hcycle, fill = "nVM")) +
  geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
  geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
  # changes
  # geom_line(aes(x = date, y = depth_range_change_roll3 %>% abs(), colour = "change of daily range")) +
  # geom_line(aes(x = date, y = (depth_median_change) + 10, colour = "change of daily median raw")) + # %>% abs()
  geom_line(aes(x = date, y = (depth_median_change_roll3) + 10, colour = "change of daily median")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change2_roll3) + 25, colour = "change of change of daily median")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change2_roll3  %>% abs()) + 25, colour = "change of change of daily median abs")) + # %>% abs()
  # geom_line(aes(x = date, y = (depth_median_change_roll3 %>% abs()) + 10, colour = "change of daily range abs")) +
  # settings
  theme_minimal() + 
  labs(title = 'Tag 321 (male)', x = "date", y = "depth in m") + 
  scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of change of daily median" = "lightblue", "change of daily median raw" = "orange",
                                            "change of change of daily median abs" = "darkgreen", "change of daily range abs" = "purple")) +
  scale_fill_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal")
  # scale_fill_manual(name = "", values = c("daily range" = "red"))

# p_321_depth_median_range_change_ribbon #%>% plotly::ggplotly()

p_t_321_depth_median_range_change_ribbon <- ggplot(data = long_dst_date %>% ungroup() %>% filter(tag_serial_number == "1293321")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "DVM"), #, t_days_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "DVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "rDVM"), #, t_days_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "rDVM")) +
  geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321", vertical_movement == "nVM"), #, t_days_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)
           aes(x = t_days, fill = "nVM")) +
  geom_line(aes(x = t_days, y = -depth_median_roll3, colour = "daily median")) +
  geom_ribbon(aes(x = t_days, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
  # changes
  # geom_line(aes(x = t_days, y = depth_range_change_roll3 %>% abs(), colour = "change of daily range")) +
  # geom_line(aes(x = t_days, y = (depth_median_change) + 10, colour = "change of daily median raw")) + # %>% abs()
  geom_line(aes(x = t_days, y = (depth_median_change_roll3) + 10, colour = "change of daily median")) + # %>% abs()
  # geom_line(aes(x = t_days, y = (depth_median_change2_roll3) + 25, colour = "change of change of daily median")) + # %>% abs()
  # geom_line(aes(x = t_days, y = (depth_median_change2_roll3  %>% abs()) + 25, colour = "change of change of daily median abs")) + # %>% abs()
  # geom_line(aes(x = t_days, y = (depth_median_change_roll3 %>% abs()) + 10, colour = "change of daily range abs")) +
  # settings
  theme_minimal() + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(title = 'Tag 321 (male)', x = "t in days", y = "depth in m") + 
  scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of change of daily median" = "lightblue", "change of daily median raw" = "orange",
                                            "change of change of daily median abs" = "darkgreen", "change of daily range abs" = "purple")) +
  scale_fill_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "change of daily median" = "darkblue",
                                          "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  
  theme(legend.position = "bottom",
        legend.box = "horizontal")

# p_t_321_depth_median_range_change_ribbon

# 
# p_321_depth_rangechange <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293321")) +
#   # geom_line(aes(x = date, y = depth_range_roll3, colour = "daily depth range")) +
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily depth range"), alpha = 0.2) +
#   # geom_line(aes(x = date, y = depth_min_change), colour = "red") +
#   # geom_line(aes(x = date, y = depth_max_change), colour = "blue") +
#   geom_line(aes(x = date, y = depth_range_change_roll3 %>% abs(), colour = "change of daily depth range")) +
#   theme_minimal() + 
#   labs(x = "date", y = "depth in m", colour = "median", title = 'Tag 321 (male)') +  
#   scale_colour_manual(name = "",
#                     values = c("change of daily depth range" = "green", "daily depth range" = "black"))
# 
# p_321_depth_rangechange %>% plotly::ggplotly()
# 
# p_longtermtracks_medianchange_321_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293321")) +
#   geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median depth")) +
#   # geom_line(aes(x = date, y = depth_min_change), colour = "red") +
#   # geom_line(aes(x = date, y = depth_max_change), colour = "blue") +
#   geom_line(aes(x = date, y = depth_median_change_roll3 %>% abs(), colour = "change of daily median depth")) +
#   theme_minimal() + 
#   labs(x = "date", y = "depth in m", colour = "median", title = 'depth median, tag 321 (male)') + #title = 'Tag 321 (female)', 
#   scale_colour_manual(name = "",
#                       values = c("change of daily median depth" = "green", "daily median depth" = "black"))
#   # geom_ribbon(aes(x = date, ymin = -depth_max_change_roll3, ymax = -depth_min_change_roll3), alpha = 0.2)
# 
# p_longtermtracks_medianchange_321_ribbon %>% plotly::ggplotly()


## mean depth
# #TODO: add errorbar
# p_longtermdsttracks_mediandepth_weekly <- ggplot(data = masterias_depth_week %>% filter(tag_serial_number %in% c("1293308", "1293321")), aes(x = weekyear, y = -depth_median, fill = tag_serial_number)) + 
#   geom_bar(stat = "identity", position = "stack", width = 1) +
#   labs(title ="Long term dst tracks", x = "week", y = "median depth in m") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_longtermdsttracks_mediandepth_weekly

# ## depth range ####
# p_longtermdsttracks_depthrange_weekly <- ggplot(data = masterias_depth_week %>% filter(tag_serial_number %in% c("1293308", "1293321")), 
#                                                 aes(x = weekyear, y = depth_range_mean, fill = tag_serial_number), alpha(0.5)) + 
#   geom_bar(stat = "identity", position = "stack", width = 1) +
#   labs(title ="Long term dst tracks", x = "week", y = "mean depth range in m") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_longtermdsttracks_depthrange_weekly #%>% plotly::ggplotly()

## mean vertical speed ####

### weekly #### 
# sd is suuuuper big!! don't do weekly sum, only monthly
# p_longtermdsttracks_meandvertspeed_weekly <- ggplot(data = masterias_depth_week %>% filter(tag_serial_number %in% c("1293308")), aes(x = weekyear, y = vertical_speed_mean)) + #, "1293321" + , fill = tag_serial_number
#   geom_bar(stat = "identity", position = "stack", width = 1) +
#   # geom_errorbar(stat = "identity", aes(ymin = vertical_speed_mean - vertical_speed_sd, ymax = vertical_speed_mean + vertical_speed_sd)) +
#   labs(title ="Long term dst tracks", x = "week", y = "mean vertical speed in m/min") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_longtermdsttracks_meandvertspeed_weekly

### monthly #### 
# violin plot
p_308_vertical_speed <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293308"), vertical_speed_m_min <= 0.5), 
                               aes(x = monthyear, y = vertical_speed_m_min, fill = day)) + #, "1293321" 
  geom_violin(colour = "transparent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 308 (female)", y = "vertical speed in m/min", x = "month")

# p_308_vertical_speed %>% ggplotly

# split violin plot
p_308_vertical_speed_splitviolin <- ggplot(masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293308"), vertical_speed_m_min <= 0.5), 
       aes(x = monthyear, y = vertical_speed_m_min, fill = day, colour = day)) + 
  geom_split_violin() +
  labs(title = "Tag 308 (female)", y = "vertical speed in m/min", x = "month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# p_308_vertical_speed_splitviolin


# violin plot
p_321_vertical_speed <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293321"), vertical_speed_m_min <= 0.5) %>% drop_na(), 
                               aes(x = monthyear, y = vertical_speed_m_min, fill = day %>% as.factor())) + #, "1293321" 
  geom_violin(colour = "transparent", position = position_dodge(width = 0.8),   # Set the width of the dodge
              scale = "width") + #, position = 'stack'
  # geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 321 (male)", y = "vertical speed in m/min", x = "month")

# p_321_vertical_speed %>% ggplotly

# split violin plot
p_321_vertical_speed_splitviolin <- ggplot(masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293321"), vertical_speed_m_min <= 0.5) %>% drop_na(), 
                                           aes(x = monthyear, y = vertical_speed_m_min, fill = day, colour = day)) + 
  geom_split_violin() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 321 (male)", y = "vertical speed in m/min", x = "month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# p_321_vertical_speed_splitviolin

# depth plot
p_308_depth_month_splitviolin <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293308"), vertical_speed_m_min <= 0.5), 
                               aes(x = monthyear, y = -depth_m, fill = day, colour = day)) + #, "1293321" 
  geom_split_violin() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 308 (female)", y = "depth in m", x = "month")

# p_308_depth_month_splitviolin

p_321_depth_month_splitviolin <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293321"), vertical_speed_m_min <= 0.5) %>% drop_na(), 
                      aes(x = monthyear, y = -depth_m, fill = day,colour = day)) + #, "1293321" 
  geom_split_violin() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 321 (male)", y = "depth in m", x = "month")

# p_321_depth_month_splitviolin

# todo: for september 2018 (also for 321): look at daily depth distribution (or weekly)

#monthly depth distribution
p_308_depth_monthly <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293308"), vertical_speed_m_min <= 5), aes(x = monthyear, y = -depth_m, fill = day)) + #, "1293321" 
  geom_violin(colour = "transparent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 308 (female)", y = "depth in m", x = "month")

# p_308_depth_monthly %>% ggplotly

p_321_depth_monthly <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293321"), vertical_speed_m_min <= 5), aes(x = monthyear, y = -depth_m, fill = day)) + #, "1293321" 
  geom_violin(colour = "transparent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 321 (male)", y = "depth in m", x = "month")

# p_321_depth_monthly %>% ggplotly

p_308_depth_test <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293308"), vertical_speed_m_min <= 5), aes(x = weekyear, y = -depth_m, fill = day)) + #, "1293321" 
  geom_violin(colour = "transparent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 308 (female)", y = "depth in m", x = "month")

# p_308_depth_test %>% ggplotly

## monthly depth range 
# TODO: noch bisschen aufpeppen
# p_long_dsts_depthrange <- ggplot(data = masterias_depth_month %>% filter(tag_serial_number %in% c("1293308", "1293321")), aes(x = month_name, y = depth_range_mean, fill = tag_serial_number)) + 
#   # geom_point(size = 3) +
#   geom_bar(position = "stack", stat = "identity") +
#   labs(x = "Month", y = "mean daily depth range in m", fill = "Tag serial number") +
#   # geom_errorbar(position = "stack", stat = "identity") +
#   # geom_ribbon(aes(ymin = -(depth_mean - depth_sd), ymax = -(depth_mean + depth_sd)), fill = "grey", alpha = 0.3) +
#   theme_minimal()
# # p_long_dsts_depthrange
# ggsave(filename = paste0(plot_path, "barplot_long_dsts_depthrange.pdf"), plot = p_long_dsts_depthrange, height = 12, width = 16, units = "cm")


# ggplot(masterias_station_month_sex, aes(x = month_name, y = detections_count, fill = area)) +
#   geom_bar(position = "stack", stat = "identity") +

# masterias_depth_month %>% filter(tag_serial_number == "1293308") %>% dplyr::select(depth_range, month, year) %>% View() 


## DVM ####

#### weekly ####
p_tag308_DVM_weekly <- ggplot(data = masterias_DVM_sum_week %>% filter(tag_serial_number == "1293308"), aes(x = weekyear, y = count, fill = vertical_movement)) +
  geom_bar(position = "fill", width = 1, stat = "identity") + 
  labs(fill = "vertical movement behaviour", y = "percentage", x = "Month", title = "tag 308") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) #+
  # scale_x_discrete(labels = masterias_DVM_sum_week$monthyear)
# p_tag308_DVM_weekly %>% ggplotly()

p_tag321_DVM_weekly <- ggplot(data = masterias_DVM_sum_week %>% filter(tag_serial_number == "1293321"), aes(x = weekyear, y = count, fill = vertical_movement)) +
  geom_bar(position = "fill", width = 1, stat = "identity") + 
  labs(fill = "vertical movement behaviour", y = "percentage", x = "week", title = "tag 321") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) #+
  # scale_x_discrete(labels = masterias_DVM_sum_week$monthyear)
# p_tag321_DVM_weekly


# ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293295")) + geom_point(aes(x = date, y = -depth_m, pch = day)) + theme_minimal()



#### monthly ####
p_tag308_DVM_monthly <- ggplot(data = masterias_DVM_sum_month %>% filter(tag_serial_number == "1293308"), aes(x = monthyear, y = count, fill = vertical_movement)) +
  geom_bar(position = "fill", width = 1, stat = "identity") + 
  labs(fill = "vertical movement behaviour", y = "percentage of days with behaviour", x = "month", title = "tag 308") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_tag308_DVM_monthly %>% ggplotly()

# ggsave(filename = paste0(plot_path, "f_308_DVM_monthly.pdf"), plot = p_tag308_DVM_monthly, height = 12, width = 16, units = "cm")


p_tag321_DVM_monthly <- ggplot(data = masterias_DVM_sum_month %>% filter(tag_serial_number == "1293321"), aes(x = monthyear, y = count, fill = vertical_movement)) +
  geom_bar(position = "fill", width = 1, stat = "identity") + 
  labs(fill = "vertical movement behaviour", y = "percentage of days with behaviour", x = "month", title = "tag 321") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_tag321_DVM_monthly

# ggsave(filename = paste0(plot_path, "m_321_DVM_monthly.pdf"), plot = p_tag321_DVM_monthly, height = 12, width = 16, units = "cm")

# migration periods ####

## tag 308 ####

### migration 1 ####

tag_308_migration1_start <- "2018-09-05" %>% as.POSIXct(tz = "UTC") #"2018-09-20" %>% as.POSIXct(tz = "UTC")
tag_308_migration1_end <- "2018-11-01" %>% as.POSIXct(tz = "UTC")

# depth distribution during day and night for this period

p_308_mig1_depth_DVM <- ggplot(data = masterias_depth_temp_summary %>% 
                                 filter(tag_serial_number == "1293308", date %>% between(tag_308_migration1_start, tag_308_migration1_end))) +
  # geom_split_violin(aes(x = date, y = -depth_m, fill = day), colour = "transparent") +
  geom_point(aes(x = date, y = -depth_m - 2, colour = day), alpha = 0.3, size = 2) +
  geom_bar(data = masterias_DVM_sum_day %>%
             filter(tag_serial_number == "1293308", date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)),
           aes(x = date_24hcycle, fill = vertical_movement)) +
  labs(title = "Potential winter 2018 migration of tag 308", y = "depth in m", x = "date") +
  theme_minimal()

# p_308_mig1_depth_DVM %>% ggplotly()


# p_tag308_mig_depth_perday <- ggplot(data = masterias_depth_temp_summary %>%
#                                       filter(tag_serial_number == "1293308", 
#                                              date %>% between(tag_308_migration1_start, tag_308_migration1_end))) +
#   geom_violin(aes(x = weekyear, y = -depth_m, fill = day), colour = "transparent") +
#   theme_minimal()
# p_tag308_mig_depth_perday %>% ggplotly()

## To Do: heatmap with time at depth bins ----------------------------------------------------------

## tag 321 ####

### migration 1 ####

tag_321_migration1_start <- "2018-10-5" %>% as.POSIXct(tz = "UTC") #"2018-09-20" %>% as.POSIXct(tz = "UTC")
tag_321_migration1_end <- "2018-11-10" %>% as.POSIXct(tz = "UTC")

# depth distribution during day and night for this period

p_321_mig1_depth_DVM <- ggplot(data = masterias_depth_temp_summary %>% 
              filter(tag_serial_number == "1293321", date %>% between(tag_321_migration1_start, tag_321_migration1_end))) +
  # geom_split_violin(aes(x = date, y = -depth_m, fill = day), colour = "transparent") +
  geom_point(aes(x = date, y = -depth_m - 2, colour = day), alpha = 0.3, size = 2) +
  geom_bar(data = masterias_DVM_sum_day %>%
             filter(tag_serial_number == "1293321", date_24hcycle %>% between(tag_321_migration1_start, tag_321_migration1_end)),
           aes(x = date_24hcycle, fill = vertical_movement)) +
  labs(title = "Potential winter 2018 migration of tag 321", y = "depth in m", x = "date") +
  theme_minimal()

# p_321_mig1_depth_DVM %>% ggplotly()

### migration 2 ####

tag_321_migration2_start <- "2019-10-01" %>% as.POSIXct(tz = "UTC") #"2018-09-20" %>% as.POSIXct(tz = "UTC")
tag_321_migration2_end <- "2019-11-01" %>% as.POSIXct(tz = "UTC")

# depth distribution during day and night for this period

p_321_mig2_depth_DVM <- ggplot(data = masterias_depth_temp_summary %>% 
                                 filter(tag_serial_number == "1293321", date %>% between(tag_321_migration2_start, tag_321_migration2_end))) +
  # geom_split_violin(aes(x = date, y = -depth_m, fill = day), colour = "transparent") +
  geom_point(aes(x = date, y = -depth_m - 2, colour = day), alpha = 0.3, size = 2) +
  geom_bar(data = masterias_DVM_sum_day %>%
             filter(tag_serial_number == "1293321", date_24hcycle %>% between(tag_321_migration2_start, tag_321_migration2_end)),
           aes(x = date_24hcycle, fill = vertical_movement)) +
  labs(title = "Potential winter 2019 migration of tag 321", y = "depth in m", x = "date") +
  theme_minimal()

# p_321_mig2_depth_DVM %>% ggplotly()


# p_tag308_mig_depth_perday <- ggplot(data = masterias_depth_temp_summary %>%
#                                       filter(tag_serial_number == "1293308", 
#                                              date %>% between(tag_308_migration1_start, tag_308_migration1_end))) +
#   geom_density(aes(x = date, y = -depth_m), stat = "identity", fill = "blue") +
#   # geom_boxplot(aes(x = date, y = -depth_m)) +
#   theme_minimal()
# p_tag308_mig_depth_perday %>% ggplotly()



p_321_vertical_speed <- ggplot(data = masterias_depth_temp_summary %>% filter(tag_serial_number %in% c("1293321"), vertical_speed_m_min <= 0.5) %>% drop_na(), 
                               aes(x = monthyear, y = vertical_speed_m_min, fill = day)) + #, "1293321" 
  geom_violin(colour = "transparent") +
  # geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "Tag 321 (male)", y = "vertical speed in m/min", x = "month")

# horizontal distance ####

p_308_321_hordistance <- ggplot(data = masterias_DVM_sum_day, aes(x = date_24hcycle, y = horizontal_distance_m / 1000, colour = tag_serial_number)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Horizontal distance, all dst data", x = "date", y = "Daily horizontal distance in km")
# p_308_321_hordistance %>% ggplotly()


tag_308_totalhordistance <- masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308") %>% dplyr::select(horizontal_distance_m) %>% sum()
tag_308_totalhordistance <- tag_308_totalhordistance / 1000
tag_321_totalhordistance <- masterias_DVM_sum_day %>% filter(tag_serial_number == "1293321") %>% dplyr::select(horizontal_distance_m) %>% sum()
tag_321_totalhordistance <- tag_321_totalhordistance / 1000

## overall distance travelled ####

locations_firstday <- masterias_DVM_sum_day %>% 
  group_by(tag_serial_number)%>% 
  dplyr::select(tag_serial_number, location_currentday, date_24hcycle) %>% 
  filter(date_24hcycle == min(date_24hcycle)) %>% dplyr::select(location_currentday) %>% 
  purrr::pluck() %>%
  rename(location_firstday = location_currentday) %>%
  ungroup()# %>% filter(tag_serial_number %in% c("1293308", "1293321")) 


# test <- masterias_DVM_sum_day %>% 
#   left_join(locations_firstday, by = "tag_serial_number") %>% 
#   mutate(distance_to_firstlocation_m = st_distance(location_currentday, location_firstday, by_element = TRUE) %>% as.numeric())

total_horizontal_distance <- masterias_DVM_sum_day %>% 
  left_join(locations_firstday, by = "tag_serial_number") %>% 
  mutate(distance_to_firstlocation_m = st_distance(location_currentday, location_firstday, by_element = TRUE) %>% as.numeric()) %>% group_by(tag_serial_number) %>%
  summarise(total_horizontal_distance_km = max(distance_to_firstlocation_m / 1000))

# old ####

# p_longtermtracks_308_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293308")) +
#   geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
#   theme_minimal() + 
#   labs(x = "date", y = "median depth in m") + #title = 'Tag 308 (female)', 
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily min and max"), alpha = 0.2)
# 
# p_longtermtracks_308_ribbon %>% plotly::ggplotly()
# 
# p_longtermtracks_change_308_ribbon <- ggplot(data = long_dst_date %>% filter(tag_serial_number == "1293308")) +
#   geom_line(aes(x = date, y = -depth_median, colour = "daily median")) +
#   # geom_line(aes(x = date, y = depth_min_change), colour = "red") +
#   # geom_line(aes(x = date, y = depth_max_change), colour = "blue") +
#   # geom_line(aes(x = date, y = depth_range_change), colour = "green") +
#   theme_minimal() + 
#   labs(x = "date", y = "depth in m", title = 'Tag 308 (female)') + #title = 'Tag 308 (female)', 
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3), alpha = 0.2) +
#   scale_colour_manual(values = c("daily median", "daily min and max"), name = "")
# 
# p_longtermtracks_change_308_ribbon %>% plotly::ggplotly()


# p_tag308_mig_DVM_daily <- ggplot() + #, y = count
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)),
#              aes(x = date_24hcycle, y = -depth_med_night), stat = "identity", fill = "darkblue") +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", date_24hcycle %>% between(tag_308_migration1_start, tag_308_migration1_end)),
#              aes(x = date_24hcycle, y = -depth_med_day), stat = "identity", fill = "orange") +
#   # geom_bar(position = "fill", width = 1) + #, stat = "identity"
#   labs(fill = "vertical movement behaviour", y = "percentage", x = "date", title = "tag 308") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p_tag308_mig_DVM_daily %>% ggplotly()

# manual separation of overwintering, migrating and oversummering periods of longterm dst tracks

# dst_longterm_periods <- tibble(tag_serial_number = c(rep("1293308", times = 6), rep("1293321", times = 8)),
#                                start_date = c("2018-08-02", "2018-08-22", "2018-10-10", "2019-03-23", "2019-05-13", "2019-08-04", 
#                                               "2018-07-18", "2018-08-21", "2018-10-23", "2019-04-22", "2019-05-11", "2019-08-31", "2019-09-26", "2019-11-17") %>% 
#                                  as.POSIXct(tz = "UTC"),
#                                activity = c("summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "recording_end",
#                                             "summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "winter_migration", "winter_residency", "recording_end")) %>%
#   mutate(year = start_date %>% lubridate::year(),
#          end_date = start_date %>% lead() %>% replace_na("2019-11-16" %>% as.POSIXct(tz = "UTC")),
#          start_date = start_date - lubridate::days(1))

# masterias_depth_daynight_activity <- masterias_depth_daynight %>% 
#   filter(tag_serial_number %in% c("1293321", "1293308"), dusk == 0 & dawn == 0) %>% #filter out dusk and dawn: not used right now
#   left_join(dst_longterm_periods,
#             by = join_by(tag_serial_number == tag_serial_number,
#                          between(x = date, y_lower = start_date, y_upper = end_date))) %>%
#   dplyr::select(!c(start_date, end_date))
# 
# masterias_depth_daynight_activity <- masterias_depth_daynight_activity %>%
#   full_join(masterias_depth_daynight_activity %>% 
#               group_by(tag_serial_number, date) %>% 
#               reframe(depth_mean = sum(depth_mean)/2,
#                       depth_sd = sum(depth_sd)/2,
#                       depth_median = sum(depth_median)/2,
#                       depth_min = min(depth_min),
#                       depth_max = max(depth_max),
#                       depth_range = depth_max - depth_min,
#                       vertical_speed_mean = sum(vertical_speed_mean)/2,
#                       vertical_speed_sd = sum(vertical_speed_sd)/2,
#                       vertical_speed_median = sum(vertical_speed_median)/2,
#                       vertical_speed_min = min(vertical_speed_min),
#                       vertical_speed_max = max(vertical_speed_max),
#                       vertical_speed_range = vertical_speed_max - vertical_speed_min,
#                       activity = unique(activity),
#                       year = unique(year),
#                       day = "full",
#                       dusk = NA,
#                       dawn = NA)) %>%
#   mutate(month = date %>% lubridate::month(),
#          month = sprintf("%02d", month %>% as.numeric()),
#          monthyear = paste0(year, "-", month))

