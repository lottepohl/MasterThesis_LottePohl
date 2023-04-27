# Script to generate figures for the thesis manuscript

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()

## set path were all figures are saved ####
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")


# set plot theme ####

thesis_theme <- ggplot2::theme(
  plot.title = element_text(family = "serif", size = 12, face = "bold"),
  plot.subtitle = element_text(family = "serif", size = 12),
  axis.title = element_text(family = "serif", size = 12),
  axis.text = element_text(family = "serif", size = 10),
  legend.title = element_text(family = "serif", size = 12),
  legend.text = element_text(family = "serif", size = 10),
  # plot.background = element_blank()#,
  panel.background = element_blank(),
  # panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_line(color = "gray70", linetype = "solid"),
  panel.grid.minor = element_line(color = "gray90", linetype = "dashed"),
)

# Set the theme as the default for all plots
ggplot2::theme_set(thesis_theme)

# plot functions ####

## 1. plot raw depthlogs ####
plot_dst_raw_depthlog <- function(data, time_vector, tag_serial_number_short){
  if((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300){ # make different choices for longterm and short term dsts
    date_breaks <- "6 weeks"
    data <- data %>% filter(row_number() %% 5 == 0)
    angle <- 30
  }else{
    date_breaks <- "1 week"
    angle <- 0
  }
  # date_breaks <- ifelse((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300, "6 weeks", "1 week")
  # angle <- ifelse((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300, 50, 0)
  dst_plot <- ggplot2::ggplot(data = data) + geom_point(aes(x = .data[[time_vector]], y = -depth_m), size = 0.5) + 
    # scale_y_continuous(limits = c(-80, 5), breaks = seq(-70, 0, by = 10)) +
    # scale_y_continuous(expand = c((-data$depth_m %>% min()) - 2, (-data$depth_m %>% max()) + 2)) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = "%b %d") + #, %y
    theme(axis.text.x = element_text(angle = angle, hjust = 0.5)) + #
    labs(x = "date", y = "depth in m") #title = paste0("tag ", tag_serial_number_short), 
  return(dst_plot)
}

## 2. plot autocorrelation ####

plot_dst_autocorrelation <- function(acf_df){
  acf_plot <- ggplot(data = acf_df, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "autocorrelation", x = "lag in days") #, title = "tag 308 (female), daily median depth roll3"
  return(acf_plot)
}

## 3. plot summary statistics ####

plot_summary_stats <- function(data_depth, data_DVM, tag_serial_num){
  
  ggplot(data = data_depth %>% ungroup() %>% 
           filter(tag_serial_number == tag_serial_num) %>% 
           mutate(t_days = t_days %>% as.numeric())) +
    geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
             aes(x = date_24hcycle, y = 3, fill = "DVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
    geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
             aes(x = date_24hcycle, y = 3, fill = "rDVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
    geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
             aes(x = date_24hcycle, y = 3, fill = "nVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
    geom_line(aes(x = date, y = -depth_median_roll3, colour = "median")) +
    geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "range"), alpha = 0.2) +
    geom_line(aes(x = date, y = -depth_median_change_roll3, colour = "median change")) + # %>% abs()
    scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d") + #, %y
    theme(axis.text.x = element_text(angle = 30, hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "date", y = "depth (change) in m") +  
    scale_colour_manual(name = "", values = c("median" = "black", "range" = "transparent", "change of range" = "black", "median change" = "purple",
                                              "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #
    scale_fill_manual(name = "", values = c("range" = "transparent", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
                                            "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  theme(legend.position = "bottom",
        legend.box = "horizontal")
  
}

# ggplot(data = long_dst_date %>% ungroup() %>% 
#          filter(tag_serial_number == "1293308") %>% 
#          mutate(t_days = t_days %>% as.numeric())) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "DVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "rDVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "nVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
#   geom_line(aes(x = date, y = -depth_median_change_roll3, colour = "daily median change")) + # %>% abs()
#   scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d") + #, %y
#   theme(axis.text.x = element_text(angle = 30, hjust = 0.5)) +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(x = "date", y = "depth (change) in m") + #title = 'Tag 308 (female)', 
#   scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "daily median change" = "purple",
#                                             "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of daily median change" = "lightblue", "daily median change raw" = "orange",
#                                             "change of daily median change abs" = "darkgreen", "change of daily range abs" = "purple")) + #
#   scale_fill_manual(name = "", values = c("daily range" = "transparent", #"daily median" = "black", "change of daily range" = "black", "daily median change" = "darkblue",
#                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) #+
# # theme(legend.position = "bottom",
# #       legend.box = "horizontal")
# 
# p_t_308_depth_median_range_change_ribbon #%>% ggplotly()

## 4. plot fft ####

plot_fft <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05){
  periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + 
    geom_line(aes(x = period, y = spec), colour = "black") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  return(periodogram)
}

## 5. plot wavelet results ####
plot_wavelet <- function(wt_df, type = c("power", "significance", "power_log"),
                             # y_breaks = c(4, 8, 16, 32, 64, 128),
                             # x_breaks = c("000", "100", "200", "300", "400", "500"),
                             date = TRUE){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  
  # transform dates ####
  wt_df$date <- wt_df$date %>% as.POSIXct(tz = "UTC")
  # # x axis labels
  # ifelse(date %>% isTRUE(),
  #        x_breaks <- c(wt_df$date[1], wt_df$date[(1/5) * n_data], wt_df$date[(2/5) * n_data], wt_df$date[(3/5) * n_data],
  #                      wt_df$date[(4/5) * n_data], wt_df$date[(5/5) * n_data])
  # ,
  # x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
  # 
  #plot
  ifelse(date %>% isTRUE(),
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = date, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.65) +
                  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "date", y = "period in hours", fill = "log2(power)") #+
                # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = significance),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "significance") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       ,
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date, y = period, fill = power),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "power") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       
                )
         )
         ,
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = t, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.65) +
                  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "date", y = "period in hours", fill = "log2(power)") #+
                # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = t, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = significance),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "significance") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       ,
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = t, y = period, fill = power),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "power") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                )
         )
  )
  return(plot)
}

## 6. plot Depth Change Points ####

plot_rulsif_scores <- function(rulsif_result, thresh = 0.95, all_data, tag_serial_num_short, time_vector = "date"){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  # # pad dates with step length 
  # pad_data_start <- tibble(date = seq(from = all_data$date %>% min() - lubridate::days(rulsif_result$step), to = (all_data$date %>% min()) - lubridate::days(1), by = "day"))
  # pad_data_end <- tibble(date = seq(from = all_data$date %>% max() + lubridate::days(1), to = (all_data$date %>% max()) + lubridate::days(rulsif_result$step), by = "day"))
  # dates <- rbind(pad_data_start, dates, pad_data_end) %>% arrange(date)
  
  row_diff <- (((nrow(dates) - length(rulsif_result$scores)) / 2) %>% floor())
  # row_diff <- rulsif_result$step
  # row_diff <- 1
  
  # scores 
  scores <- rulsif_result$scores %>% 
    as.data.frame() %>% 
    `colnames<-`("score") %>%
    mutate(r_num = seq(from = row_diff,
                       to = row_diff + length(rulsif_result$scores) - 1,
                       by = 1))
  
  df_scores <- dates %>% 
    mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
    left_join(scores, by = "r_num")
  
  # plots 
  p_scores <- ggplot(data = df_scores, aes(x = date, y = score)) +
    # geom_hline(aes(yintercept = (df_scores$score %>% max(na.rm = T)) * 0.9)) +
    geom_ribbon(aes(ymin = (score %>% max(na.rm = T)) * thresh,
                    ymax = score %>% max(na.rm = T)),
                # fill = "black", alpha = 0.2) +
                fill = "yellow", alpha = 0.25) +
    geom_line(colour = "black") + 
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "rPE score")
  
  # p_scores
  
  return(p_scores)
}

# plot_rulsif_data <- function(rulsif_result, var = "depth_median", tag_serial_num_short, all_data, time_vector = "date"){

#   all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
# 
#   dates <- all_data %>% dplyr::select(time_vector %>% all_of())
# 
#   var_df <- all_data %>% dplyr::select(var %>% all_of()) %>%
#     `colnames<-`("var_name")
# 
#   # if var contains median, min, max or mean, then inverse it to have depths plotted negatively
#   if( ( grep("(median|mean|max|min)", var) %>% length() ) > 0){
#     var_df <- var_df %>%
#       mutate(var_name = -var_name)
#   }
# 
#   # change_points
#   c_points <- rulsif_result$change_points %>%
#     as.data.frame() %>%
#     `colnames<-`("r_num") %>%
#     mutate(c_point = TRUE)
# 
#   df_c_points <- dates %>%
#     mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
#     left_join(c_points, by = "r_num") %>%
#     dplyr::filter(c_point == TRUE) %>%
#     dplyr::select(date) %>%
#     dplyr::mutate(week = date %>% lubridate::week(),
#                   year = date %>% lubridate::year(),
#                   CP_period = 1) %>%
#     mutate(week_diff = (week - dplyr::lag(week, default = week[1])) %>% abs())
# 
#   for(i in 2:nrow(df_c_points)){
#     if(df_c_points$week_diff[i] <= 1){
#       df_c_points$CP_period[i] <- df_c_points$CP_period[i-1]
#     }else{df_c_points$CP_period[i] <- df_c_points$CP_period[i-1] + 1}
#   }
# 
#   df_c_points_week <- df_c_points %>%
#     # dplyr::ungroup() %>%
#     dplyr::group_by(CP_period) %>%
#     dplyr::mutate(start_date = min(date),
#                   end_date = max(date)) %>%
#     dplyr::select(CP_period, start_date, end_date) %>%
#     # mutate(CP_period = CP_period %>% as.factor()) %>%
#     distinct()
# 
#   # df_c_points <- df_c_points %>%
#   #   left_join
# 
#   # plots
#   p_data <- ggplot() +
#     geom_rect(data = df_c_points_week, aes(xmin = start_date, xmax = end_date, fill = CP_period %>% as.factor(),
#                                            ymin = -Inf, ymax = Inf),
#               alpha = 0.3) +
#     geom_vline(data = df_c_points, aes(xintercept = date, colour = CP_period %>% as.factor()), alpha = 1) +
#     geom_line(aes(x = dates$date, y = var_df$var_name)) +
#     scale_y_continuous(expand = c(0,0)) +
#     labs(x = "", y = "depth in m", fill = "CP period", colour = "CP period") +
#     # theme(legend.position = "bottom",
#     #       legend.box = "horizontal")
#     theme(legend.position="bottom", legend.direction="horizontal", legend.box.margin = margin())
# 
#   p_data
#   return(p_data)
# 
# }

plot_rulsif_data_ribbon <- function(rulsif_result, var = var_list, tag_serial_num_short, all_data, time_vector = "date"){
  
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
    distinct()
  
  # plots 
  p_data <- ggplot() +
    geom_ribbon(aes(x = dates$date, 
                    ymin = var_df %>% dplyr::select(contains("max")) %>% pull(),
                    ymax = var_df %>% dplyr::select(contains("min")) %>% pull()),
                alpha = 0.2) +
    geom_rect(data = df_c_points_week, aes(xmin = start_date, xmax = end_date, fill = CP_period %>% as.factor(),
                                           ymin = -Inf, ymax = Inf),
              alpha = 0.3) +
    geom_vline(data = df_c_points, aes(xintercept = date, colour = CP_period %>% as.factor()), alpha = 1) +
    geom_line(aes(x = dates$date, y = var_df %>% dplyr::select(contains("median")) %>% pull())) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "depth in m", fill = "CP period", colour = "CP period") +
    # theme(legend.position = "bottom",
    #       legend.box = "horizontal")
    theme(legend.position="bottom", legend.direction="horizontal")
  
  # p_data
  return(p_data)
  
}


# plot_all_rulsif_data <- function(rulsif_result, var_list, tag_serial_num_short, all_data){
  plots <- list()
  for(variable in var_list){
    plot <- plot_rulsif_data(rulsif_result = rulsif_result, 
                             all_data = all_data,
                             tag_serial_num_short = tag_serial_num_short,
                             var = variable)
    plots[[variable]] <- plot
    # assign(paste0("p_", variable, "_", tag_serial_num_short, "_rulsif"), plot)
  }
  return(plots)
}


# plots ####

## 0. basic acoustic detections/dst plots ####

p_length_sex <- ggplot(data = tagged_animal_info) +
  geom_boxplot(aes(x = sex, y = length1)) +
  # geom_text(aes(x = group, y = max(value) + 0.2, label = round(max(value), 2)), 
  #           size = 3, position = position_dodge(width = 0.75)) +
  geom_text(data = tagged_animal_info %>% group_by(sex) %>% summarise(n = n()),
            aes(x = sex, y = 98, label = paste0("n =  ", n)), angle = 0, family = "serif") +
  labs(x = "sex", y = "total length in cm")

save_data(data = p_length_sex, folder = plot_path)
  
## 1. raw depthlogs ####

p_dst_raw_295 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293295"),
                              time_vector = "date_time",
                              tag_serial_number_short = "295")
# p_dst_raw_295
save_data(data = p_dst_raw_295, folder = plot_path)

p_dst_raw_308 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "308")
# p_dst_raw_308
save_data(data = p_dst_raw_308, folder = plot_path)

p_dst_raw_321 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "321")
# p_dst_raw_321
save_data(data = p_dst_raw_321, folder = plot_path)

## 2. autocorrelation ####

p_acf_308 <- plot_dst_autocorrelation(acf_308_df)
# p_acf_308
save_data(data = p_acf_308, folder = plot_path)

p_acf_321 <- plot_dst_autocorrelation(acf_321_df)
# p_acf_321
save_data(data = p_acf_321, folder = plot_path)

## 3. fft ####

p_fft_295 <- plot_fft(fft_result = fft_295, 
                 tag_serial_number_short = "295")
# p_fft_295
save_data(data = p_fft_295, folder = plot_path)

p_fft_308 <- plot_fft(fft_result = fft_308, 
                      tag_serial_number_short = "308")
# p_fft_308
save_data(data = p_fft_308, folder = plot_path)

p_fft_321 <- plot_fft(fft_result = fft_321, 
                      tag_serial_number_short = "321")
# p_fft_321
save_data(data = p_fft_321, folder = plot_path)


## 4. summary statistics ####

p_308_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      data_DVM = masterias_DVM_sum_day,
                                      tag_serial_num = "1293308")
# p_308_sum_stats
save_data(data = p_308_sum_stats, folder = plot_path)

p_321_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      data_DVM = masterias_DVM_sum_day,
                                      tag_serial_num = "1293321")
# p_321_sum_stats
save_data(data = p_321_sum_stats, folder = plot_path)


## 5. wavelet results ####
### tag 308 ####
p_308_wavelet_depth_median_roll3 <- plot_wavelet(wt_df = wt_df_308_mediandepth_roll3,
                                                 type = "power_log") 

# p_308_wavelet_depth_median_roll3
save_data(data = p_308_wavelet_depth_median_roll3, folder = plot_path)

p_308_wavelet_depth_median_change_roll3 <- plot_wavelet(wt_df = wt_df_308_mediandepth_change_roll3,
                                                        type = "power_log") 

# p_308_wavelet_depth_median_change_roll3
save_data(data = p_308_wavelet_depth_median_change_roll3, folder = plot_path)

p_308_wavelet_maxdepth_change_roll3 <- plot_wavelet(wt_df = wt_df_308_maxdepth_change_roll3,
                                                 type = "power_log") 

# p_308_wavelet_maxdepth_change_roll3
save_data(data = p_308_wavelet_maxdepth_change_roll3, folder = plot_path)

p_308_wavelet_depth_min <- plot_wavelet(wt_df = wt_df_308_mindepth,
                                                 type = "power_log") 

# p_308_wavelet_depth_min
save_data(data = p_308_wavelet_depth_min, folder = plot_path)

### tag 321 ####

p_321_wavelet_depth_median_roll3 <- plot_wavelet(wt_df = wt_df_321_mediandepth_roll3,
                                                 type = "power_log") 

# p_321_wavelet_depth_median_roll3
save_data(data = p_321_wavelet_depth_median_roll3, folder = plot_path)

p_321_wavelet_depth_median_change_roll3 <- plot_wavelet(wt_df = wt_df_321_mediandepth_change_roll3,
                                                 type = "power_log") 

# p_321_wavelet_depth_median_change_roll3
save_data(data = p_321_wavelet_depth_median_change_roll3, folder = plot_path)


p_321_wavelet_maxdepth_change_roll3 <- plot_wavelet(wt_df = wt_df_321_maxdepth_change_roll3,
                                                    type = "power_log") 

# p_321_wavelet_maxdepth_change_roll3
save_data(data = p_321_wavelet_maxdepth_change_roll3, folder = plot_path)

p_321_wavelet_depth_min <- plot_wavelet(wt_df = wt_df_321_mindepth,
                                        type = "power_log") 

# p_321_wavelet_depth_min
save_data(data = p_321_wavelet_depth_min, folder = plot_path)


## 6. Change Point Detections ####

var_list <- c("depth_median_sgolay", "depth_max_sgolay", "depth_min_sgolay")

### tag 308 ####
# step = 2.5 %
p_308_scores_rulsif_2_5percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_2_5percent,
                                                     all_data = long_dst_date,
                                                     tag_serial_num_short = "308",
                                                     thresh = 0.95)
save_data(data = p_308_scores_rulsif_2_5percent, folder = plot_path)
p_308_ribbon_rulsif_2_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_2_5percent,
                                                          all_data = long_dst_date,
                                                          var = var_list,
                                                          tag_serial_num_short = "308")
save_data(data = p_308_ribbon_rulsif_2_5percent, folder = plot_path)
# grid.arrange(p_308_ribbon_rulsif_2_5percent, p_308_scores_rulsif_2_5percent, ncol = 1)

# step = 5 %
p_308_scores_rulsif_5percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_5percent,
                                                     all_data = long_dst_date,
                                                     tag_serial_num_short = "308",
                                                     thresh = 0.95)
save_data(data = p_308_scores_rulsif_5percent, folder = plot_path)
p_308_ribbon_rulsif_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_5percent,
                                                          all_data = long_dst_date,
                                                          var = var_list,
                                                          tag_serial_num_short = "308")
save_data(data = p_308_ribbon_rulsif_5percent, folder = plot_path)
# grid.arrange(p_308_ribbon_rulsif_5percent, p_308_scores_rulsif_5percent, ncol = 1)


# step = 10 %
p_308_scores_rulsif_10percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_10percent,
                                                   all_data = long_dst_date,
                                                   tag_serial_num_short = "308",
                                                   thresh = 0.95)
save_data(data = p_308_scores_rulsif_10percent, folder = plot_path)
p_308_ribbon_rulsif_10percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_10percent,
                                                        all_data = long_dst_date,
                                                        var = var_list,
                                                        tag_serial_num_short = "308")
save_data(data = p_308_ribbon_rulsif_10percent, folder = plot_path)
# grid.arrange(p_308_ribbon_rulsif_10percent, p_308_scores_rulsif_10percent, ncol = 1)

# step = 15 %
# p_308_scores_rulsif_15percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_15percent,
#                                                     all_data = long_dst_date,
#                                                     tag_serial_num_short = "308",
#                                                     thresh = 0.95)
# p_308_ribbon_rulsif_15percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_15percent,
#                                                          all_data = long_dst_date,
#                                                          var = var_list,
#                                                          tag_serial_num_short = "308")
# grid.arrange(p_308_ribbon_rulsif_15percent, p_308_scores_rulsif_15percent, ncol = 1)


### tag 321 ####
# step = 2.5 %
p_321_scores_rulsif_2_5percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_2_5percent,
                                                     all_data = long_dst_date,
                                                     tag_serial_num_short = "321",
                                                     thresh = 0.95)
save_data(data = p_321_scores_rulsif_2_5percent, folder = plot_path)
p_321_ribbon_rulsif_2_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_2_5percent,
                                                          all_data = long_dst_date,
                                                          var = var_list,
                                                          tag_serial_num_short = "321")
save_data(data = p_321_ribbon_rulsif_2_5percent, folder = plot_path)
# grid.arrange(p_321_ribbon_rulsif_2_5percent, p_321_scores_rulsif_2_5percent, ncol = 1)

# step = 5 %
p_321_scores_rulsif_5percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_5percent,
                                                   all_data = long_dst_date,
                                                   tag_serial_num_short = "321",
                                                   thresh = 0.95)
save_data(data = p_321_scores_rulsif_5percent, folder = plot_path)
p_321_ribbon_rulsif_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_5percent,
                                                        all_data = long_dst_date,
                                                        var = var_list,
                                                        tag_serial_num_short = "321")
save_data(data = p_321_ribbon_rulsif_5percent, folder = plot_path)
# grid.arrange(p_321_ribbon_rulsif_5percent, p_321_scores_rulsif_5percent, ncol = 1)


# step = 10 %
p_321_scores_rulsif_10percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_10percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "321",
                                                    thresh = 0.95)
save_data(data = p_321_scores_rulsif_10percent, folder = plot_path)
p_321_ribbon_rulsif_10percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_10percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "321")
save_data(data = p_321_ribbon_rulsif_10percent, folder = plot_path)
# grid.arrange(p_321_ribbon_rulsif_10percent, p_321_scores_rulsif_10percent, ncol = 1)
# step = 15 %
# p_321_scores_rulsif_15percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_15percent,
#                                                     all_data = long_dst_date,
#                                                     tag_serial_num_short = "321",
#                                                     thresh = 0.95)
# p_321_ribbon_rulsif_15percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_15percent,
#                                                          all_data = long_dst_date,
#                                                          var = var_list,
#                                                          tag_serial_num_short = "321")
# grid.arrange(p_321_ribbon_rulsif_15percent, p_321_scores_rulsif_15percent, ncol = 1)

# save all plots ad .pdf and .png ####
#To Do

# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".pdf"), plot = dst_plot, width = 18, height = 12, units = "cm")
# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".png"), plot = dst_plot, width = 18, height = 12, units = "cm")

