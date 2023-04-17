# Script to generate figures for the thesis manuscript

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scales)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()

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




# plots ####

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


# save all plots ad .pdf and .png ####
#To Do

# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".pdf"), plot = dst_plot, width = 18, height = 12, units = "cm")
# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".png"), plot = dst_plot, width = 18, height = 12, units = "cm")

