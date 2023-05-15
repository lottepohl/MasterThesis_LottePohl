# Script with plots for the master symposium on May 12, 2023


# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(pracma)
library(sf)
library(leaflet)
library(leafem)
library(leaflet.extras)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
## functions ####
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
source(paste0(dir_path, "/01_code/06_functions/compute_wavelettransform.R"))
## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
source(paste0(dir_path, "/01_code/02_load_data/load_environmental_data.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_human_activities.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R"))
# source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
# source(paste0(dir_path, "/01_code/02_load_data/load_bathy.R"))


## set path were all figures are saved ####
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

# Set the theme ####
presentation_theme <- ggplot2::theme(
  plot.title = element_text(size = 13, face = "bold"),
  plot.subtitle = element_text(family = "serif", size = 13),
  axis.title = element_text(family = "serif", size = 13),
  axis.text = element_text(family = "serif", size = 11),
  legend.title = element_text(family = "serif", size = 13),
  legend.text = element_text(family = "serif", size = 11),
  # plot.background = element_blank()#,
  panel.background = element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent"), # Add this line
  # panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_line(color = "gray40", linetype = "solid"),
  panel.grid.minor = element_line(color = "gray60", linetype = "dashed"),
)
ggplot2::theme_set(presentation_theme) # or theme minimal

# functions for plotting ####

plot_raw_depth <- function(depthlog = masterias_depth_temp, tag_serial_num){
  data <- depthlog %>% filter(tag_serial_number == tag_serial_num)
  max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(10)) %>% as.POSIXct()
  min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(10)) %>% as.POSIXct()
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    # geom_point(aes(color = temp_c), size = 0.5) + 
    geom_line() +
    scale_x_datetime(date_breaks = "1 month",
                     # date_breaks = "1 day",
                     # date_minor_breaks = "1 week",
                     date_labels = "%b'%y",
                     expand = c(0,0),
                     limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "Depth in m", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 
  
}

plot_depth_subset_points <- function(depthlog = masterias_depth_temp, tag_serial_num, start_date_chr, end_date_chr){
  data <- depthlog %>% filter(tag_serial_number == tag_serial_num,
                              lubridate::date(date_time) %>% between(as.POSIXct(start_date_chr), as.POSIXct(end_date_chr)),
                              row_number() %% 1 == 0)
  # max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(10)) %>% as.POSIXct()
  # min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(10)) %>% as.POSIXct()
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    geom_point(size = 1.5) + # aes(colour = lubridate::hour(date_time)),%>% as.factor()
    # geom_line() +
    scale_x_datetime(date_breaks = "1 day",
                     date_minor_breaks = "12 hours",
                     date_labels = "%d %b", #'%y
                     expand = c(0,0)
                     # ,limits = c(min_date, max_date)
                     ) +
    # theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
    scale_y_continuous(expand = c(0,0), limits = c(-20,0)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    labs(x = "", y = "Depth in m", color = "Hour of the day") #+ #title = paste0("tag ", tag_serial_number_short), 
  
}


plot_summary_stats <- function(data_depth, tag_serial_num){
  data <- data_depth %>% ungroup() %>% 
    filter(tag_serial_number == tag_serial_num) %>% 
    mutate(t_days = t_days %>% as.numeric())
  max_date <- max(data$date) + lubridate::days(10)
  min_date <- min(data$date) -lubridate::days(10)
  ggplot(data = data) +
    geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "daily depth range"), alpha = 0.75) +
    geom_line(aes(x = date, y = -depth_median_sgolay, colour = "daily median depth"), linewidth = 1) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + #angle = 30
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "Depth in m") +  
    scale_colour_manual(name = "", values = c("daily median depth" = "black", "daily depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
                                              "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
    scale_fill_manual(name = "", values = c("daily depth range" = "lightgrey", "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #"range" = "grey", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
    
    theme(legend.position = "bottom",
          legend.box = "horizontal") +
    scale_x_datetime(
      # date_minor_breaks = "2 weeks", # make vector with 15.1., 15.2. to take as minor breaks
      date_breaks = "1 month",
      # date_minor_breaks = "1 month",
      # date_breaks = "2 months",
      date_labels = "%b'%y"
      ,expand = c(0,0),
      limits = c(min_date, max_date)
    ) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.25))
}

## fft ####

calc_fft <- function(depth_log, sample_freq){
  # `depth_log` contains the columns `depth_m` and `date_time`.
  
  # prepare/format data
  depth_log <- depth_log %>% 
    # make time vector
    mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric()) %>% 
    #filter out NAs
    filter(!is.na(depth_m))
  
  # define tmax and n
  tmax <- depth_log$t %>% max()
  n <- depth_log$t %>% length()
  
  # mit nullen auffuellen bis naechste n^2
  next_power <- 2^ceiling(log2(n))
  depth_log_padded <- c(depth_log$depth_m, rep(0, next_power - n))
  
  # compute the frequency spectrum
  spec <- abs(fft(depth_log_padded))^2
  
  # compute the frequency vector
  # freq <- seq(0, length(spec) - 1) * (1/sample_freq) / length(spec)
  freq <- seq(0, length(spec) - 1) * (sample_freq) / length(spec)
  
  # make result dataframe
  result_fft <- cbind(spec, freq) %>% as.data.frame() %>% 
    # calculate period
    mutate(period = 1 / freq)
  
  return(result_fft)
}

# plot periodogram 
plot_periodogram <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05, path = plot_path){
  # todo: get local rule or set values for period upper and lower lim
  periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + 
    geom_line(aes(x = period, y = spec), colour = "black", linewidth = 1) + #theme_bw() +
    scale_y_continuous(expand = c(0,0)) +
    # theme_bw(base_size = 12) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".png"), plot = periodogram, width = 18, height = 12, units = "cm")
  return(periodogram)
}

plot_periodogram_biweekly <- function(fft_result, tag_serial_number_short, period_upperlim = 500, period_lowerlim = 0.05, path = plot_path){
  # todo: get local rule or set values for period upper and lower lim
  periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + 
    geom_line(aes(x = period, y = spec), colour = "black", linewidth = 1) + #theme_bw() +
    scale_y_continuous(expand = c(0,0)) +
    # theme_bw(base_size = 12) +
    scale_x_continuous(expand = c(0,0), breaks = seq(0, period_upperlim, by = 25)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".png"), plot = periodogram, width = 18, height = 12, units = "cm")
  return(periodogram)
}

# calc and plot subset periodograms 

fft_calc_plot <- function(depth_log, tag_serial_num_short, start_date, end_date, sample_frequency){
  fft_res <- calc_fft(depth_log = depth_log %>% filter(tag_serial_number == paste0("1293", tag_serial_num_short),
                                                       lubridate::date(date_time) %>% 
                                                         between(as.POSIXct(start_date), as.POSIXct(end_date))),
                      sample_freq = sample_frequency)
  
  pgram <- plot_periodogram(fft_result = fft_res, 
                            tag_serial_number_short = tag_serial_num_short)
  pgram %>% return()
}

plot_wavelet <- function(wt_df, type = c("power", "significance", "power_log"),
                         # y_breaks = c(4, 8, 16, 32, 64, 128),
                         # x_breaks = c("000", "100", "200", "300", "400", "500"),
                         date = TRUE){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  
  # transform dates 
  wt_df$date <- wt_df$date %>% as.POSIXct(tz = "UTC")
  
  max_date <- max(wt_df$date) + lubridate::days(10)
  min_date <- min(wt_df$date) -lubridate::days(10)
  
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
                  # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_x_datetime(date_breaks = "1 month",
                                   # date_minor_breaks = "1 month",
                                   date_labels = "%b'%y",
                                   expand = c(0,0),
                                   limits = c(min_date, max_date)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "", y = "Period in days", fill = "log2(Power)") +
                theme(legend.position = "bottom",
                      legend.box = "horizontal") +
                theme(axis.text.x = element_text(angle = 15, hjust = 0.25))
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


# DST ####
## raw logs ####
### depthlog 

# tag 295

tag295_raw_depth <- plot_raw_depth(masterias_depth_temp, tag_serial_num = "1293295")
tag295_raw_depth #ggplotly()

# tag 308

tag308_raw_depth <- plot_raw_depth(masterias_depth_temp, tag_serial_num = "1293308")

tag308_raw_depth %>% ggplotly()

gridExtra::grid.arrange(tag308_raw_depth, tag321_raw_depth, nrow = 1)

### templog 
tag308_raw_temp <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"), aes(x = date_time, y = temp_c)) + 
  geom_line(colour = "black") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25))+
  labs(x = "", y = "Temperature in °C", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag308_raw_temp 

gridExtra::grid.arrange(tag308_raw_depth, tag308_raw_temp, ncol = 1)

# tag 321
tag321_raw_depth <- plot_raw_depth(masterias_depth_temp, tag_serial_num = "1293321")
tag321_raw_depth %>% ggplotly()
### templog 
tag321_raw_temp <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = temp_c)) + 
  geom_line(colour = "black") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  labs(x = "", y = "Temperature in °C", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag321_raw_temp

gridExtra::grid.arrange(tag308_raw_depth, tag321_raw_depth, tag308_raw_temp, tag321_raw_temp, ncol = 2)


## daily summaries ####

p_308_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293308")
p_308_sum_stats

p_321_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293321")
p_321_sum_stats

gridExtra::grid.arrange(
  # tag308_raw_depth, tag321_raw_depth, 
  p_308_sum_stats, p_321_sum_stats
                        ,ncol = 2
  # , nrow = 2
                        )

## depthlog subsets ####

p_308_depth_summer <- plot_depth_subset_points(depthlog = masterias_depth_temp, tag_serial_num = "1293308", start_date_chr = "2018-09-21", end_date_chr = "2018-09-24")
p_308_depth_summer

p_321_depth_summer <- plot_depth_subset_points(depthlog = masterias_depth_temp, tag_serial_num = "1293321", start_date_chr = "2018-09-15", end_date_chr = "2018-09-18")
p_321_depth_summer


## fft ####

pgram_308_summer18 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                   start_date = "2018-08-02",
                                   end_date = "2018-09-25",
                                   sample_frequency = 30)
pgram_308_summer18

pgram_308_summer19 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                  start_date = "2019-05-11",
                                  end_date = "2019-08-02",
                                  sample_frequency = 30)
pgram_308_summer19

pgram_321_summer18 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                    start_date = "2018-08-03",
                                    end_date = "2018-10-15",
                                    sample_frequency = 30)
pgram_321_summer18

gridExtra::grid.arrange(p_308_depth_summer,p_321_depth_summer, 
                        pgram_308_summer18, pgram_321_summer18)

### winter fft long period ####

fft_308_winter <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
                                          lubridate::date(date_time) %>% 
                                            between(as.POSIXct("2018-11-15"), as.POSIXct("2019-03-15"))), sample_freq  = 30)

pgram_308_winter <- plot_periodogram_biweekly(fft_result = fft_308_winter, tag_serial_number_short = "308", period_upperlim = 400, period_lowerlim = 0.05, path = plot_path)
pgram_308_winter# %>% ggplotly()

fft_321_winter <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293321",
                                                                       lubridate::date(date_time) %>% 
                                                                         between(as.POSIXct("2018-11-15"), as.POSIXct("2019-03-15"))), sample_freq  = 30)

pgram_321_winter <- plot_periodogram_biweekly(fft_result = fft_321_winter, tag_serial_number_short = "321", period_upperlim = 400, period_lowerlim = 0.05, path = plot_path)
pgram_321_winter %>% ggplotly()


## wavelet ####

# dates
dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

## tag 308
wt_308_depthmedian_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                              filter(tag_serial_number == "1293308") %>%
                                              dplyr::select(depth_median_sgolay),
                                            dt = 1,
                                            factor_smallest_scale = 2)
wt_df_308_depthmedian_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthmedian_sgolay)

p_wt_308_depthmedian_sgolay <- plot_wavelet(wt_df = wt_df_308_depthmedian_sgolay,
                                                 type = "power_log") 
p_wt_308_depthmedian_sgolay

## tag 321
wt_321_depthmedian_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293321") %>%
                                               dplyr::select(depth_median_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_321_depthmedian_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthmedian_sgolay)

p_wt_321_depthmedian_sgolay <- plot_wavelet(wt_df = wt_df_321_depthmedian_sgolay,
                                            type = "power_log") 

p_wt_321_depthmedian_sgolay

gridExtra::grid.arrange(p_wt_308_depthmedian_sgolay, p_wt_321_depthmedian_sgolay, ncol = 2)


# acoustic detections ####
## summaries ####
detections_sum_sex <- detections_tempdepth_daynight %>% group_by(sex) %>% summarise(n_detect = n(), 
                                                              n_detect_perc = 100 * (n_detect / detections_tempdepth_daynight %>% nrow()))

detections_sum_area <- detections_tempdepth_daynight %>% group_by(area) %>% summarise(n_detect = n(), 
                                                              n_detect_perc = 100 * (n_detect / detections_tempdepth_daynight %>% nrow()))
detections_sum_area

## abacus plot ####

masterias_info %>% 
  mutate(tag_serial_num_short = tag_serial_number %>% 
           stringr::str_trunc(width = 3, side = "left", ellipsis = ""))

p_abacus <- ggplot() + # %>% mutate(tag_serial_number = reorder(tag_serial_number, masterias_info$release_date_time))
  # geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
  #            aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number, shape = sex), stroke = 3, colour = "black", size = 4) +
  
  geom_point(data = detections_tempdepth_daynight,
             aes(x = date_time, y = tag_serial_number, colour = area), size = 3) + #, pch = sex
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number, shape = sex), stroke = 3, colour = "black", size = 4) +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 month",
                   date_labels = "%b'%y"
                   # ,expand = c(0,0)
                   ) +
  scale_y_discrete(labels = masterias_info %>% 
                     dplyr::filter(n_detect > 1) %>% 
                     mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T) %>% 
                                                              as.character()) %>% 
                     dplyr::select(tag_serial_number) %>% 
                     pull() %>% 
                     stringr::str_trunc(width = 3, side = "left", ellipsis = "")
                     ) +
  scale_color_manual(values = c("black", "#ed7d31","#3483ac")) + #, "#34b3bb"
  # scale_shape_manual(values = c(2, 4)) +
  scale_shape_manual(values = c("\u2640", "\u2642")) +
  labs(x = "", y = "tag serial nr.", colour = "receiver array", shape = "tagging date") +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25))

p_abacus

## heatmap ####

station_names_order <- c("OG10", "DL7", "DL9", "DL12", "OGDL", "SP3", "GVWSP","TRAWL", "WN2", "W6", "W7", #WS1
                         "borssele", "11", "13", "PVTSS", "4", #WS2
                         "Birkenfels","CPowerReefballs", "G-88", "Grafton", "LottoBuoy", "Nauticaena", "S4", "VG2", "W1", "WK12", "Westhinder" #BPNS
                         ) %>% base::rev()

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
            n_ind = tag_serial_number %>% unique() %>% length()) %>%
  mutate(n_detect = ifelse(n_detect > 1500, 1000, n_detect))

p_detections_heatmap <- ggplot(data = detections_sum_station %>% filter(!area == "BPNS", sex == "f"), #
                               aes(x = month_year, y = station_name, fill = n_detect, colour = n_detect)) + #, colour = n_detect
  # geom_tile(linewidth = 0.75) +
  geom_tile(linewidth = 1) +
  # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "black", angle = 0, family = "sans", fontface = "bold", size = 5) + #, colour = "grey"
  geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "white", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  # facet_grid(vars(sex), scales="free_y") +
  scale_fill_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  scale_colour_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  # scale_colour_manual(name = "", values = c("# individuals" = "grey")) +
  # scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
  #                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
  scale_x_datetime(date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   date_labels = "%b'%y"
                   ,expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + #labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), 
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  labs(x = "", y = "receiver station", fill = "# detections", colour = "# detections") #, colour = "# detections"

p_detections_heatmap #%>% ggplotly()

gridExtra::grid.arrange(p_abacus, p_detections_heatmap, ncol = 1)

### heatmap detail ####

detections_OG102019 <- detections_tempdepth_daynight %>% 
  dplyr::mutate(station_name = gsub("ws-", "", station_name)
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
            sex = sex %>% unique()
            ) 

# p <- ggplot(data = detections_OG102019 %>% filter(sensor_type == "pressure"), aes(x = date_time, y = -parameter)) +
#   geom_line(aes(colour = tag_serial_number)) +
#   geom_point(aes(colour = lubridate::hour(date_time) %>% as.factor()), size = 2) + #aes(pch = sex), 
#   labs(x = "", y = "depth in m", colour = "tag serial number")
# 
# p %>% ggplotly()

p <- ggplot(data = detections_OG102019, aes(x = date, y = -depth_median, colour = tag_serial_number)) +
  geom_line() +
  geom_point(aes(size = n_detect)) + #aes(pch = sex), 
  labs(x = "", y = "depth in m", colour = "tag serial number") #, size = "# detections per day"

p %>% ggplotly()

### tag 299 ####
p_299_2019_WS <- ggplot(data = detections_tempdepth_daynight %>% 
         dplyr::mutate(station_name = gsub("ws-", "", station_name),
                       station_name = factor(station_name, levels = station_names_order)) %>%
         filter(tag_serial_number == "1293299",
                lubridate::year(date_time) == "2019",
                sensor_type == "pressure"),
       aes(x = date_time, y = station_name)) +
  scale_color_viridis_c() +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 week",
                   date_labels = "%b'%y",
                   limits = c(as.POSIXct("2019-04-15"), as.POSIXct("2019-11-01"))
                   # ,expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  geom_point(aes(colour = -parameter), size = 4) +
  labs(x = "", y = "Receiver Station", colour = "depth", title = "tag 299 (female)")
p_299_2019_WS

### tag 297 #### 
p_297_2019_WS <- ggplot(data = detections_tempdepth_daynight %>% 
                          dplyr::mutate(station_name = gsub("ws-", "", station_name),
                                        station_name = gsub("bpns-", "", station_name),
                                        station_name = factor(station_name, levels = station_names_order)) %>%
                          filter(tag_serial_number == "1293297",
                                 lubridate::year(date_time) == "2019",
                                 sensor_type == "pressure"),
                        aes(x = date_time, y = station_name)) +
  scale_color_viridis_c() +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 week",
                   date_labels = "%b'%y",
                   limits = c(as.POSIXct("2019-04-15"), as.POSIXct("2019-11-01"))
                   # ,expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  geom_point(aes(colour = -parameter), size = 4) +
  labs(x = "", y = "Receiver Station", colour = "depth", title = "tag 297 (female)")
p_297_2019_WS

### tag 308 ####
p_308_2019_WS <- ggplot(data = detections_tempdepth_daynight %>% 
                          dplyr::mutate(station_name = gsub("ws-", "", station_name),
                                        station_name = gsub("bpns-", "", station_name),
                                        station_name = factor(station_name, levels = station_names_order)) %>%
                          filter(tag_serial_number == "1293308",
                                 lubridate::year(date_time) == "2019",
                                 sensor_type == "pressure"),
                        aes(x = date_time, y = station_name)) +
  scale_color_viridis_c() +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 week",
                   date_labels = "%b'%y",
                   limits = c(as.POSIXct("2019-04-15"), as.POSIXct("2019-11-01"))
                   # ,expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  geom_point(aes(colour = -parameter), size = 4) +
  labs(x = "", y = "Receiver Station", colour = "depth", title = "tag 308 (female)")
p_308_2019_WS

gridExtra::grid.arrange(p_299_2019_WS, p_297_2019_WS, p_308_2019_WS, ncol = 1)

### combine DST adn acoustic ####

p_308_DST_acoustic <- ggplot() +
  geom_line(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
                                                   lubridate::date(date_time) > as.POSIXct("2019-04-30", tz = "utc")), 
            aes(x = date_time, y = -depth_m)) +
  geom_point(data = detections_tempdepth_daynight %>%
               dplyr::mutate(station_name = gsub("ws-", "", station_name),
                             station_name = gsub("bpns-", "", station_name),
                             station_name = factor(station_name, levels = station_names_order %>% base::rev())) %>%
               dplyr::filter(tag_serial_number == "1293308",
                             lubridate::year(date_time) == "2019",
                             sensor_type == "pressure"),
             aes(x = date_time, y = -parameter, colour = station_name), size = 2.5) +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 week",
                   date_labels = "%b'%y"
                   # ,expand = c(0,0)
                   ) +
  labs(x = "", y = "Depth in m", colour = "Receiver station")

p_308_DST_acoustic# %>% ggplotly()

# maps ####

# bathy_belgium <- bathy_belgium %>% filter(dplyr::between(latitude, 51.343, 51.485) & dplyr::between(longitude, 3.455, 3.77))

labels_latlng <- tibble(name = c("North Sea", "Hurd deep", "English Channel", "France", "Belgium", "the Netherlands"),
                        lat = c(51.92, Hurd_deep$latitude, 50.5, 49.8, 50.86, 52.5),
                        lng = c(3.75, Hurd_deep$longitude, -1.6, 2.7, 3.84, 4.3))

receiver_stations <- deployments %>% 
  group_by(station_name) %>% 
  summarise(deploy_latitude = mean(deploy_latitude),
            deploy_longitude = mean(deploy_longitude)) %>%
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                station_name = gsub("bpns-", "", station_name)) %>%
  filter(deploy_latitude %>% between(50, 52),
         deploy_longitude %>% between(1.5, 4.5))

ws_stations <- receiver_stations %>% 
  filter(deploy_latitude %>% between(51.3, 51.5),
         deploy_longitude %>% between(3.4, 4.05)) %>%
  mutate(area = ifelse(deploy_longitude < 3.6, "WS1", 
                       ifelse(deploy_longitude < 3.9, "WS2", "WS3")) %>%
           as.factor())

ws_rectangles <- ws_stations %>% group_by(area) %>%
  summarise(lat1 = min(deploy_latitude),
            lat2 = max(deploy_latitude),
            lng1 = min(deploy_longitude),
            lng2 = max(deploy_longitude))

# outlines <- rbind(Belgium, Netherlands, North_sea, English_channel)
outlines <- rbind(Belgium, Netherlands)

# color palette 
# pal_dst <- colorNumeric(palette = "magma", domain = data$date_time) 

pal_areas <- colorFactor(palette = c("#ed7d31","#344e9c", "#34b3bb"), domain = ws_stations$area)
# col_scale_areas <- c("#ed7d31","#344e9c", "#34b3bb")

# for emodnet layer
emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
cite_emodnet <- "<a href='https://emodnet.ec.europa.eu'>EMODnet</a>"
attr(cite_emodnet, "class") <- c("html", "character")

## westerschelde ####
map1_overview <- leaflet::leaflet(#) %>%
  options = leafletOptions(zoomControl = FALSE,
                           # minZoom = 8, maxZoom = 8,
                           dragging = T
  )
) %>%
  # ADD BASELAYERS #
  # addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  ## BPNS
  addPolygons(data = BPNS, color = "darkgrey",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  # OUTLINES BOUNDARIES #
  addPolygons(data = outlines, color = "grey",
              weight = 1,
              opacity = 1.0,
              # label = "North Sea",
              fillOpacity = 0,
              labelOptions = labelOptions(noHide = T, textOnly = F, offset = c(-170, 500), permanent = T)) %>% #
  # ADD RELEASED TAGS #
  addCircleMarkers(data = release_locations,
                   lat = ~lat,
                   lng = ~lng,
                   label = ~release_loc) %>%
  # addCircleMarkers(data=masterias_info,
  #                  lat = ~release_latitude,
  #                  lng = ~release_longitude,
  #                  weight= 0,# increase if black circle wanted
  #                  color = "black",
  #                  fillOpacity = 0.5,
  #                  radius = 6,
  #                  fillColor = "grey",
  #                  label = ~paste0("tag ", str_trunc(tag_serial_number, 3, "left", ellipsis = ""), " (release: ", release_dateyear, "), ", sex),
  #                  group = "<span style=color:grey>released tags</span>") %>%
addRectangles(data = ws_rectangles,
  lng1 = ~lng1 - 0.025, lat1 = ~lat1 - 0.025, lng2 = ~lng2 + 0.025, lat2 = ~lat2 + 0.025,
  fillOpacity = 0.2, weight = 2, color = ~pal_areas(area),
  group = "areas") %>%
# ADD STATIONS #
addCircleMarkers(data = receiver_stations,
                 lat = ~deploy_latitude,
                 lng = ~deploy_longitude,
                 fillColor = "black",
                 weight = 0,
                 # color = "white",
                 # weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
                 radius = 3,
                 opacity = 1,
                 fillOpacity = 1,
                 # labelOptions = labelOptions(noHide = T, textOnly = T,
                 #                             style = list("font-style" = "bold",
                 #                                          "font-weight" = "bold",
                 #                                          "font-size" = "12px"),
                 #                             offset = c(10, 0)),
                 # label = ~station_name,
                 # highlightOptions = highlightOptions(bringToFront = TRUE),
                 group = "stations") %>%
  addCircleMarkers(data = masterias_stations %>% # WS1
                     dplyr::mutate(station_name = gsub("ws-", "", station_name),
                                   station_name = gsub("bpns-", "", station_name)) %>%
                     filter(area == "WS1"),
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   # fillColor = "",
                   weight = 0,
                   color = "black",
                   # weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
                   radius = 0,
                   opacity = 1,
                   fillOpacity = 1,
                   labelOptions = labelOptions(noHide = T, textOnly = T,
                                               style = list(
                                                 "font-style" = "bold",
                                                            "font-weight" = "bold",
                                                            "font-size" = "10px"),
                                               offset = c(5, 0)),
                   # label = ~station_name,
                   group = "stations") %>%
  addCircleMarkers(data = masterias_stations %>% #WS2
                     dplyr::mutate(station_name = gsub("ws-", "", station_name),
                                   station_name = gsub("bpns-", "", station_name)) %>%
                     filter(area == "WS2"),
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   # fillColor = "",
                   weight = 0,
                   color = "black",
                   # weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
                   radius = 0,
                   opacity = 1,
                   fillOpacity = 1,
                   labelOptions = labelOptions(noHide = T, textOnly = T,
                                               style = list(
                                                 "font-style" = "bold",
                                                            "font-weight" = "bold",
                                                 "font-size" = "10px"),
                                               offset = c(-5, 0)),
                   # label = ~station_name,
                   group = "stations") %>%
  # addCircleMarkers(data = ws_stations,
  #                  lat = ~deploy_latitude,
  #                  lng = ~deploy_longitude,
  #                  fillColor = ~pal_areas(area),
  #                  weight = 2,
  #                  color = "black",
  #                  # weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
  #                  radius = 4,
  #                  opacity = 1,
  #                  fillOpacity = 1,
  #                  # labelOptions = labelOptions(noHide = T, textOnly = T,
  #                  #                             style = list("font-style" = "bold",
  #                  #                                          "font-weight" = "bold",
  #                  #                                          "font-size" = "12px"),
  #                  #                             offset = c(20, 0)),
  #                  # label = ~station_name,
  #                  # highlightOptions = highlightOptions(bringToFront = TRUE),
  #                  group = "stations") %>%

# GRATICULE #
addSimpleGraticule(
  # interval = 0.5
  interval = 0.25                 
                   ) %>%

  addCircleMarkers(data = labels_latlng,
                   lat = ~lat,
                   lng = ~lng,
                   weight = 2,
                   opacity = 0,
                   fillOpacity = 0,
                   label = ~name,
                   labelOptions = labelOptions(noHide = T, 
                                               textOnly = T,
                                               style = list("font-style" = "bold",
                                                            "font-weight" = "bold",
                                                            "font-size" = "12px"))) %>%
  # ADD CONTROLS #
  # leafem::addMouseCoordinates() %>%
  setView(3.5, 51.6, zoom = 8.5) %>%
  addLegend(position = "bottomright",
            opacity = 1,
            colors = c("black", "#ed7d31","#344e9c", "#34b3bb"),
            labels = c("receiver station", "Westerschelde 1", "Westerschelde 2", "Westerschelde 3"),
            # title = "Legend",
            # colors = "black",
            # labels = "acoustic receiver station",
            labFormat = labelFormat(textOnly = T)) %>%
  # SCALEBAR #
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, imperial = F))

map1_overview


# addFullscreenControl() %>%
# addLayersControl(position = "topright",
#                  baseGroups = c("EMODnet bathymetry", "satellite", "bathymetry", "OpenStreetMap"),
#                  overlayGroups = c("<span style=color:grey>released tags</span>", "stations", "wrecks, OWFs, cables"),
#                  options = layersControlOptions(collapsed = FALSE)) #%>%
# addPolygons(data = Schelde_boundaries, color = "yellow",
#             weight = 2,
#             opacity = 1.0,
#             fillOpacity = 0) %>%
# addMiniMap(position = "bottomright",
#            width = 100,
#            height = 100,
#            zoomLevelOffset = -3,
#            zoomLevelFixed = T,
#            tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"#providers$Esri.WorldStreetMap)
# ) %>%
# addRasterImage(bathy_belgium_raster, opacity = 1, colors = "Spectral", group = "bathymetry") %>%
# addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "grey", weight = 0, fillOpacity = 0.7, group = "bathymetry") %>% #"#ECE4BF"
# addTiles(group = "OpenStreetMap") %>%
# area rectangles #
# addRectangles( #WS1
#   lng1 = 3.387, lat1 = 51.365, lng2 = 3.577, lat2 = 51.513,
#   fillOpacity = 0.2, weight = 2, color = col_scale_areas[2],
#   group = "areas") %>%
#   addRectangles( #WS2
#     lng1 = 3.656, lat1 = 51.314, lng2 = 3.859, lat2 = 51.447,
#     fillOpacity = 0.2, weight = 2, color = col_scale_areas[3],
#     group = "areas") %>%
# submarine cables #
# addPolylines(data = cables,
#              color = "blue",
#              weight = 1,
#              opacity = 0.6,
#              label = ~name,
#              group = "wrecks, OWFs, cables") %>%
# shipwrecks #
# addCircleMarkers(data = wrecks,
#                  fillColor = "green",
#                  opacity = 0,
#                  fillOpacity = 1,
#                  radius = 0.9,
#                  label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
#                  group = "wrecks, OWFs, cables") %>%
# addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
#                  lng = ~longitude,
#                  lat = ~latitude,
#                  fillColor = "green",
#                  opacity = 0,
#                  fillOpacity = 1,
#                  radius = 0.9,
#                  label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
#                  group = "wrecks, OWFs, cables") %>%
# more bathy #
# addCircleMarkers(data = bathy_belgium, lat = ~latitude, lng = ~longitude,
#                  opacity = 0, fillOpacity = 0,
#                  label = ~paste0(depth_m %>% round(digits = 2), " m")) %>%
# windfarms #
# addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
#             color = "red",
#             weight = 1,
#             opacity = 1,
#             fillOpacity = 0.3,
#             label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
#             group = "wrecks, OWFs, cables") %>%


## overview ####

outlines2 <- rbind(Belgium, North_sea, English_channel)

labels_latlng <- tibble(name = c("North Sea", "Hurd deep", "English Channel", "France", "Belgium", "the Netherlands", "BPNS"),
                        lat = c(52.07, Hurd_deep$latitude, 50.1, 49.8, 50.86, 52.5, 51.5),
                        lng = c(3.75, (Hurd_deep$longitude - 0.25), -1.6, 2.7, 4.5, 8, 3.1))


map2_overview <- leaflet::leaflet(#) %>%
  options = leafletOptions(zoomControl = FALSE,
                           # minZoom = 8, maxZoom = 8,
                           dragging = T
  )
) %>%
  # ADD BASELAYERS #
  # addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  ## BPNS
  addPolygons(data = BPNS, color = "darkgrey",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  # OUTLINES BOUNDARIES #
  addPolygons(data = outlines2, color = "grey",
              weight = 1,
              opacity = 1.0,
              # label = "North Sea",
              fillOpacity = 0,
              labelOptions = labelOptions(noHide = T, textOnly = F, offset = c(-170, 500), permanent = T)) %>%
  addCircleMarkers(data = labels_latlng,
                   lat = ~lat,
                   lng = ~lng,
                   weight = 2,
                   opacity = 0,
                   fillOpacity = 0,
                   label = ~name,
                   labelOptions = labelOptions(noHide = T, 
                                               textOnly = T,
                                               style = list("font-style" = "bold",
                                                            "font-weight" = "bold",
                                                            "font-size" = "12px"))) %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, imperial = F))

map2_overview

## investigaion pupping ####

leaflet() %>% addTiles() %>%
  addCircleMarkers(data = detections_tempdepth_daynight %>% filter(tag_serial_number == "1293299",
                                                                   lubridate::year(date_time) == "2019"),
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   label = ~paste0("date:", date_time, ", sensor: ", parameter, " ", sensor_unit %>% round(digits = 2))) %>%
  addPolylines(data = detections_tempdepth_daynight %>% filter(tag_serial_number == "1293299",
                                                               lubridate::year(date_time) == "2019"),
               lat = ~deploy_latitude,
               lng = ~deploy_longitude)

