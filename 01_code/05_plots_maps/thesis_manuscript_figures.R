# Script to generate figures for the thesis manuscript

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(pracma)
library(oce)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_models.R") %>% base::source()

## set path were all figures are saved ####
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")


# set plot theme ####

thesis_theme <- ggplot2::theme(
  plot.title = element_text(family = "serif", size = 11, face = "bold"),
  plot.subtitle = element_text(family = "serif", size = 11),
  axis.title = element_text(family = "serif", size = 11),
  axis.text = element_text(family = "serif", size = 9),
  legend.title = element_text(family = "serif", size = 11),
  legend.text = element_text(family = "serif", size = 9),
  # plot.background = element_blank()#,
  panel.background = element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent"), # Add this line
  # panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_line(color = "gray70", linetype = "solid"),
  panel.grid.minor = element_line(color = "gray90", linetype = "dashed"),
)

# Set the theme as the default for all plots
ggplot2::theme_set(thesis_theme)

# plot functions ####

## 1. plot raw depthlogs ####
plot_raw_depth <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308"){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num, 
    row_number() %% 15 == 0)
  # , date > (min(wt_df$date) + lubridate::days(7))) # cut first week of data off to avoid looking at tagging effect
  
  max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(10)) %>% as.POSIXct()
  min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(10)) %>% as.POSIXct()
  
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    geom_line(linewidth = 0.15) +
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b '%y",
                     expand = c(0,0),
                     limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(-max(data$depth_m),0)) +
    labs(x = "", y = "Depth in m") +
    theme(legend.position = "none",
          legend.box = "horizontal", legend.margin = margin(t = -15))
}

plot_raw_depth_short <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308"){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num)
  # , date > (min(wt_df$date) + lubridate::days(7))) # cut first week of data off to avoid looking at tagging effect
  
  max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(1)) %>% as.POSIXct()
  min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(1)) %>% as.POSIXct()
  
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    geom_line(linewidth = 0.25) +
    scale_x_datetime(date_breaks = "4 days",
                     date_labels = "%d.%m.%y",
                     expand = c(0,0),
                     limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(-max(data$depth_m),0)) +
    labs(x = "", y = "Depth in m") +
    theme(legend.position = "none",
          legend.box = "horizontal", legend.margin = margin(t = -15))
}
# p_test <- plot_raw_temp()

plot_raw_temp <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308"){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num)
  # , date > (min(wt_df$date) + lubridate::days(7))) # cut first week of data off to avoid looking at tagging effect
  
  max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(10)) %>% as.POSIXct()
  min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(10)) %>% as.POSIXct()
  
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = temp_c)) + 
    geom_line(linewidth = 0.15) +
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b '%y",
                     expand = c(0,0),
                     limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "Temperature in °C") +
    theme(legend.position = "none",
          legend.box = "horizontal", legend.margin = margin(t = -15))
}

plot_raw_temp_short <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308"){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num)
  # , date > (min(wt_df$date) + lubridate::days(7))) # cut first week of data off to avoid looking at tagging effect
  
  max_date <- (max(data$date_time %>% lubridate::date()) + lubridate::days(1)) %>% as.POSIXct()
  min_date <- (min(data$date_time %>% lubridate::date()) -lubridate::days(1)) %>% as.POSIXct()
  
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = temp_c)) + 
    geom_line(linewidth = 0.25) +
    scale_x_datetime(date_breaks = "4 days",
                     date_labels = "%d.%m.%y",
                     expand = c(0,0),
                     limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "Temperature in °C") +
    theme(legend.position = "none",
          legend.box = "horizontal", legend.margin = margin(t = -15))
}

plot_dst_raw_depthlog <- function(data, time_vector, tag_serial_number_short){
  if((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300){ # make different choices for longterm and short term dsts
    date_breaks <- "1 month"
    # date_minor_breaks <- "1 month"
    date_labels <- "%b '%y"
    data <- data %>% filter(row_number() %% 5 == 0)
    angle <- 0 #30
  }else{
    date_breaks <- "1 week"
    date_minor_breaks <- "1 day"
    date_labels <- "%d.%m.%y"
    angle <- 0
  }
  # date_breaks <- ifelse((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300, "6 weeks", "1 week")
  # angle <- ifelse((data$date_time %>% lubridate::date() %>% unique() %>% length()) > 300, 50, 0)
  dst_plot <- ggplot2::ggplot(data = data) + geom_point(aes(x = .data[[time_vector]], y = -depth_m, color = temp_c), size = 0.5) + 
    # scale_y_continuous(limits = c(-80, 5), breaks = seq(-70, 0, by = 10)) +
    # scale_y_continuous(expand = c((-data$depth_m %>% min()) - 2, (-data$depth_m %>% max()) + 2)) +
    # scale_x_datetime(date_breaks = date_breaks, date_labels = "%b %d") + #, %y
    scale_x_datetime(
      # date_minor_breaks = date_minor_breaks,
                     date_breaks = date_breaks,
                     date_labels = date_labels,
                     expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = angle, hjust = 0.5)) + #
    # scale_colour_distiller(palette ="Spectral") +
    # scale_color_gradient2(low = "#003399", mid = "#b30000", high = "#ff8c1a") +
    scale_color_gradientn(colors = c("#003399", "#b30000", "#ff8c1a")) +
    labs(x = "Date", y = "Depth in m", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 
    # theme(legend.position = "bottom",
    #       legend.box = "horizontal", legend.margin = margin(t = -15))
  return(dst_plot)
}

plot_depth_subset_summer <- function(depthlog = masterias_depth_temp, tag_serial_num, start_date_chr, end_date_chr){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num,
                                     # lubridate::date(date_time) %>% between(as.POSIXct(start_date_chr), as.POSIXct(end_date_chr)),
                                     date_time %>% between(as.POSIXct(start_date_chr), as.POSIXct(end_date_chr)),
                                     row_number() %% 1 == 0)
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    # geom_point(size = 1.5) + # aes(colour = lubridate::hour(date_time)),%>% as.factor()
    geom_line(linewidth = 0.25) +
    scale_x_datetime(date_breaks = "1 day",
                     date_minor_breaks = "12 hours",
                     date_labels = "%d.%m.%y", #'%y
                     expand = c(0,0)
                     # ,limits = c(min_date, max_date)
    ) +
    # theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
    scale_y_continuous(expand = c(0,0), limits = c(-21,0)) +
    theme(legend.position = "bottom", legend.box = "horizontal") +
    labs(x = "", y = "Depth in m", color = "Hour of the day") #+ #title = paste0("tag ", tag_serial_number_short), 
  
}

plot_depth_subset_winter <- function(depthlog = masterias_depth_temp, tag_serial_num, start_date_chr, end_date_chr){
  data <- depthlog %>% dplyr::filter(tag_serial_number == tag_serial_num,
                                     lubridate::date(date_time) %>% between(as.POSIXct(start_date_chr), as.POSIXct(end_date_chr)),
                                     row_number() %% 2 == 0)
                                  
  plot <- ggplot2::ggplot(data = data, aes(x = date_time, y = -depth_m)) + 
    # geom_point(size = 1.5) + # aes(colour = lubridate::hour(date_time)),%>% as.factor()
    geom_line(321) +
    scale_x_datetime(date_breaks = "1 week",
                     date_minor_breaks = "1 day",
                     date_labels = "%d.%m.%y", #'%y
                     expand = c(0,0)
                     # ,limits = c(min_date, max_date)
    ) +
    # theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
    scale_y_continuous(expand = c(0,0), limits = c(-round(max(data$depth_m)),0)) + #round to nearest multiple of 10 ceiling(max(data$depth_m)
    theme(legend.position = "bottom", legend.box = "horizontal") +
    labs(x = "", y = "Depth in m", color = "Hour of the day") #+ #title = paste0("tag ", tag_serial_number_short), 
  
}


## 2. plot autocorrelation ####

plot_dst_autocorrelation <- function(acf_df, tagging_date, xaxis_lag = T){
  # tagging_date_seq <- base::seq(from = tagging_date %>% pull(), to = (tagging_date %>% pull()) + lubridate::days(nrow(acf_df) - 1), by = 1) #%>% base::as.data.frame()
  # xvals <- ifelse(isFALSE(xaxis_lag), seq(from = 0, to = nrow(acf_df) -1 , by = 1), tagging_date_seq)
  # xvals <- tagging_date_seq # for now bc ifelse does not work
  # xaxislab <- ifelse(isFALSE(xaxis_lag), "Lag in days", "Lag date")
  
  # # sum dataframe
  # acf_df_sum <- acf_308_df %>% mutate(acf_abs = abs(acf)) %>% filter(acf == min(acf)  | acf_abs == min(acf_abs))
  # acf_df_sum
  # acf_df$acf <- acf_df$acf %>% abs()
  # 
  # test <- findpeaks(acf_df$acf, npeaks = 3)
  
  acf_plot <- ggplot(data = acf_df, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "Autocorrelation", x = "Lag in days") +
    scale_x_continuous(expand = c(0,0))
    # scale_x_datetime(date_minor_breaks = "1 month",
    #                  date_breaks = "2 months",
    #                  date_labels = "%b %Y",
    #              expand = c(0,0))
  return(acf_plot)
}

## 3. plot summary statistics ####

plot_summary_stats <- function(data_depth, tag_serial_num, moon = TRUE){

  data <- data_depth %>%
    ungroup() %>%
    dplyr::filter(tag_serial_number == tag_serial_num) %>%
    mutate(t_days = t_days %>% as.numeric())
  max_date <- max(data$date) + lubridate::days(10)
  min_date <- min(data$date) - lubridate::days(10)
  
  # Calculate moon fraction illuminated
  dates <- seq(from = min(data$date), to = max(data$date), by = "day")
  moonfraq <- oce::moonAngle(t = dates, longitude = 2.45, latitude = 51)$illuminatedFraction
  moonfraq_scaled <- (moonfraq * max(data$depth_max_sgolay)) - max(data$depth_max_sgolay)
  moonfraq_df <- tibble::tibble(dates = dates %>% as.POSIXct(tz = "utc"),
                                moonfraq = moonfraq_scaled)
  
  plot <- ggplot(data = data) +
    geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "daily depth range"), alpha = 0.75) +
    geom_line(aes(x = date, y = -depth_median_sgolay, colour = "daily median depth"), linewidth = 0.5) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_y_continuous(expand = c(0, 0)) + #, sec.axis = sec_axis(trans = ~ (((. * (100/max(data$depth_max_sgolay))) + 100) * (.01)), name = "Illuminated Moon Fraction", labels = scales::label_percent())) +
    labs(x = "", y = "Depth in m") +
    scale_colour_manual(name = "", values = c("daily median depth" = "black", "daily depth range" = "lightgrey", "illuminated moon fraction" = "red")) +
    scale_fill_manual(name = "", values = c("daily depth range" = "lightgrey")) +
    theme(legend.position = "none", legend.box = "horizontal",   legend.margin = margin(t = -15)) + #"bottom"
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0, 0), limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
  
  if (isTRUE(moon)) {
    plot <- plot +
      geom_vline(aes(xintercept = max_date), colour = "black", alpha = 1, linewidth = 0.25) +
      geom_line(data = moonfraq_df, aes(x = dates, y = moonfraq_scaled, colour = "illuminated moon fraction"), linewidth = 1, alpha = 0.75) +
      scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(trans = ~ (((. * (100/max(data$depth_max_sgolay))) + 100) * (.01)), name = "Illuminated Moon Fraction", labels = scales::label_percent()))
  }
  
  plot
  
  
  # 
  # 
  # 
  # data <- data_depth %>% ungroup() %>% 
  #   filter(tag_serial_number == tag_serial_num) %>% 
  #   mutate(t_days = t_days %>% as.numeric())
  # max_date <- max(data$date) + lubridate::days(10)
  # min_date <- min(data$date) -lubridate::days(10)
  # 
  # # calc moon fraction illuminated
  # dates <- seq(from = min(data$date), to = max(data$date), by = "day")
  # moonfraq <- oce::moonAngle(t = dates, longitude = 2.45, latitude = 51)$illuminatedFraction #coordinates approx of dunkerque
  # moonfraq <- (moonfraq * max(data$depth_max_sgolay)) - max(data$depth_max_sgolay)
  # moonfraq_df <- tibble::tibble(dates = dates %>% as.POSIXct(tz = "utc"),
  #                               moonfraq = moonfraq)
  # 
  # plot <- ggplot(data = data) +
  #   geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "daily depth range"), alpha = 0.75) +
  #   geom_line(aes(x = date, y = -depth_median_sgolay, colour = "daily median depth"), linewidth = 1) +
  #   theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + #angle = 30
  #   scale_y_continuous(expand = c(0,0)) +
  #   labs(x = "", y = "Depth in m") +  
  #   scale_colour_manual(name = "", values = c("daily median depth" = "black", "daily depth range" = "lightgrey", "illuminated moon fraction" = "red"))  + #, "alphamoon" = "transparent", "median change" = "purple", "DVM" = "red", "rDVM" = "blue", "nVM" = "green"
  #   scale_fill_manual(name = "", values = c("daily depth range" = "lightgrey")) + #, "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "range" = "grey", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
  #   
  #   theme(legend.position = "bottom",
  #         legend.box = "horizontal",   legend.margin = margin(t = -15)) +
  #   scale_x_datetime(date_breaks = "1 month",
  #                    # date_minor_breaks = "1 month",
  #                    # date_breaks = "2 months",
  #                    # date_labels = "%b '%y"
  #                    date_labels = "%b %Y"
  #                    ,expand = c(0,0),
  #                    limits = c(min_date, max_date)
  #   ) +
  #   theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) 
  # 
  # if(isTRUE(moon)){
  #   plot <- plot + geom_line(data = moonfraq_df, aes(x = dates, y = moonfraq, colour = "illuminated moon fraction"), linewidth = 1, alpha = 0.5)
  # }
  # 
  # return(plot)
}

plot_summary_daynight <- function(data_depth, tag_serial_num, moon = TRUE, daytime = 1){
  
  data <- data_depth %>%
    ungroup() %>%
    # mutate(day = as.numeric(day)) %>%
    dplyr::filter(tag_serial_number == tag_serial_num
           ,day == daytime
           )
  
  
  # %>% mutate(t_days = t_days %>% as.numeric())
  max_date <- max(data$date) + lubridate::days(10)
  min_date <- min(data$date) - lubridate::days(10)
  
  # Calculate moon fraction illuminated
  dates <- seq(from = min(data$date), to = max(data$date), by = "day")
  moonfraq <- oce::moonAngle(t = dates, longitude = 2.45, latitude = 51)$illuminatedFraction
  moonfraq_scaled <- (moonfraq * max(data$depth_max_sgolay)) - max(data$depth_max_sgolay)
  moonfraq_df <- tibble::tibble(dates = dates %>% as.POSIXct(tz = "utc"),
                                moonfraq = moonfraq_scaled)
  # depth_median_label <- ifelse(daytime == 1, 
  #                              "median depth during day",
  #                              "median depth during night")
  # depth_median_label <- "median depth during day"
  # depth_range_label <- ifelse(daytime == 1, 
  #                              "depth range during day",
  #                              "depth range during night")
  
  plot <- ggplot() + #data = data
    # geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "depth range"), alpha = 0.75) +
    # geom_line(aes(x = date, y = -depth_median_sgolay, colour = "median depth"), linewidth = 1) +
    
    geom_line(data = moonfraq_df, aes(x = dates, y = moonfraq_scaled, colour = "illuminated moon fraction"), linewidth = 0.75, alpha = 0.75, linetype = "11") +
    # geom_line(data = moonfraq_df, aes(x = dates, y = moonfraq_scaled), colour = "darkblue", linewidth = 0.25, alpha = 0.75) +
    
    geom_ribbon(data = data %>% dplyr::filter(day == daytime), aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "depth range"), colour = "transparent", alpha = 0.65) + #
    # geom_ribbon(data = data %>% dplyr::filter(day == 1), aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "depth range (D)"), colour = "transparent", alpha = 0.75) + #
    geom_line(data = data %>% dplyr::filter(day == daytime), aes(x = date, y = -depth_median_sgolay, colour = "median depth"), linewidth = 0.5) +
    # geom_line(data = data %>% dplyr::filter(day == 0), aes(x = date, y = -depth_median_sgolay, colour = "median depth (N)"), linewidth = 0.25) +

    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    # scale_y_continuous(expand = c(0, 0)) +
    geom_vline(aes(xintercept = max_date), colour = "gray70", alpha = 1, linewidth = 0.25) +
    scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(trans = ~ (((. * (100/max(data$depth_max_sgolay))) + 100) * (.01)), name = "Illuminated Moon Fraction", labels = scales::label_percent())) +
    labs(x = "", y = "Depth in m") +
    scale_colour_manual(name = "", values = c("median depth (D)" = "orange", "median depth (N)" = "darkblue", "median depth" = ifelse(daytime == 1, "red", "darkblue"), "depth range" = ifelse(daytime == 1, "yellow", "darkblue"), "illuminated moon fraction" = "black")) +
    scale_fill_manual(name = "", values = c("depth range" = ifelse(daytime == 1, "orange", "lightblue"), "depth range (D)" = "yellow", "depth range (N)" = "lightblue")) +
    theme(legend.position = "none", legend.box = "horizontal",   legend.margin = margin(t = -15)) + #"bottom"
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0, 0), limits = c(min_date, max_date)) +
    theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
  
  # if (isTRUE(moon)) {
  #   plot <- plot +
  #     geom_vline(aes(xintercept = max_date), colour = "black", alpha = 1, linewidth = 0.25) +
  #     scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(trans = ~ (((. * (100/max(data$depth_max_sgolay))) + 100) * (.01)), name = "Illuminated Moon Fraction", labels = scales::label_percent())) +
  #     geom_line(data = moonfraq_df, aes(x = dates, y = moonfraq_scaled, colour = "illuminated moon fraction"), linewidth = 1, alpha = 0.75)
  # }
  
  plot

}


## 4. plot fft ####

plot_fft <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05){
  periodogram <- ggplot(data = fft_result %>% dplyr::filter(period < period_upperlim & period > period_lowerlim)) + 
    geom_line(aes(x = period, y = spec), colour = "black") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "Spectral density", x = "Period in hours") #, title = paste0("tag ", tag_serial_number_short)
  return(periodogram)
}

calc_fft <- function(depth_log, sample_freq){
  # `depth_log` contains the columns `depth_m` and `date_time`.
  
  # prepare/format data
  depth_log <- depth_log %>% 
    # make time vector
    mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric()) %>% 
    #dplyr::filter out NAs
    dplyr::filter(!is.na(depth_m))
  
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
  periodogram <- ggplot(data = fft_result %>% dplyr::filter(period < period_upperlim & period > period_lowerlim)) + 
    geom_line(aes(x = period, y = spec), colour = "black", linewidth = 1) + #theme_bw() +
    scale_y_continuous(expand = c(0,0)) +
    # theme_bw(base_size = 12) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".png"), plot = periodogram, width = 18, height = 12, units = "cm")
  return(periodogram)
}

fft_calc_plot <- function(depth_log, tag_serial_num_short, start_date, end_date, sample_frequency){
  fft_res <- calc_fft(depth_log = depth_log %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short),
                                                       lubridate::date(date_time) %>% 
                                                         between(start_date %>% as.POSIXct(), end_date %>% as.POSIXct())),
                      sample_freq = sample_frequency)
  
  pgram <- plot_periodogram(fft_result = fft_res, 
                            tag_serial_number_short = tag_serial_num_short)
  pgram %>% return()
}


## 5. plot wavelet results ####
plot_wavelet <- function(wt_df, type = c("power", "significance", "power_log"),
                             # y_breaks = c(4, 8, 16, 32, 64, 128),
                             # x_breaks = c("000", "100", "200", "300", "400", "500"),
                             date = TRUE, max_period = 50){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  y_breaks <- y_breaks[y_breaks <= max_period]
  
  wt_df <- wt_df  %>% dplyr::filter(period <= max_period)
  
  # transform dates
  wt_df$date <- wt_df$date %>% as.POSIXct(tz = "UTC")

  # change max and min date to include max x axis label completely
  max_date <- max(wt_df$date) + lubridate::days(10)
  min_date <- min(wt_df$date) -lubridate::days(10) #17 days bc one week of data was cut off
  
  #plot
  ifelse(date %>% isTRUE(),
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = date, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.5) +
                  geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_x_datetime(
                    # date_minor_breaks = "1 month",
                                   date_breaks = "1 month",
                                   date_labels = "%b '%y",
                                   expand = c(0,0), 
                                   limits = c(min_date, max_date)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "", y = "Period in days", fill = "log2(Power)") +
                  theme(legend.position = "bottom", # "bottom",
                        legend.box = "horizontal",   legend.margin = margin(t = -15)) +
                  theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date, y = period, fill = significance),
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
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date, y = period, fill = power),
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
                  geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = power_log),
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
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = significance),
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
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = power),
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

plot_wavelet_hrperiod <- function(wt_df = wt_df, type = c("power", "significance", "power_log"),
                                  date = TRUE, max_period = 72 #hours = 4 days
){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  wt_df <- wt_df %>% dplyr::filter(date > (min(wt_df$date) + lubridate::days(7))) # cut first week of data off to avoid looking at tagging effect
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  y_breaks <- y_breaks[y_breaks <= max_period]
  # transform dates
  # wt_df$date_time <- wt_df$date_time %>% as.POSIXct(tz = "UTC") # not needed bc we have the posict objects
  
  # # x axis labels
  # ifelse(date %>% isTRUE(),
  #        x_breaks <- c(wt_df$date_time[1], wt_df$date_time[(1/5) * n_data], wt_df$date_time[(2/5) * n_data], wt_df$date_time[(3/5) * n_data],
  #                      wt_df$date_time[(4/5) * n_data], wt_df$date_time[(5/5) * n_data])
  # ,
  # x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
  # 
  #plot
  
  wt_df <- wt_df  %>% dplyr::filter(period <= max_period)
  
  # change max and min date to include max x axis label completely
  max_date <- max(wt_df$date) + lubridate::days(10)
  min_date <- min(wt_df$date) -lubridate::days(10)
  
  
  ifelse(date %>% isTRUE(),
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = date, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.5) +
                  geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_x_datetime(
                    # date_minor_breaks = "1 month",
                    date_breaks = "1 month",
                    date_labels = "%b '%y",
                    expand = c(0,0),
                    limits = c(min_date, max_date)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "", y = "Period in hours", fill = "log2(Power)") +
                  theme(legend.position = "bottom", # "bottom",
                        legend.box = "horizontal",
                        legend.margin = margin(t = -15)) +
                  theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date_posicxt, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date_posicxt, y = period, fill = significance),
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
                         geom_tile(aes(x = date_posicxt, y = period, fill = power),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = date_posicxt, y = period, fill = power),
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
                  geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = power_log),
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
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = significance),
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
                         geom_tile(data = wt_df %>% dplyr::filter(sig == 1), aes(x = t, y = period, fill = power),
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
    labs(x = "Date", y = "rPE Score") +
    scale_x_datetime(date_minor_breaks = "1 month",
                     date_breaks = "2 months",
                     date_labels = "%b %Y",
                     expand = c(0,0))
  
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
#     #       legend.box = "horizontal",   legend.margin = margin(t = -15))
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
    distinct()%>%
    mutate(duration = base::difftime(end_date + lubridate::days(1), start_date, units = "days") %>% 
             as.numeric()) %>%
    dplyr::filter(duration > 3) %>% #only keep the periods that last more than 4 days
    ungroup() %>%
    mutate(CP_period_new = 1:n()) #make new CP_periods count
  
  df_c_points <- df_c_points %>%
    dplyr::filter(CP_period %in% (df_c_points_week$CP_period %>% unique()))
  
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
    labs(x = "Date", y = "Depth in m", fill = "CP period", colour = "CP period") +
    # theme(legend.position = "bottom",
    #       legend.box = "horizontal",   legend.margin = margin(t = -15))
    theme(legend.position="none", legend.direction="horizontal") + #"bottom"
    scale_x_datetime(date_minor_breaks = "1 month",
                     date_breaks = "2 months",
                     date_labels = "%b %Y",
                     expand = c(0,0))
  
  # p_data
  return(p_data)
  
}

# # plot_all_rulsif_data <- function(rulsif_result, var_list, tag_serial_num_short, all_data){
#   plots <- list()
#   for(variable in var_list){
#     plot <- plot_rulsif_data(rulsif_result = rulsif_result, 
#                              all_data = all_data,
#                              tag_serial_num_short = tag_serial_num_short,
#                              var = variable)
#     plots[[variable]] <- plot
#     # assign(paste0("p_", variable, "_", tag_serial_num_short, "_rulsif"), plot)
#   }
#   return(plots)
# }


# plots ####


## 0. basic acoustic detections/dst plots ####

### 0.1. boxplot length per sex ####
p_length_sex <- ggplot(data = tagged_animal_info) +
  geom_boxplot(aes(x = sex, y = length1)) +
  # geom_text(aes(x = group, y = max(value) + 0.2, label = round(max(value), 2)), 
  #           size = 3, position = position_dodge(width = 0.75)) +
  geom_text(data = tagged_animal_info %>% group_by(sex) %>% summarise(n = n()),
            aes(x = sex, y = 98, label = paste0("n =  ", n)), angle = 0, family = "serif") +
  labs(x = "Sex", y = "Total Length in cm")

save_data(data = p_length_sex, folder = plot_path)

### 0.2. abacus plot ####
# detections_tempdepth_daynight2 <- detections_tempdepth_daynight %>%
#   left_join(masterias_info %>% 
#               dplyr::mutate(area = "tagging date"), by = "tag_serial_number")

p_abacus <- ggplot() + # %>% mutate(tag_serial_number = reorder(tag_serial_number, masterias_info$release_date_time))
  # geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
  #            aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number, shape = sex), stroke = 0.5, colour = "black", size = 1.5) +
  
  geom_point(data = detections_tempdepth_daynight,
             aes(x = date_time, y = tag_serial_number, colour = area), size = 1) + #, pch = sex
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number, shape = sex), stroke = 0.5, colour = "black", size = 1.5) +
  scale_x_datetime(date_breaks = "2 months",
                   date_minor_breaks = "1 month",
                   date_labels = "%b '%y"
                   # ,expand = c(0,0)
  ) +
  scale_y_discrete(labels = masterias_info %>%
                     dplyr::filter(n_detect > 1) %>%
                     mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = F) %>%
                              as.character() %>% rev()) %>%
                     dplyr::select(tag_serial_number) %>%
                     pull() %>%
                     stringr::str_trunc(width = 3, side = "left", ellipsis = "")
  ) +
  scale_color_manual(values = c("black", "#ed7d31","#49E8E3")) + #, "#34b3bb"
  # scale_shape_manual(values = c(2, 4)) +
  scale_shape_manual(values = c(0,2)) + #c("\u2640", "\u2642") # unicode symbols for female and male
  labs(x = "", y = "tag serial nr.", colour = "receiver array:", shape = "tagging date:") +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.5)) +
  theme(legend.position = "bottom", # "bottom",
        legend.box = "horizontal",   legend.margin = margin(t = -15)) 

p_abacus
save_data(data = p_abacus, folder = plot_path)

### 0.3. heatmap detections ####

station_names_order <- c("OG10", "DL7", "DL9", "DL12", "OGDL", "SP3", "GVWSP","TRAWL", "WN2", "W6", "W7", #WS1
                         "borssele", "11", "13", "PVTSS", "4", #WS2
                         "Birkenfels","CPowerReefballs", "G-88", "Grafton", "LottoBuoy", "Nauticaena", "S4", "VG2", "W1", "WK12", "Westhinder" #BPNS
) %>% base::rev()

### stations Westerschelde
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
            n_ind = tag_serial_number %>% unique() %>% length()) #%>%
# mutate(n_detect = ifelse(n_detect > 1500, 1000, n_detect))
# 
# detections_sum_station_sum_ind <- detections_sum_station %>% group_by(n_ind) %>% summarise(n_ind_group = n(), ind_percent = 100 * ((n_ind_group)/ (detections_sum_station %>% nrow())))

p_detections_heatmap <- ggplot(data = detections_sum_station %>% dplyr::filter(!area == "BPNS", sex == "f"), #
                               aes(x = month_year, y = station_name, fill = n_detect, colour = "transparent")) + #, colour = n_detect
  # geom_tile(linewidth = 0.75) +
  geom_tile(linewidth = 1) +
  # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "black", angle = 0, family = "sans", fontface = "bold", size = 5) + #, colour = "grey"
  geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "white", angle = 0, family = "serif", fontface = "bold", size = 3) + #, colour = "grey"
  # facet_grid(vars(sex), scales="free_y") +
  scale_fill_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  scale_colour_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  # scale_colour_manual(name = "", values = c("# individuals" = "grey")) +
  # scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
  #                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
  scale_x_datetime(date_breaks = "2 months",
                   date_minor_breaks = "1 month",
                   date_labels = "%b '%y"
                   ,expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) + #labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), 
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  labs(x = "", y = "receiver station", fill = "number of acoustic detections", colour = "number of acoustic detections")  +
  theme(legend.position = "bottom", # "bottom",
        legend.box = "horizontal",   legend.margin = margin(t = -15)) 

p_detections_heatmap #%>% ggplotly()

save_data(data = p_detections_heatmap, folder = plot_path)

### 0.4. heatmap OG10 ####

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


p_detections_heatmap_OG10 <- ggplot() + #, colour = n_detect
  # geom_tile(linewidth = 0.75) +
  geom_tile(data = detections_OG102019, #
            mapping = aes(x = date, y = tag_serial_number, fill = n_detect, colour = n_detect), linewidth = 0) +
  # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "black", angle = 0, family = "sans", fontface = "bold", size = 5) + #, colour = "grey"
  # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "white", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  # facet_grid(vars(sex), scales="free_y") +
  scale_fill_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  scale_colour_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
  geom_label(data = OG10_2019_RI, mapping = aes(y = tag_serial_number , label = paste0("RI = ", RI %>% round(digits = 3)), x = detections_OG102019$date %>% min() - lubridate::days(12)), family = "serif", fill = "white", label.r = unit(0, "lines"), label.padding = unit(0.175, "lines"), size = 2) +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 day",
                   date_labels = "%b'%y"
                   ,expand = c(0,0),
                   limits = c(detections_OG102019$date %>% min() - lubridate::days(24), detections_OG102019$date %>% max() + lubridate::days(7))) +
  scale_y_discrete(expand = c(0,0)) + #labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), 
  theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
  labs(x = "", y = "tag serial nr.", fill = "number of acoustic detections", colour = "number of acoustic detections")  +
  theme(legend.position = "bottom", # "bottom",
        legend.box = "horizontal",   legend.margin = margin(t = -15)) 

p_detections_heatmap_OG10 #%>% ggplotly()

# 
# # old version
# 
# p_detections_heatmap_OG10 <- ggplot(data = detections_OG102019, #
#                                     aes(x = date, y = tag_serial_number, fill = n_detect, colour = "transparent")) + #, colour = n_detect
#   # geom_tile(linewidth = 0.75) +
#   geom_tile(linewidth = 1) +
#   # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "black", angle = 0, family = "sans", fontface = "bold", size = 5) + #, colour = "grey"
#   # geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "white", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
#   # facet_grid(vars(sex), scales="free_y") +
#   scale_fill_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
#   scale_colour_viridis_c(expand = c(0,0), option = "turbo", direction = 1) +
#   # scale_colour_manual(name = "", values = c("# individuals" = "grey")) +
#   # scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
#   #                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
#   scale_x_datetime(date_breaks = "1 month",
#                    # date_minor_breaks = "1 day",
#                    date_labels = "%b'%y"
#                    ,expand = c(0,0)) +
#   scale_y_discrete(expand = c(0,0)) + #labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), 
#   theme(axis.text.x = element_text(angle = 15, hjust = 0.25)) +
#   labs(x = "", y = "tag serial nr.", fill = "number of acoustic detections", colour = "number of acoustic detections")  +
#   theme(legend.position = "bottom", # "bottom",
#         legend.box = "horizontal",   legend.margin = margin(t = -15)) 
# 
# p_detections_heatmap_OG10 #%>% ggplotly()


save_data(data = p_detections_heatmap_OG10, folder = plot_path)


# ggplot(data = detections_OG102019 %>% group_by(tag_serial_number) %>% summarise(days_detected = n())) + 
#   geom_histogram(aes(x = days_detected), binwidth = 2)
# 
# ggplot(data = detections_OG102019) + 
#   geom_histogram(aes(x = n_detect))

### 0.5. residency index OG10 2019 ####
OG102019_residency <- detections_OG102019 %>% 
  mutate(total_days = base::difftime(date %>% max(), date %>% min(), units = "days") %>% as.numeric()) %>%
  group_by(tag_serial_number) %>%
  summarise(days_detected = n(),
            residency_index = days_detected / unique(total_days))

detections_OG102019 <- detections_OG102019 %>% 
  left_join(OG102019_residency, by = "tag_serial_number")

### boxplot n_detect RI

p_OG10_boxplot <- ggplot(data = detections_OG102019) +
  geom_boxplot(aes(x = tag_serial_number, y = n_detect, fill = residency_index)) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  geom_text(aes(x = tag_serial_number, y = 175, label = paste0("RI = ", residency_index %>% round(digits = 2))), colour = "black", angle = 0, fontface = "plain", family = "serif", size = 4) + #, colour = "grey" 
  labs(x = "tag serial nr.", y = "# detections per day", fill = "residency index")

p_OG10_boxplot
save_data(data = p_OG10_boxplot, folder = plot_path)

# # plot to see if the tagging batch has sth to do with mortality
# 
# plot <- ggplot(data = tagged_animal_info) +
#   geom_point(aes(x = days_at_liberty, y = length1, 
#                  colour = release_date_time %>% as.factor(),
#                  pch = release_loc %>% as.factor()),
#              size = 7, alpha = 0.6) +
#   geom_text(aes(x = days_at_liberty, y = length1, label = str_trunc(tag_serial_number, width = 3, side = "left", ellipsis = "")), nudge_y = 0, size = 4) +
#   labs(colour = "release date", y = "total length in cm", pch = "release location", x = "days at liberty")
# 
# plot# %>% ggplotly()
# 
# tagged_animal_info <- tagged_animal_info %>% mutate(release_date_time = release_date_time %>% as.factor(),
#                                                     release_loc = release_loc %>% as.factor())
# 
# lm_release <- lm(formula = days_at_liberty ~ release_date_time, data = tagged_animal_info)
# lm_release %>% summary()


### 0.6. vertical space use analysis ####

plot_depth_range <- ggplot(data = summary_all2 %>% dplyr::mutate(station_name = gsub("ws-", "", station_name),
                                                                 group = ifelse(group == "depth", "bathymetry", group))) +
  # geom_point(aes(x = station_name, y = -min_depth, colour = group)) +
  # geom_point(aes(x = station_name, y = -max_depth, colour = group)) +
  # geom_rect(aes(ymin = -min_depth, ymax = -max_depth, fill = group, x = station_name)) +
  geom_linerange(aes(ymin = min_depth, ymax = max_depth, x = station_name, color = group), linewidth = 3, alpha = 0.7) +
  geom_text(aes(x = station_name, y = -47.5, label = paste0(n_detect)), angle = 20, family = "serif", size = 3) + #"n =  ", 
  # geom_text(aes(x = station_name, y = -45), label = "n =", angle = 0, family = "serif") + #"n =  ", 
  # theme_minimal(base_size = 12) +
  scale_y_continuous(limits = c(-49,0)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(x = "Receiver Station:", y = "Depth in m", color = "")

plot_depth_range #%>% ggplotly()
save_data(data = plot_depth_range, folder = plot_path)


plot_depth_range_heatmap <- ggplot(data = summary_wide2 %>% dplyr::mutate(station_name = gsub("ws-", "", station_name)),
                                    aes(x = station_name, y = depth_range, fill = value)) + #, color = group
  geom_tile(linewidth = 0.25) +
  facet_grid(vars(group), scales="free_y") +
  # theme_minimal(base_size = 12) +
  scale_fill_viridis_c(expand = c(0,0)) +
  scale_y_discrete(labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(x = "Receiver Station", y = "Depth bin", fill = "Percentage")

# plot_depth_range_heatmap #%>% ggplotly()

save_data(data = plot_depth_range_heatmap, folder = plot_path)

### 0.7 ADST 308 ####

p_308_DST_acoustic <- ggplot() +
  geom_line(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                          lubridate::date(date_time) > as.POSIXct("2019-04-30", tz = "utc"),
                                                          row_number() %% 15 == 0), 
            aes(x = date_time, y = -depth_m), linewidth = 0.35) +
  geom_point(data = detections_tempdepth_daynight %>%
               dplyr::mutate(station_name = gsub("ws-", "", station_name),
                             station_name = gsub("bpns-", "", station_name)) %>%
               dplyr::filter(tag_serial_number == "1293308",
                             lubridate::year(date_time) == "2019",
                             sensor_type == "pressure"),
             aes(x = date_time, y = -parameter, colour = station_name), size = 1) +
  geom_point(data = detections_tempdepth_daynight %>%
               dplyr::mutate(station_name = gsub("ws-", "", station_name),
                             station_name = gsub("bpns-", "", station_name)) %>%
               dplyr::filter(tag_serial_number == "1293308",
                             lubridate::year(date_time) == "2019",
                             station_name == "Birkenfels",
                             sensor_type == "pressure"),
             aes(x = date_time, y = -parameter), colour ="red", size = 2) +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 week",
                   date_labels = "%b'%y"
                   # ,expand = c(0,0)
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "", y = "Depth in m", colour = "Receiver Station:") +
  theme(legend.position = "bottom",
        legend.box = "horizontal", legend.margin = margin(t = -15))

p_308_DST_acoustic# %>% ggplotly()

save_data(data = p_308_DST_acoustic, folder = plot_path)

## 1. raw depthlogs ####

## temp & depth lines ####

p_308_depth <- plot_raw_depth(depthlog = masterias_depth_temp, tag_serial_num = "1293308")
save_data(data = p_308_depth, folder = plot_path)
p_321_depth <- plot_raw_depth(depthlog = masterias_depth_temp, tag_serial_num = "1293321")
save_data(data = p_321_depth, folder = plot_path)

p_308_temp <- plot_raw_temp(depthlog = masterias_depth_temp, tag_serial_num = "1293308")
save_data(data = p_308_temp, folder = plot_path)
p_321_temp <- plot_raw_temp(depthlog = masterias_depth_temp, tag_serial_num = "1293321")
save_data(data = p_321_temp, folder = plot_path)

## subsets summer/winter ####


p_308_depth_summer <- plot_depth_subset_summer(depthlog = masterias_depth_temp, tag_serial_num = "1293308", start_date_chr = "2018-09-21 14:00:00 UTC", end_date_chr = "2018-09-24 14:00:00 UTC")
p_308_depth_summer
save_data(data = p_308_depth_summer, folder = plot_path)

p_308_depth_winter <- plot_depth_subset_winter(depthlog = masterias_depth_temp, tag_serial_num = "1293308", start_date_chr = "2019-02-08", end_date_chr = "2019-03-07")
p_308_depth_winter
save_data(data = p_308_depth_winter, folder = plot_path)

p_321_depth_summer <- plot_depth_subset_summer(depthlog = masterias_depth_temp, tag_serial_num = "1293321", start_date_chr = "2018-09-15 14:00:00 UTC", end_date_chr = "2018-09-18 14:00:00 UTC")
p_321_depth_summer
save_data(data = p_321_depth_summer, folder = plot_path)

p_321_depth_winter <- plot_depth_subset_winter(depthlog = masterias_depth_temp, tag_serial_num = "1293321", start_date_chr = "2019-02-08", end_date_chr = "2019-03-07")
p_321_depth_winter #%>% ggplotly()
save_data(data = p_321_depth_winter, folder = plot_path)

### short term dsts

p_319_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293319")
save_data(data = p_319_depth, folder = plot_path)
p_319_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293319")
save_data(data = p_319_temp, folder = plot_path)
gridExtra::grid.arrange(p_319_depth, p_319_temp, ncol = 1)

p_322_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293322")
save_data(data = p_322_depth, folder = plot_path)
p_322_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293322")
save_data(data = p_322_temp, folder = plot_path)
gridExtra::grid.arrange(p_322_depth, p_322_temp, ncol = 1)

p_295_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293295")
save_data(data = p_295_depth, folder = plot_path)
p_295_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293295")
save_data(data = p_295_temp, folder = plot_path)
gridExtra::grid.arrange(p_295_depth, p_295_temp, ncol = 1)

p_304_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293304")
save_data(data = p_304_depth, folder = plot_path)
p_304_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293304")
save_data(data = p_304_temp, folder = plot_path)
gridExtra::grid.arrange(p_304_depth, p_304_temp, ncol = 1)

p_310_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293310")
save_data(data = p_310_depth, folder = plot_path)
p_310_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293310")
save_data(data = p_310_temp, folder = plot_path)
gridExtra::grid.arrange(p_310_depth, p_310_temp, ncol = 1)

p_312_depth <- plot_raw_depth_short(depthlog = masterias_depth_temp, tag_serial_num = "1293312")
save_data(data = p_312_depth, folder = plot_path)
p_312_temp <- plot_raw_temp_short(depthlog = masterias_depth_temp, tag_serial_num = "1293312")
save_data(data = p_312_temp, folder = plot_path)
gridExtra::grid.arrange(p_312_depth, p_312_temp, ncol = 1)

## temp points ####

p_dst_raw_295 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293295"),
                              time_vector = "date_time",
                              tag_serial_number_short = "295")
# p_dst_raw_295
save_data(data = p_dst_raw_295, folder = plot_path)

p_dst_raw_319 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293319"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "319")
# p_dst_raw_319
save_data(data = p_dst_raw_319, folder = plot_path)

p_dst_raw_322 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293322"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "322")
# p_dst_raw_322
save_data(data = p_dst_raw_322, folder = plot_path)

p_dst_raw_304 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293304"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "304")
# p_dst_raw_304
save_data(data = p_dst_raw_304, folder = plot_path)

p_dst_raw_310 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293310"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "310")
# p_dst_raw_310
save_data(data = p_dst_raw_310, folder = plot_path)

p_dst_raw_312 <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293312"),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "312")
# p_dst_raw_312
save_data(data = p_dst_raw_312, folder = plot_path)

p_dst_raw_308_subset <- plot_dst_raw_depthlog(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308", lubridate::date(date_time) > as.POSIXct("2019-06-01")),
                                       time_vector = "date_time",
                                       tag_serial_number_short = "308")
# p_dst_raw_308_subset
save_data(data = p_dst_raw_308_subset, folder = plot_path)


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

p_acf_308 <- plot_dst_autocorrelation(acf_308_df,
                                      tagging_date = tagged_animal_info %>% dplyr::filter(tag_serial_number == "1293308") %>% dplyr::select(release_date_time))
p_acf_308 %>% ggplotly()

# get important lags (max, min, 0)
lags_308_important <- c(81, 170, 264, 300)

p_acf_308 <- p_acf_308 +
  geom_point(data = acf_308_df %>% dplyr::filter(lag %in% lags_308_important),
               mapping = aes(x = lag , y = acf), #, xend = lag, yend = 0
               colour = "blue", size = 3) +
  geom_text(data = acf_308_df %>% dplyr::filter(lag %in% lags_308_important),
            aes(x = lag, y = 0.625, label = lag), colour = "blue", family = "serif", size = 3.5) 
  
save_data(data = p_acf_308, folder = plot_path)

p_acf_321 <- plot_dst_autocorrelation(acf_321_df,
                                      tagging_date = tagged_animal_info %>% dplyr::filter(tag_serial_number == "1293321") %>% dplyr::select(release_date_time))
p_acf_321 %>% ggplotly()

grid.arrange(p_acf_308, p_acf_321, ncol = 2)

# get important lags (max, min, 0)
lags_321_important <- c(75, 138, 256, 345, 383)

p_acf_321 <- p_acf_321 +
  geom_point(data = acf_321_df %>% dplyr::filter(lag %in% lags_321_important),
             mapping = aes(x = lag , y = acf), #, xend = lag, yend = 0
             colour = "blue", size = 3) +
  geom_text(data = acf_321_df %>% dplyr::filter(lag %in% lags_321_important),
            aes(x = lag, y = 0.625, label = lag), colour = "blue", family = "serif", size = 3.5) 

p_acf_321
save_data(data = p_acf_321, folder = plot_path)


### acf + rulsif ####


rulsif_308_table_all <- rbind(rulsif_308_table_2_5percent %>% mutate(step_percent = 2.5),
                          rulsif_308_table_5percent %>% mutate(step_percent = 5),
                          rulsif_308_table_10percent %>% mutate(step_percent = 10))

p_308_rulsif_all <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == 
                                                "1293308")) + 
  geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay), alpha = 0.7, fill = "grey") + #, fill = "depth range"
  geom_line(aes(x = date, y = -depth_median_sgolay), colour = "black") +
  geom_rect(data = rulsif_308_table_all, aes(xmin = start_date, xmax = end_date, fill = step_percent %>% as.factor(),
                                         ymin = -Inf, ymax = Inf),
            alpha = 0.6) +
  scale_x_datetime(date_minor_breaks = "1 month",
                   date_breaks = "2 months",
                   date_labels = "%b %Y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Date", y = "Depth in m", fill = "Step percent") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",   legend.margin = margin(t = -15))
# p_308_rulsif_all 

save_data(data = p_308_rulsif_all, folder = plot_path)


rulsif_321_table_all <- rbind(rulsif_321_table_2_5percent %>% mutate(step_percent = 2.5),
                              rulsif_321_table_5percent %>% mutate(step_percent = 5),
                              rulsif_321_table_10percent %>% mutate(step_percent = 10))

p_321_rulsif_all <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == 
                                                                    "1293321")) + 
  geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay), alpha = 0.7, fill = "grey") + #, fill = "depth range"
  geom_line(aes(x = date, y = -depth_median_sgolay), colour = "black") +
  geom_rect(data = rulsif_321_table_all, aes(xmin = start_date, xmax = end_date, fill = step_percent %>% as.factor(),
                                             ymin = -Inf, ymax = Inf),
            alpha = 0.6) +
  scale_x_datetime(date_minor_breaks = "1 month",
                   date_breaks = "2 months",
                   date_labels = "%b %Y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Date", y = "Depth in m", fill = "Step percent") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",   legend.margin = margin(t = -15))
# p_321_rulsif_all 

save_data(data = p_321_rulsif_all, folder = plot_path)

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

### 3.1. fft subsets ####
#### tag 308 ####
#### summer residence
pgram_308_summerres_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                      start_date = "2018-08-09",
                                      # end_date = as.POSIXct("2018-09-30"), 
                                      end_date = "2018-09-25", 
                                      sample_frequency = 30)
pgram_308_summerres_2018 <- pgram_308_summerres_2018 + labs(title = "summer residency, Aug 11 - Sep 25, 2018")
save_data(data = pgram_308_summerres_2018, folder = plot_path)
#### winter migration
pgram_308_wintermig_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                          start_date = "2018-09-26",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2018-10-10", 
                                          sample_frequency = 30)
pgram_308_wintermig_2018 <- pgram_308_wintermig_2018 + labs(title = "winter migration, Sep 26 - Oct 10, 2018")
save_data(data = pgram_308_wintermig_2018, folder = plot_path)
#### winter residence
pgram_308_winterres_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                          start_date = "2018-10-11",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-03-21", 
                                          sample_frequency = 30)
pgram_308_winterres_2018 <- pgram_308_winterres_2018  + labs(title = "winter residency, Oct 11, 2018 - Mar 21, 2019")
save_data(data = pgram_308_winterres_2018, folder = plot_path)
#### summer migration
pgram_308_summermig_2019 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                          start_date = "2019-03-22",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-05-10", 
                                          sample_frequency = 30)
pgram_308_summermig_2019 <- pgram_308_summermig_2019 + labs(title = "summer migration, Mar 22 - May 10, 2019")
save_data(data = pgram_308_summermig_2019, folder = plot_path)
#### summer residence
pgram_308_summerres_2019 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                          start_date = "2019-05-11",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-08-03", 
                                          sample_frequency = 30)
pgram_308_summerres_2019 <- pgram_308_summerres_2019 + labs(title = "summer residency, May 11 - Aug 03, 2019")
save_data(data = pgram_308_summerres_2019, folder = plot_path)

gridExtra::grid.arrange(pgram_308_summerres_2018, pgram_308_wintermig_2018, pgram_308_winterres_2018, pgram_308_summermig_2019, pgram_308_summerres_2019, ncol = 2)

#### tag 321 ####
#### summer residence
pgram_321_summerres_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2018-07-29",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2018-09-29", 
                                          sample_frequency = 30)
pgram_321_summerres_2018 <- pgram_321_summerres_2018 + labs(title = "summer residency, Jul 29 - Sep 29, 2018")
save_data(data = pgram_321_summerres_2018, folder = plot_path)
#### winter migration
pgram_321_wintermig_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2018-09-30",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2018-11-03", 
                                          sample_frequency = 30)
pgram_321_wintermig_2018 <- pgram_321_wintermig_2018 + labs(title = "winter migration, Sep 30 - Nov 03, 2018")
save_data(data = pgram_321_wintermig_2018, folder = plot_path)
#### winter residence
pgram_321_winterres_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2018-11-03",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-05-02", 
                                          sample_frequency = 30)
pgram_321_winterres_2018 <- pgram_321_winterres_2018  + labs(title = "winter residency, Oct 03, 2018 - May 02, 2019")
save_data(data = pgram_321_winterres_2018, folder = plot_path)
#### summer migration
pgram_321_summermig_2019 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2019-05-03",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-05-26", 
                                          sample_frequency = 30)
pgram_321_summermig_2019 <- pgram_321_summermig_2019 + labs(title = "summer migration, May 03 - May 26, 2019")
save_data(data = pgram_321_summermig_2019, folder = plot_path)
#### summer residence
pgram_321_summerres_2019 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2019-05-27",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-09-11", 
                                          sample_frequency = 30)
pgram_321_summerres_2019 <- pgram_321_summerres_2019 + labs(title = "summer residency, May 27 - Sep 1, 2019")
save_data(data = pgram_321_summerres_2019, folder = plot_path)
#### winter migration
pgram_321_wintermig_2019 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = "2019-09-12",
                                          # end_date = as.POSIXct("2018-09-30"), 
                                          end_date = "2019-10-15", 
                                          sample_frequency = 30)
pgram_321_wintermig_2019 <- pgram_321_wintermig_2019 + labs(title = "winter migration, Sep 12 - Oct 15, 2019")
save_data(data = pgram_321_wintermig_2019, folder = plot_path)

gridExtra::grid.arrange(pgram_321_summerres_2018, pgram_321_wintermig_2018, pgram_321_winterres_2018, pgram_321_summermig_2019, pgram_321_summerres_2019, pgram_321_wintermig_2019, ncol = 2)


## 4. summary statistics ####

p_308_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                           tag_serial_num = "1293308",
                                           moon = F)
p_308_sum_stats # %>% ggplotly()
save_data(data = p_308_sum_stats, folder = plot_path)

# #assess difference between sgolay filtered and raw min deph
# p_308_raw_sumstat <- p_308_sum_stats + geom_line(aes(x = date, y = -depth_min), colour = "red")
# p_308_raw_sumstat %>% ggplotly()

p_321_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                           tag_serial_num = "1293321",
                                           moon = F)
p_321_sum_stats %>% ggplotly()
save_data(data = p_321_sum_stats, folder = plot_path)

# with moon
p_308_sum_stats_moon <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293308",
                                      moon = T)
p_308_sum_stats_moon #%>% ggplotly()
save_data(data = p_308_sum_stats_moon, folder = plot_path)

p_321_sum_stats_moon <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293321",
                                      moon = T)
p_321_sum_stats_moon #%>% ggplotly()
save_data(data = p_321_sum_stats_moon, folder = plot_path)

### 4.1 daynight sum stats ####

p_308_sum_night <- plot_summary_daynight(data_depth = long_dst_daynight,
                                         tag_serial_num = "1293308", 
                                         daytime = 0)
# p_308_sum_night #%>% ggplotly()
save_data(data = p_308_sum_night, folder = plot_path)

p_308_sum_day <- plot_summary_daynight(data_depth = long_dst_daynight,
                                       tag_serial_num = "1293308", 
                                       daytime = 1)
# p_308_sum_day #%>% ggplotly()
save_data(data = p_308_sum_day, folder = plot_path)

p_321_sum_night <- plot_summary_daynight(data_depth = long_dst_daynight,
                                         tag_serial_num = "1293321", 
                                         daytime = 0)
# p_321_sum_night #%>% ggplotly()
save_data(data = p_321_sum_night, folder = plot_path)

p_321_sum_day <- plot_summary_daynight(data_depth = long_dst_daynight,
                                       tag_serial_num = "1293321", 
                                       daytime = 1)
# p_321_sum_day #%>% ggplotly()
save_data(data = p_321_sum_day, folder = plot_path)

#### daynight
p_308_sum_daynight <- plot_summary_daynight(data_depth = long_dst_daynight,
                                      tag_serial_num = "1293308", 
                                      daytime = 0)
p_308_sum_daynight #%>% ggplotly()
save_data(data = p_308_sum_daynight, folder = plot_path)

p_321_sum_daynight <- plot_summary_daynight(data_depth = long_dst_daynight,
                                      tag_serial_num = "1293321", 
                                      moon = TRUE,
                                      daytime = 0)
p_321_sum_daynight #%>% ggplotly()
save_data(data = p_321_sum_daynight, folder = plot_path) 
#   
# #### day
# 
# p_308_sum_day <- plot_summary_daynight(data_depth = long_dst_daynight,
#                                       tag_serial_num = "1293308", daytime = 1)
# p_308_sum_day #%>% ggplotly()
# save_data(data = p_308_sum_day, folder = plot_path)
# 
# p_321_sum_day <- plot_summary_daynight(data_depth = long_dst_daynight,
#                                       tag_serial_num = "1293321", daytime = 1)
# p_321_sum_day #%>% ggplotly()
# save_data(data = p_321_sum_day, folder = plot_path) 
# 
# gridExtra::grid.arrange(p_308_sum_stats + labs(title = "day + night, female"), p_321_sum_stats + labs(title = "day + night, male"),
#                         p_308_sum_night + labs(title = "day, female"), p_321_sum_night + labs(title = "day, male"),
#                         p_308_sum_day + labs(title = "night, female"), p_321_sum_day + labs(title = "night, male"),
#                         ncol = 2)

## 5. wavelet results ####

### 5.1. daily summaries ####

#### tag 308 ####

p_308_wavelet_depth_median_sgolay <- plot_wavelet(wt_df = wt_df_308_mediandepth_sgolay,
                                                  type = "power_log", max_period = 50)

# p_308_wavelet_depth_median_sgolay
save_data(data = p_308_wavelet_depth_median_sgolay, folder = plot_path)

p_308_wavelet_depth_range_sgolay <- plot_wavelet(wt_df = wt_df_308_depthrange_sgolay,
                                                 type = "power_log", max_period = 50)

# p_308_wavelet_depth_range_sgolay #%>% ggplotly()
save_data(data = p_308_wavelet_depth_range_sgolay, folder = plot_path)

p_308_wavelet_depth_min_sgolay <- plot_wavelet(wt_df = wt_df_308_mindepth_sgolay,
                                               type = "power_log", max_period = 50) 

# p_308_wavelet_depth_min_sgolay
save_data(data = p_308_wavelet_depth_min_sgolay, folder = plot_path)

p_308_wavelet_depth_max_sgolay <- plot_wavelet(wt_df = wt_df_308_maxdepth_sgolay,
                                               type = "power_log", max_period = 50) 

# p_308_wavelet_depth_max_sgolay
save_data(data = p_308_wavelet_depth_max_sgolay, folder = plot_path)

# gridExtra::grid.arrange(p_308_wavelet_depth_median_sgolay + labs(title = "daily median depth"),
#                         p_308_wavelet_depth_range_sgolay + labs(title = "daily depth range"),
#                         ncol = 1)

# p_308_wavelet_depth_median_roll3 <- plot_wavelet(wt_df = wt_df_308_mediandepth_roll3,
#                                                  type = "power_log") 
# 
# # p_308_wavelet_depth_median_roll3
# save_data(data = p_308_wavelet_depth_median_roll3, folder = plot_path)
# 
# p_308_wavelet_depth_median_change_roll3 <- plot_wavelet(wt_df = wt_df_308_mediandepth_change_roll3,
#                                                         type = "power_log") 
# 
# # p_308_wavelet_depth_median_change_roll3
# save_data(data = p_308_wavelet_depth_median_change_roll3, folder = plot_path)
# 
# p_308_wavelet_maxdepth_change_roll3 <- plot_wavelet(wt_df = wt_df_308_maxdepth_change_roll3,
#                                                  type = "power_log") 
# 
# # p_308_wavelet_maxdepth_change_roll3
# save_data(data = p_308_wavelet_maxdepth_change_roll3, folder = plot_path)
# 
# p_308_wavelet_depth_min <- plot_wavelet(wt_df = wt_df_308_mindepth,
#                                                  type = "power_log") 
# 
# # p_308_wavelet_depth_min
# save_data(data = p_308_wavelet_depth_min, folder = plot_path)
# 
# p_308_wavelet_depth_max <- plot_wavelet(wt_df = wt_df_308_maxdepth,
#                                         type = "power_log") 

# p_308_wavelet_depth_max
# save_data(data = p_308_wavelet_depth_max, folder = plot_path)


#### tag 321 ####

p_321_wavelet_depth_median_sgolay <- plot_wavelet(wt_df = wt_df_321_mediandepth_sgolay,
                                                  type = "power_log", max_period = 50) 

# p_321_wavelet_depth_median_sgolay
save_data(data = p_321_wavelet_depth_median_sgolay, folder = plot_path)

p_321_wavelet_depth_range_sgolay <- plot_wavelet(wt_df = wt_df_321_depthrange_sgolay,
                                                 type = "power_log", max_period = 50) 

# p_321_wavelet_depth_range_sgolay
save_data(data = p_321_wavelet_depth_range_sgolay, folder = plot_path)

p_321_wavelet_depth_min_sgolay <- plot_wavelet(wt_df = wt_df_321_mindepth_sgolay,
                                               type = "power_log", max_period = 50) 

# p_321_wavelet_depth_min_sgolay
save_data(data = p_321_wavelet_depth_min_sgolay, folder = plot_path)

p_321_wavelet_depth_max_sgolay <- plot_wavelet(wt_df = wt_df_321_maxdepth_sgolay,
                                               type = "power_log", max_period = 50) 

# p_321_wavelet_depth_max_sgolay
save_data(data = p_321_wavelet_depth_max_sgolay, folder = plot_path)

# gridExtra::grid.arrange(p_308_wavelet_depth_median_sgolay + labs(title = "female, daily median depth"),
#                         p_321_wavelet_depth_median_sgolay + labs(title = "male, daily median depth"),
#                         p_308_wavelet_depth_range_sgolay + labs(title = "female, daily depth range"),
#                         p_321_wavelet_depth_range_sgolay + labs(title = "male, daily depth range"),
#                         ncol = 2)

# p_321_wavelet_depth_median_roll3 <- plot_wavelet(wt_df = wt_df_321_mediandepth_roll3,
#                                                  type = "power_log") 
# 
# # p_321_wavelet_depth_median_roll3
# save_data(data = p_321_wavelet_depth_median_roll3, folder = plot_path)
# 
# p_321_wavelet_depth_median_change_roll3 <- plot_wavelet(wt_df = wt_df_321_mediandepth_change_roll3,
#                                                  type = "power_log") 
# 
# # p_321_wavelet_depth_median_change_roll3
# save_data(data = p_321_wavelet_depth_median_change_roll3, folder = plot_path)
# 
# 
# p_321_wavelet_maxdepth_change_roll3 <- plot_wavelet(wt_df = wt_df_321_maxdepth_change_roll3,
#                                                     type = "power_log") 
# 
# # p_321_wavelet_maxdepth_change_roll3
# save_data(data = p_321_wavelet_maxdepth_change_roll3, folder = plot_path)
# 
# p_321_wavelet_depth_min <- plot_wavelet(wt_df = wt_df_321_mindepth,
#                                         type = "power_log") 
# 
# # p_321_wavelet_depth_min
# save_data(data = p_321_wavelet_depth_min, folder = plot_path)
# 
# p_321_wavelet_depth_max <- plot_wavelet(wt_df = wt_df_321_maxdepth,
#                                         type = "power_log") 
# 
# # p_321_wavelet_depth_max
# save_data(data = p_321_wavelet_depth_max, folder = plot_path)

### 5.2. hourly summaries ####
#### tag 308
p_308_wavelet_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_308_depth_hr, type = "power_log",
                                                date = TRUE, max_period = 72)

# p_308_wavelet_depth_hr
save_data(data = p_308_wavelet_depth_hr, folder = plot_path)

#### tag 321
p_321_wavelet_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_321_depth_hr, type = "power_log",
                                                date = TRUE, max_period = 72)

# p_321_wavelet_depth_hr
save_data(data = p_321_wavelet_depth_hr, folder = plot_path)

gridExtra::grid.arrange(p_308_sum_stats, p_321_sum_stats,
                        p_308_wavelet_depth_median_sgolay, p_321_wavelet_depth_median_sgolay, 
                        p_308_wavelet_depth_hr, p_321_wavelet_depth_hr,
                        p_308_wavelet_depth_range_sgolay, p_321_wavelet_depth_range_sgolay,
                        ncol = 2)

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
grid.arrange(p_308_ribbon_rulsif_2_5percent, p_308_scores_rulsif_2_5percent, ncol = 1)

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

## 7. lm moon illumination ####


### plot lm 308 ####

# ### min daily depth
# p_308_lm_min_moonfraq_smooth <- ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_min_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily minimum depth in m (Savitzky-Golay filter)") #, title = 'daily min depth over moon fraq'
# save_data(data = p_308_lm_min_moonfraq_smooth, folder = plot_path)
# 
# # plot residuals
# p_308_lm_min_moonfraq_residuals <- ggplot(lm_308_depthmin_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# save_data(data = p_308_lm_min_moonfraq_residuals, folder = plot_path)
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# p_308_lm_min_moonfraq_qq <- ggplot(lm_308_depthmin_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# save_data(data = p_308_lm_min_moonfraq_qq, folder = plot_path)

# ## log transformed -> does not look better
# ggplot(data_lm_308, aes(sample=-log(depth_min_sgolay))) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")

# 
# # plot density (to assess normal distribution of residuals)
# p_308_lm_min_moonfraq_density <- ggplot(lm_308_depthmin_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# save_data(data = p_308_lm_min_moonfraq_density, folder = plot_path)


### median daily depth
# p_308_lm_median_moonfraq_smooth <- ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)") #, title = 'daily median depth over moon fraq'
# save_data(data = p_308_lm_median_moonfraq_smooth, folder = plot_path)

p_308_lm_median_moonfraq_smooth <- ggplot(mapping = aes(x = moonfraq, y = -(depth_median_sgolay))) +
  geom_point(data = data_lm_308_day, mapping = aes(colour = "median depth (D)"), fill = "transparent", alpha = 0.75, size = 1) +
  geom_point(data = data_lm_308_night, mapping = aes(colour = "median depth (N)"), fill = "transparent", alpha = 0.75, size = 1) +
  geom_smooth(data = data_lm_308_day, method = "lm", mapping = aes(fill = "lm with 95 % CI (D)"), colour = "orange", alpha = 0.6, linewidth = 0.25) +
  geom_smooth(data = data_lm_308_night, method = "lm", mapping = aes(fill = "lm with 95 % CI (N)"), colour = "darkblue", alpha = 0.6, linewidth = 0.25) +
  geom_text(data = lm_sum_308_depthmedian_day_night %>% dplyr::filter(daytime == "day"), aes(x = 0.125, y = -42, label = paste0("y (day) = ", intercept %>% round(digits = 2), " + ", slope %>% round(digits = 2), ", R squared (adj.) = ", adj.r.squared %>% round(digits = 4))), colour = "orange", hjust = "left", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  geom_text(data = lm_sum_308_depthmedian_day_night %>% dplyr::filter(daytime == "night"), aes(x = 0.125, y = -46, label = paste0("y (night) = ", intercept %>% round(digits = 2), " + ", slope %>% round(digits = 2), ", R squared (adj.) = ", adj.r.squared %>% round(digits = 4))), colour = "darkblue", hjust = "left", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  
  scale_colour_manual(name = "", values = c("linear model" = "darkgrey", "median depth (D)" = "orange", "median depth (N)" = "darkblue", "median depth" = "black")) +
  scale_fill_manual(name = "", values = c("lm with 95 % CI (D)" = "yellow", "lm with 95 % CI (N)" = "lightblue", "median depth (D)" = "orange", "median depth (N)" = "darkblue")) +
  theme(legend.position = "bottom", legend.box = "horizontal",   legend.margin = margin(t = -15)) + #"bottom"
  labs(x = "illuminated moon fraction of the moon", y = "median depth in m (Savitzky-Golay filtered)") #, title = 'daily median depth over moon fraq'
save_data(data = p_308_lm_median_moonfraq_smooth, folder = plot_path)

# ### median daily depth daynight
# p_308_daynight_lm_median_moonfraq_smooth <- ggplot(data = data_lm_308_daynight, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)") #, title = 'daily median depth over moon fraq'
# save_data(data = p_308_daynight_lm_median_moonfraq_smooth, folder = plot_path)
# 
# # plot residuals
# p_308_lm_median_moonfraq_residuals <- ggplot(lm_308_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# save_data(data = p_308_lm_median_moonfraq_residuals, folder = plot_path)
# 
# # plot residuals daynight
# p_308_daynight_lm_median_moonfraq_residuals <- ggplot(lm_308_daynight_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# save_data(data = p_308_daynight_lm_median_moonfraq_residuals, folder = plot_path)
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# p_308_lm_median_moonfraq_qq <- ggplot(lm_308_depthmedian_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# save_data(data = p_308_lm_median_moonfraq_qq, folder = plot_path)
# 
# # plot qq daynight of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# p_308_daynight_lm_median_moonfraq_qq <- ggplot(lm_308_daynight_depthmedian_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# save_data(data = p_308_daynight_lm_median_moonfraq_qq, folder = plot_path)

# ## log transformed -> does not look better
# ggplot(data_lm_308, aes(sample=-log(depth_median_sgolay))) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# p_308_lm_median_moonfraq_density <- ggplot(lm_308_depthmedian_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# save_data(data = p_308_lm_median_moonfraq_density, folder = plot_path)
# 
# 
# # plot density daynight (to assess normal distribution of residuals)
# p_308_daynight_lm_median_moonfraq_density <- ggplot(lm_308_daynight_depthmedian_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# save_data(data = p_308_daynight_lm_median_moonfraq_density, folder = plot_path)

### plot lm 321 ####

# ### min daily depth
# p_321_lm_min_moonfraq_smooth <- ggplot(data = data_lm_321, aes(x = moonfraq, y = -log(depth_min_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily minimum depth in m (Savitzky-Golay filter)") #, title = 'daily min depth over moon fraq'
# save_data(data = p_321_lm_min_moonfraq_smooth, folder = plot_path)
# 
# # plot residuals
# p_321_lm_min_moonfraq_residuals <- ggplot(lm_321_depthmin_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# save_data(data = p_321_lm_min_moonfraq_residuals, folder = plot_path)
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# p_321_lm_min_moonfraq_qq <- ggplot(lm_321_depthmin_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# save_data(data = p_321_lm_min_moonfraq_qq, folder = plot_path)
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_321, aes(sample=-log(depth_min_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# p_321_lm_min_moonfraq_density <- ggplot(lm_321_depthmin_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# save_data(data = p_321_lm_min_moonfraq_density, folder = plot_path)


### median daily depth

p_321_lm_median_moonfraq_smooth <- ggplot(mapping = aes(x = moonfraq, y = -(depth_median_sgolay))) +
  geom_point(data = data_lm_321_day, mapping = aes(colour = "median depth (D)"), fill = "transparent", alpha = 0.75) +
  geom_point(data = data_lm_321_night, mapping = aes(colour = "median depth (N)"), fill = "transparent", alpha = 0.75) +
  geom_smooth(data = data_lm_321_day, method = "lm", mapping = aes(fill = "lm with 95 % CI (D)"), colour = "orange", alpha = 0.4) +
  geom_smooth(data = data_lm_321_night, method = "lm", mapping = aes(fill = "lm with 95 % CI (N)"), colour = "darkblue", alpha = 0.4) +
  geom_text(data = lm_sum_321_depthmedian_day_night %>% dplyr::filter(daytime == "day"), aes(x = 0.125, y = -44, label = paste0("y (day) = ", intercept %>% round(digits = 2), " + ", slope %>% round(digits = 2), ", R squared (adj.) = ", adj.r.squared %>% round(digits = 4))), colour = "orange", hjust = "left", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  geom_text(data = lm_sum_321_depthmedian_day_night %>% dplyr::filter(daytime == "night"), aes(x = 0.125, y = -46, label = paste0("y (night) = ", intercept %>% round(digits = 2), " + ", slope %>% round(digits = 2), ", R squared (adj.) = ", adj.r.squared %>% round(digits = 4))), colour = "darkblue", hjust = "left", angle = 0, family = "serif", fontface = "bold", size = 4) + #, colour = "grey"
  
  scale_colour_manual(name = "", values = c("linear model" = "darkgrey", "median depth (D)" = "orange", "median depth (N)" = "darkblue", "median depth" = "black")) +
  scale_fill_manual(name = "", values = c("lm with 95 % CI (D)" = "yellow", "lm with 95 % CI (N)" = "lightblue", "median depth (D)" = "orange", "median depth (N)" = "darkblue")) +
  theme(legend.position = "bottom", legend.box = "horizontal",   legend.margin = margin(t = -15)) + #"bottom"
  labs(x = "illuminated fraction of the moon", y = "median depth in m (Savitzky-Golay filtered)") #, title = 'daily median depth over moon fraq'
save_data(data = p_321_lm_median_moonfraq_smooth, folder = plot_path)

# p_321_lm_median_moonfraq_smooth <- ggplot(data = data_lm_321, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)") #, title = 'daily median depth over moon fraq'
# save_data(data = p_321_lm_median_moonfraq_smooth, folder = plot_path)
# 
# # plot residuals
# p_321_lm_median_moonfraq_residuals <- ggplot(lm_321_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# save_data(data = p_321_lm_median_moonfraq_residuals, folder = plot_path)
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# p_321_lm_median_moonfraq_qq <- ggplot(lm_321_depthmedian_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# save_data(data = p_321_lm_median_moonfraq_qq, folder = plot_path)
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_321, aes(sample=-log(depth_median_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# p_321_lm_median_moonfraq_density <- ggplot(lm_321_depthmedian_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# save_data(data = p_321_lm_median_moonfraq_density, folder = plot_path)


# save all plots as .pdf and .png ####
#To Do

# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".pdf"), plot = dst_plot, width = 18, height = 12, units = "cm")
# ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".png"), plot = dst_plot, width = 18, height = 12, units = "cm")

