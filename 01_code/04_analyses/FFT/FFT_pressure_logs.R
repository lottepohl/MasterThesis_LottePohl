# WORKSPACE ####
library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(psdr)
library(ggplot2)

rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/04_analysis_results/spectral_analysis/fft/")

# source(paste0(dir_path, "/02_scripts/02_load_data/load_depth_temp_logs.R"))
source(paste0(dir_path, "/02_scripts/04_analyses/FFT/calculate_fft_psd.R"))
source(paste0(dir_path, "/02_scripts/05_plots_maps/plot_fft_periodogram.R"))
source(paste0(dir_path, "/02_scripts/05_plots_maps/plot_dst_pressure_temp.R"))

# todo: load 2 scripts (fft calc and periodogram plot) and do ggplotly() and maybe superpose them?



plot_dst_295 %>% ggplotly()
pgram_295 %>% ggplotly()

pgram_304 %>% ggploty()
plot_dst_304 %>% ggplotly()

pgram_308 %>% ggploty()
plot_dst_308 %>% ggplotly()

pgram_310 %>% ggploty()
plot_dst_310 %>% ggplotly()

pgram_312 %>% ggploty()
plot_dst_312 %>% ggplotly()

pgram_319 %>% ggploty()
plot_dst_319 %>% ggplotly()

pgram_321 %>% ggploty()
plot_dst_321 %>% ggplotly()

pgram_322 %>% ggploty()
plot_dst_322 %>% ggplotly()





# 
# # Function: calculate fft ####
# calc_fft <- function(depth_log, sample_freq){
#   # `depth_log` contains the columns `depth_m` and `date_time`.
#   
#   # prepare/format data
#   depth_log <- depth_log %>% 
#     # make time vector
#     mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric()) %>% 
#     #filter out NAs
#     filter(!is.na(depth_m))
# 
#   # define tmax and n
#   tmax <- depth_log$t %>% max()
#   n <- depth_log$t %>% length()
#   
#   # mit nullen auffuellen bis naechste n^2
#   next_power <- 2^ceiling(log2(n))
#   depth_log_padded <- c(depth_log$depth_m, rep(0, next_power - n))
# 
#   # compute the frequency spectrum
#   spec <- abs(fft(depth_log_padded))^2
#   
#   # compute the frequency vector
#   # freq <- seq(0, length(spec) - 1) * (1/sample_freq) / length(spec)
#   freq <- seq(0, length(spec) - 1) * (sample_freq) / length(spec)
#   
#   # make result dataframe
#   result_fft <- cbind(spec, freq) %>% as.data.frame() %>% 
#     # calculate period
#     mutate(period = 1 / freq)
#   
#   return(result_fft)
# }
# 
# # Function: make and save periodogram ####
# 
# plot_periodogram <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05, path = plot_path){
#   # todo: get local rule or set values for period upper and lower lim
#   periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + geom_line(aes(x = period, y = spec), colour = "black") + theme_bw() +
#     scale_y_continuous(expand = c(0,0)) +
#     scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
#     labs(y = "spectral density", x = "period in hours", title = paste0("tag ", tag_serial_number_short))
#   
#   ggplot2::ggsave(filename = paste0(plot_path, deparse(substitute(periodogram)), ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
#   return(periodogram)
# }
# 
# 
# # test out functions ####
# sample_frequency <- 30 # 1 sample per 2 min == 30 samples per 60 min == 30 [1/hour]
# 
# fft_295 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293295"),
#                     sample_freq = 30)
# 
# periodogram_295 <- plot_periodogram(fft_result = result_fft,
#                                     tag_serial_number_short = "295")
# 
# # FFT CALCS FOR THE DST TAGS: for tag 295 ####
# 
# data_fft_295 <- masterias_depth_temp %>% filter(tag_serial_number == "1293295") %>% mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric())
# 
# ggplot(data = data_fft_295) + geom_point(aes(x = t, y = -depth_m)) + theme_minimal()
# 
# fft_295 <- calc_fft(time_vector = data_fft_295$t, 
#                     data = -data_fft_295$depth_m,
#                     sample_freq = 1/30)
# 
# periodogram_295 <- ggplot(data = fft_295 %>% filter(period < 55 & period > 0.05)) + geom_line(aes(x = period, y = spec), colour = "red") + theme_bw() +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_continuous(expand = c(0,0)) +
#   labs(y = "magnitude", x = "period in hours", title = "tag 295")
# 
# ggplot2::ggsave(filename = paste0(plot_path, deparse(substitute(periodogram_295)), ".pdf"), plot = periodogram_295, width = 18, height = 12, units = "cm")
# 
# plotly::ggplotly(periodogram_295)
# 
# 
# # TAG 304 ####
# 
# data_fft_304 <- masterias_depth_temp %>% filter(tag_serial_number == "1293304") %>% mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric())
# 
# ggplot(data = data_fft_304) + geom_point(aes(x = t, y = -depth_m)) + theme_minimal()
# 
# data_fft_304 <- data_fft_304 %>% filter(t < 300)
# 
# fft_304 <- calc_fft(time_vector = data_fft_304$t, 
#                     data = -data_fft_304$depth_m,
#                     sample_freq = 1/30)
# 
# plot_fft_304 <- ggplot(data = fft_304 %>% filter(period < 100 & period > 0.05)
#                        ) + geom_line(aes(x = period, y = spec), colour = "red") + theme_minimal() +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(y = "magnitude", x = "period", title = "tag 304")
# plotly::ggplotly(plot_fft_304)
# 
# # TAG 310 ####
# 
# data_fft_310 <- masterias_depth_temp %>% filter(tag_serial_number == "1293310") %>% mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric())
# 
# ggplot(data = data_fft_310) + geom_point(aes(x = t, y = -depth_m)) + theme_minimal()
# 
# data_fft_310 <- data_fft_310 %>% filter(t < 900)
# 
# fft_310 <- calc_fft(time_vector = data_fft_310$t, 
#                     data = -data_fft_310$depth_m,
#                     sample_freq = 1/30)
# 
# plot_fft_310 <- ggplot(data = fft_310 %>% filter(period < 40 & period > 0.05)) + geom_line(aes(x = period, y = spec), colour = "red") + theme_minimal() +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(y = "magnitude", x = "period", title = "tag 310")
# plotly::ggplotly(plot_fft_310)
# 
# # TAG 312 ####
# 
# data_fft_312 <- masterias_depth_temp %>% filter(tag_serial_number == "1293312" & !is.na(depth_m)) %>% mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric())
# 
# ggplot(data = data_fft_312) + geom_point(aes(x = t, y = -depth_m)) + theme_minimal()
# 
# data_fft_312 <- data_fft_312 %>% filter(t < 750)
# 
# fft_312 <- calc_fft(time_vector = data_fft_312$t, 
#                     data = -data_fft_312$depth_m,
#                     sample_freq = 1/30)
# 
# plot_fft_312 <- ggplot(data = fft_312 %>% filter(period < 40 & period > 0.05)
#                        ) + geom_line(aes(x = period, y = spec), colour = "red") + theme_minimal() +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(y = "magnitude", x = "period", title = "tag 312")
# plotly::ggplotly(plot_fft_312)
# 
# # TAG 319 ####
# 
# data_fft_319 <- masterias_depth_temp %>% filter(tag_serial_number == "1293319" & !is.na(depth_m)) %>% mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric())
# 
# ggplot(data = data_fft_319) + geom_point(aes(x = t, y = -depth_m)) + theme_minimal()
# 
# data_fft_319 <- data_fft_319 %>% filter(t < 550)
# 
# fft_319 <- calc_fft(time_vector = data_fft_319$t, 
#                     data = -data_fft_319$depth_m,
#                     sample_freq = 1/30)
# 
# plot_fft_319 <- ggplot(data = fft_319 %>% filter(period < 40 & period > 0.05)
# ) + geom_line(aes(x = period, y = spec), colour = "red") + theme_minimal() +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(y = "magnitude", x = "period", title = "tag 319")
# plotly::ggplotly(plot_fft_319)
