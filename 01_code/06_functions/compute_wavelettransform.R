# Script to conduct and plot wavelet analyses

library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(tidyverse)
library(purrr)
library(RColorBrewer)

# rm(list = ls())

# paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# source(paste0(getwd(), "/01_code/02_load_data/load_dst_summarystatistics.R"))

compute_wavelet <- function(parameter, dt, factor_smallest_scale){
  # make time vector according to nrow() of parameter and given dt
  timevector <- seq(from = 0, to = (nrow(parameter) * dt) - dt, by = dt)
  wt_input <- cbind(timevector, parameter) %>% as.matrix()
  # make wt result
  wt_output <- biwavelet::wt(d = wt_input,
                             dt = dt,
                             do.sig = T,
                             s0 = factor_smallest_scale * dt) # this specifies the scale at which periods are looked at
  
  return(wt_output)
}
# test with 321 date vector
# dates <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date) 
# t1 <- cbind(1:100, rnorm(100))
# wt_output <- t1 %>% wt()
# dates <- t1[,1] %>% as.data.frame()

wavelet_output_compare <- function(dates, wt_output){
  dates <- dates %>% `colnames<-`("date") %>% mutate(t = seq(from = 1, to = nrow(dates)),
                                                     date_posicxt = date,
                                                     date = date %>% as.character())
  # extract important results
  period <- wt_output$period %>% as.data.frame() %>% `colnames<-`("period")
  xaxis <- wt_output$xaxis %>% as.data.frame() %>% `colnames<-`("time")
  signif <- wt_output$signif %>% as.data.frame() %>%
    purrr::set_names(as.character(dates$date)) %>%
    cbind(period) %>%
    pivot_longer(cols = -last_col(offset = 0), names_to = "date") %>% #don't pivot the two last columns
    rename(significance = value)
  # wt_df <- wt_output$power.corr %>% as.data.frame() %>%
  wt_df <- wt_output$power %>% as.data.frame() %>%
    purrr::set_names(as.character(dates$date)) %>%
    cbind(period) %>%
    # mutate(period_log = log2(period))# %>%
    # arrange(desc(period)) %>%
    mutate(height = log2(period) - log2(dplyr::lead(period)),
           height = height + 0.15) 
  wt_df$height[wt_df$height %>% is.na()] <- wt_df$period[nrow(wt_df) - 1] - wt_df$period[nrow(wt_df)] # fill the NA created by `dplyr::lead()`
  wt_df <- wt_df %>%
    pivot_longer(cols = -c(last_col(offset = 1), last_col(offset = 0)), names_to = "date") %>% #don't pivot the two last columns
    rename(power = value) %>%
    # relocate(date, period, power) %>%
    left_join(signif, by = join_by(period, date), multiple = "all") %>%
    mutate(date = date, # %>% as.POSIXct()
           # power_scaled = power %>% scale(),
           power_log = log2(power),
           power_log_scale = power_log %>% scale(),
           period_log = log2(period)) %>%
    left_join(dates, by = "date", multiple = "all") %>%
    mutate(t = sprintf("%03d", t %>% as.numeric()),
           sig = ifelse(significance >= 1, 1, 0))
  
  return(wt_df)
}

# wt_ggplot <- make_wavelet_result_ggplot_obj(wt_output = wt_output) %>%
#   mutate(power_raw = power,
#          power = power %>% log2() %>% scale(),
#          period_log = period %>% log2(),
#          t = sprintf("%03d", t %>% as.numeric())) %>%
#   left_join(wt_df %>% dplyr::select(date, significance), by = join_by(t == date), multiple = "all")


# # test <- wt_df %>%
# #   mutate(t = seq(from = 1, to = nrow(wt_df), by = 1)) %>% 
# #   filter(
# #     # t %>% between(200, 310),
# #          period %>% between(2, 16))
# 
# p <- ggplot(data = wt_df) +
#   geom_point(aes(x = t, y = significance)) +
#   # geom_line(aes(x = t, y = 0.05), colour = "red") +
#   scale_x_continuous(n.breaks = 6) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# p %>% ggplotly()
# 
# # plot(x = wt_df$t, y = wt_df$period, z= wt_df$significance, type = "b")
# 
# test_plot <- ggplot(data = test) +
#   # geom_tile(aes(x = t, y = period, fill = significance),
#   #           # height = 5,
#   #           position = "identity") +
#   geom_histogram(aes(significance)) +
#   # scale_y_reverse(expand =c(0,0)) +
#   # scale_x_continuous(expand =c(0,0)) +
#   # scale_fill_viridis_c() +
#   # labs(x = "Date", y = "Period", fill = "Significance") +
#   theme_bw() #+
#   # theme(axis.text.x = element_text(angle = 60, hjust = 1))
# test_plot #%>% ggplotly()


make_wavelet_result_ggplot_obj <- function(wt_output){
  # extract important results
  period <- wt_output$period %>% as.data.frame() %>% `colnames<-`("period")
  xaxis <- wt_output$xaxis %>% as.data.frame() %>% `colnames<-`("time")
  signif <- wt_output$signif %>% as.data.frame() %>%
    purrr::set_names(as.character(dates$date)) %>%
    cbind(period) %>%
    pivot_longer(cols = -last_col(offset = 0), names_to = "date") %>% #don't pivot the two last columns
    rename(significance = value)
  wt_ggplot_df <- wt_output$power.corr %>% as.data.frame() %>%
    purrr::set_names(as.character(xaxis$time)) %>%
    # `colnames <-` as.character(xaxis$time)
    cbind(period) %>%
    arrange(desc(period)) %>%
    mutate(height = period - dplyr::lead(period),
           height = height + 0.15
           # ,
           # height = height %>% replace_na(wt_ggplot_df$height[nrow(wt_ggplot_df) - 1]),
           # height = height
           ) #%>%  + 0.15
  wt_ggplot_df$height[wt_ggplot_df$height %>% is.na()] <- wt_ggplot_df$period[nrow(wt_ggplot_df) - 1] - wt_ggplot_df$period[nrow(wt_ggplot_df)] # fill the NA created by `dplyr::lead()`
  wt_ggplot_df <- wt_ggplot_df %>%
    pivot_longer(cols = -c(last_col(offset = 1), last_col(offset = 0)), names_to = "t") %>% #don't pivot the two last columns
    rename(power = value)
  
  
  # wt_df <- wt_output$power %>% as.data.frame() %>%
  #   purrr::set_names(as.character(dates$date)) %>%
  #   cbind(period) %>%
  #   pivot_longer(cols = -last_col(offset = 0), names_to = "date") %>% #don't pivot the two last columns
  #   rename(power = value) %>%
  #   relocate(date, period, power) %>%
  #   left_join(signif, by = join_by(period, date), multiple = "all") %>%
  #   mutate(date = date, # %>% as.POSIXct()
  #          # power_scaled = power %>% scale(),
  #          power_norm = log2(power),
  #          period_norm = log2(period)) %>%
  #   left_join(dates, by = "date", multiple = "all")
  
  return(wt_ggplot_df)
}

# tests ####
# 
# wt_321_date_depthmedian <- compute_wavelet(parameter = masterias_depth_date %>% ungroup() %>% filter(tag_serial_number == "1293321") %>% # wt_321_daynight_depthmedian
#                                              dplyr::select(depth_median),
#                                            dt = 0.5,
#                                            factor_smallest_scale = 1 # 0.5 day period minimum
# )
# 
# plot_wt_321_date_depthmedian <- make_wavelet_result_ggplot_obj(wt_321_date_depthmedian)
# 
# # 
# test_plot <- ggplot(data = wt_df) +
#   geom_tile(aes(x = date, y = period, fill = frequency_scaled),
#             height = 50,
#             position = "identity") +
#   scale_y_reverse(expand =c(0,0)) +
#   # scale_x_continuous(expand =c(0,0)) +
#   scale_fill_viridis_c() +
#   labs(x = "Date", y = "Period", fill = "Frequency") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# test_plot #%>% ggplotly()
# 
# 
# # comparison to the biwavelet plots
# 
# par(mfcol = c(1,1))
# plot(wt_output,
#      type = "power.corr", main = "", tol = 0.5
#      # , #power.corr.norm
#      # fill.cols = brewer.pal(11, "RdYlGn")
#      )
# # plot(x = data_321_summary_wt_all$date, y = -data_321_summary_wt_all$depth_median, type = "l")
