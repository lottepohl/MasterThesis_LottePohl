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
    arrange(desc(period)) %>%
    mutate(height = period - dplyr::lead(period), #height = log2(period) - dplyr::lead(log2(period)
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
           sig = ifelse(significance >= 1, 1, 0),
           height2 = period - dplyr::lead(period), #height = log2(period) - dplyr::lead(log2(period)
           height2 = height2 + 0.15)
  wt_df$height2[wt_df$height2 %>% is.na()] <- wt_df$period[nrow(wt_df) - 1] - wt_df$period[nrow(wt_df)] # fill the NA created by `dplyr::lead()`

  return(wt_df)
}
# 
# plot_wavelet_gg2 <- function(wt_df, type = c("power", "significance", "power_log"),
#                             # y_breaks = c(4, 8, 16, 32, 64, 128),
#                             # x_breaks = c("000", "100", "200", "300", "400", "500"),
#                             date = TRUE){
#   # transformation function for the y axis
#   my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
#   
#   n_data <- wt_df$t %>% unique %>% length()
#   
#   # y axis labels
#   y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
#   
#   # x axis labels
#   ifelse(date %>% isTRUE(),
#          x_breaks <- c(wt_df$date[1], wt_df$date[(1/5) * n_data], wt_df$date[(2/5) * n_data], wt_df$date[(3/5) * n_data],
#                             wt_df$date[(4/5) * n_data], wt_df$date[(5/5) * n_data])
#          ,
#          x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
# 
#   #plot
#   ifelse(date %>% isTRUE(),
# 
#          ifelse(type == "power_log",
#                 
#                 plot <- ggplot(data = wt_df) +
#                   geom_tile(aes(x = date, y = period, fill = power_log),
#                             position = "identity",
#                             alpha = 0.65) +
#                   geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power_log),
#                             position = "identity") +
#                   scale_y_continuous(trans = my_trans,
#                                      breaks = y_breaks, 
#                                      expand = c(0,0)) +
#                   scale_x_discrete(breaks = x_breaks) +
#                   scale_fill_viridis_c(direction = 1, option = "turbo") +
#                   labs(x = "dates", y = "period in hours", fill = "log2(power)") #+
#                   # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                 ,
#                 
#                 ifelse(type == "significance",
#                        
#                        plot <- ggplot(data = wt_df) +
#                          geom_tile(aes(x = date, y = period, fill = significance),
#                                    position = "identity",
#                                    alpha = 0.65) +
#                          geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = significance),
#                                    position = "identity") +
#                          scale_y_continuous(trans = my_trans,
#                                             breaks = y_breaks, 
#                                             expand = c(0,0)) +
#                          scale_x_discrete(breaks = x_breaks) +
#                          scale_fill_viridis_c(direction = 1, option = "turbo") +
#                          labs(x = "dates", y = "period in hours", fill = "significance") #+
#                          # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                        ,
#                        
#                        plot <- ggplot(data = wt_df) +
#                          geom_tile(aes(x = date, y = period, fill = power),
#                                    position = "identity",
#                                    alpha = 0.65) +
#                          geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power),
#                                    position = "identity") +
#                          scale_y_continuous(trans = my_trans,
#                                             breaks = y_breaks, 
#                                             expand = c(0,0)) +
#                          scale_x_discrete(breaks = x_breaks) +
#                          scale_fill_viridis_c(direction = 1, option = "turbo") +
#                          labs(x = "dates", y = "period in hours", fill = "power") #+
#                          # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                        
#                        )
#                 )
#          ,
#          
#          ifelse(type == "power_log",
#                 
#                 plot <- ggplot(data = wt_df) +
#                   geom_tile(aes(x = t, y = period, fill = power_log),
#                             position = "identity",
#                             alpha = 0.65) +
#                   geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log),
#                             position = "identity") +
#                   scale_y_continuous(trans = my_trans,
#                                      breaks = y_breaks, 
#                                      expand = c(0,0)) +
#                   scale_x_discrete(breaks = x_breaks) +
#                   scale_fill_viridis_c(direction = 1, option = "turbo") +
#                   labs(x = "time in days", y = "period in hours", fill = "log2(power)")# +
#                   # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                 ,
#                 
#                 ifelse(type == "significance",
#                        
#                        plot <- ggplot(data = wt_df) +
#                          geom_tile(aes(x = t, y = period, fill = significance),
#                                    position = "identity",
#                                    alpha = 0.65) +
#                          geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = significance),
#                                    position = "identity") +
#                          scale_y_continuous(trans = my_trans,
#                                             breaks = y_breaks, 
#                                             expand = c(0,0)) +
#                          scale_x_discrete(breaks = x_breaks) +
#                          scale_fill_viridis_c(direction = 1, option = "turbo") +
#                          labs(x = "time in days", y = "period in hours", fill = "significance")# +
#                          # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                        ,
#                        
#                        plot <- ggplot(data = wt_df) +
#                          geom_tile(aes(x = t, y = period, fill = power),
#                                    position = "identity",
#                                    alpha = 0.65) +
#                          geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power),
#                                    position = "identity") +
#                          scale_y_continuous(trans = my_trans,
#                                             breaks = y_breaks, 
#                                             expand = c(0,0)) +
#                          scale_x_discrete(breaks = x_breaks) +
#                          scale_fill_viridis_c(direction = 1, option = "turbo") +
#                          labs(x = "time in days", y = "period in hours", fill = "power") #+
#                          # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                        )
#                 )
#          )
#   
#   return(plot)
# }
# 
# # plot #%>% ggplotly()
# 
# 
# 
# plot_wavelet_gg <- function(wt_df, type = c("power", "significance", "power_log"),
#                             y_breaks = c(4, 8, 16, 32, 64, 128),
#                             x_breaks = c("000", "100", "200", "300", "400"),
#                             xval){
#   my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
#   ifelse(type == "power_log",
#          plot <- ggplot(data = wt_df) +
#            geom_tile(aes(x = xval, y = period, fill = power_log, height = height), #, colour = sig
#                      position = "identity"
#            ) +
#            # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, height = height + 0.15), #
#            #           fill = "black",
#            #           width = 3,
#            #           # height = ,
#            #           position = "identity") +
#            # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, fill = power_log, height = height),
#            #           position = "identity") +
#            
#            # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = xval, y = period, height = height),
#            #           fill = "white",
#            #           alpha = 0.3) +
#          # geom_tile(aes(x = xval, y = period, height = height, fill = sig),
#          #           alpha = 0) +
#          scale_y_continuous(trans = my_trans,
#                             breaks = y_breaks, 
#                             expand = c(0,0)) +
#            # scale_y_reverse(expand = c(0,0)) +
#            # scale_y_continuous(trans = "log2", expand = c(0,0)) +
#            scale_x_discrete(breaks = x_breaks,
#                             expand = c(0,0)) +
#            scale_fill_viridis_c(direction = 1, option = "turbo") +
#            labs(x = "Date", y = "period in hours", fill = "log2(Power)") + #
#            # theme_bw() +
#            theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#          , 
#          ifelse(type == "significance",
#                 plot <- ggplot(data = wt_df) +
#                   geom_tile(aes(x = xval, y = period, fill = significance, height = height), #, colour = sig
#                             position = "identity"
#                   ) +
#                   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, height = height + 0.15), #
#                   #           fill = "black",
#                   #           width = 3,
#                   #           # height = ,
#                   #           position = "identity") +
#                   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, fill = power_log, height = height),
#                   #           position = "identity") +
#                   
#                   # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = xval, y = period, height = height),
#                   #           fill = "white",
#                   #           alpha = 0.3) +
#                 # geom_tile(aes(x = xval, y = period, height = height, fill = sig),
#                 #           alpha = 0) +
#                 scale_y_continuous(trans = my_trans,
#                                    breaks = y_breaks, 
#                                    expand = c(0,0)) +
#                   # scale_y_reverse(expand = c(0,0)) +
#                   # scale_y_continuous(trans = "log2", expand = c(0,0)) +
#                   scale_x_discrete(breaks = x_breaks,
#                                    expand = c(0,0)) +
#                   scale_fill_viridis_c(direction = 1, option = "turbo") +
#                   labs(x = "Date", y = "period in hours", fill = "Significance Level") + #
#                   # theme_bw() +
#                   theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#                 ,
#                 plot <- ggplot(data = wt_df) +
#                   geom_tile(aes(x = xval, y = period, fill = power, height = height), #, colour = sig
#                             position = "identity"
#                   ) +
#                   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, height = height + 0.15), #
#                   #           fill = "black",
#                   #           width = 3,
#                   #           # height = ,
#                   #           position = "identity") +
#                   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = xval, y = period, fill = power_log, height = height),
#                   #           position = "identity") +
#                   
#                   # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = xval, y = period, height = height),
#                   #           fill = "white",
#                   #           alpha = 0.3) +
#                 # geom_tile(aes(x = xval, y = period, height = height, fill = sig),
#                 #           alpha = 0) +
#                 scale_y_continuous(trans = my_trans,
#                                    breaks = y_breaks, 
#                                    expand = c(0,0)) +
#                   # scale_y_reverse(expand = c(0,0)) +
#                   # scale_y_continuous(trans = "log2", expand = c(0,0)) +
#                   scale_x_discrete(breaks = x_breaks,
#                                    expand = c(0,0)) +
#                   scale_fill_viridis_c(direction = 1, option = "turbo") +
#                   labs(x = "Date", y = "log2(Period)", fill = "Power") + #
#                   # theme_bw() +
#                   theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#          )
#   )
#   
#   # plot <- ggplot(data = wt_df #%>% filter(date %>% between(dates1$t[1], dates1$t[nrow(dates1) - 1]))
#   # ) +
#   #   geom_tile(aes(x = xval, y = period, fill = ifelse(type == "power_log", power_log,
#   #                                                  ifelse(type == "significance", significance,
#   #                                                         power)), 
#   #                 height = height), #, colour = sig
#   #             position = "identity"
#   #   ) +
#   #   # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = xval, y = period, height = height + 0.15), #
#   #   #           fill = "black",
#   #   #           width = 3,
#   #   #           # height = ,
#   #   #           position = "identity") +
#   #   # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = xval, y = period, fill = power_log, height = height),
#   #   #           position = "identity") +
#   #   
#   #   # geom_tile(data = wt_df4 %>% filter(significance >= 1), aes(x = xval, y = period, height = height),
#   #   #           fill = "white",
#   #   #           alpha = 0.3) +
#   # # geom_tile(aes(x = xval, y = period, height = height, fill = sig),
#   # #           alpha = 0) +
#   # scale_y_continuous(trans = my_trans,
#   #                    breaks = y_breaks, 
#   #                    expand = c(0,0)) +
#   #   # scale_y_reverse(expand = c(0,0)) +
#   #   # scale_y_continuous(trans = "log2", expand = c(0,0)) +
#   #   scale_x_discrete(breaks = x_breaks,
#   #                    expand = c(0,0)) +
#   #   scale_fill_viridis_c(direction = 1, option = "turbo") +
#   #   labs(x = "Date", y = "log2(Period)", fill = ifelse(type == "power_log", "log2(Power)", type)) + #
#   #   # theme_bw() +
#   #   theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
#   
#   return(plot) #%>% plotly::ggplotly()
# }
# 
# 
# 
# # wt_ggplot <- make_wavelet_result_ggplot_obj(wt_output = wt_output) %>%
# #   mutate(power_raw = power,
# #          power = power %>% log2() %>% scale(),
# #          period_log = period %>% log2(),
# #          t = sprintf("%03d", t %>% as.numeric())) %>%
# #   left_join(wt_df %>% dplyr::select(date, significance), by = join_by(t == date), multiple = "all")
# 
# 
# # # test <- wt_df %>%
# # #   mutate(t = seq(from = 1, to = nrow(wt_df), by = 1)) %>% 
# # #   filter(
# # #     # t %>% between(200, 310),
# # #          period %>% between(2, 16))
# # 
# # p <- ggplot(data = wt_df) +
# #   geom_point(aes(x = t, y = significance)) +
# #   # geom_line(aes(x = t, y = 0.05), colour = "red") +
# #   scale_x_continuous(n.breaks = 6) +
# #   theme_minimal() +
# #   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# # p %>% ggplotly()
# # 
# # # plot(x = wt_df$t, y = wt_df$period, z= wt_df$significance, type = "b")
# # 
# # test_plot <- ggplot(data = test) +
# #   # geom_tile(aes(x = t, y = period, fill = significance),
# #   #           # height = 5,
# #   #           position = "identity") +
# #   geom_histogram(aes(significance)) +
# #   # scale_y_reverse(expand =c(0,0)) +
# #   # scale_x_continuous(expand =c(0,0)) +
# #   # scale_fill_viridis_c() +
# #   # labs(x = "Date", y = "period in hours", fill = "Significance") +
# #   theme_bw() #+
# #   # theme(axis.text.x = element_text(angle = 60, hjust = 1))
# # test_plot #%>% ggplotly()
# 
# 
# make_wavelet_result_ggplot_obj <- function(wt_output){
#   # extract important results
#   period <- wt_output$period %>% as.data.frame() %>% `colnames<-`("period")
#   xaxis <- wt_output$xaxis %>% as.data.frame() %>% `colnames<-`("time")
#   signif <- wt_output$signif %>% as.data.frame() %>%
#     purrr::set_names(as.character(dates$date)) %>%
#     cbind(period) %>%
#     pivot_longer(cols = -last_col(offset = 0), names_to = "date") %>% #don't pivot the two last columns
#     rename(significance = value)
#   wt_ggplot_df <- wt_output$power.corr %>% as.data.frame() %>%
#     purrr::set_names(as.character(xaxis$time)) %>%
#     # `colnames <-` as.character(xaxis$time)
#     cbind(period) %>%
#     arrange(desc(period)) %>%
#     mutate(height = period - dplyr::lead(period),
#            height = height + 0.15
#            # ,
#            # height = height %>% replace_na(wt_ggplot_df$height[nrow(wt_ggplot_df) - 1]),
#            # height = height
#            ) #%>%  + 0.15
#   wt_ggplot_df$height[wt_ggplot_df$height %>% is.na()] <- wt_ggplot_df$period[nrow(wt_ggplot_df) - 1] - wt_ggplot_df$period[nrow(wt_ggplot_df)] # fill the NA created by `dplyr::lead()`
#   wt_ggplot_df <- wt_ggplot_df %>%
#     pivot_longer(cols = -c(last_col(offset = 1), last_col(offset = 0)), names_to = "t") %>% #don't pivot the two last columns
#     rename(power = value)
#   
#   
#   # wt_df <- wt_output$power %>% as.data.frame() %>%
#   #   purrr::set_names(as.character(dates$date)) %>%
#   #   cbind(period) %>%
#   #   pivot_longer(cols = -last_col(offset = 0), names_to = "date") %>% #don't pivot the two last columns
#   #   rename(power = value) %>%
#   #   relocate(date, period, power) %>%
#   #   left_join(signif, by = join_by(period, date), multiple = "all") %>%
#   #   mutate(date = date, # %>% as.POSIXct()
#   #          # power_scaled = power %>% scale(),
#   #          power_norm = log2(power),
#   #          period_norm = log2(period)) %>%
#   #   left_join(dates, by = "date", multiple = "all")
#   
#   return(wt_ggplot_df)
# }
# 
# tests ####
# # 
# # wt_321_date_depthmedian <- compute_wavelet(parameter = masterias_depth_date %>% ungroup() %>% filter(tag_serial_number == "1293321") %>% # wt_321_daynight_depthmedian
# #                                              dplyr::select(depth_median),
# #                                            dt = 0.5,
# #                                            factor_smallest_scale = 1 # 0.5 day period minimum
# # )
# # 
# # plot_wt_321_date_depthmedian <- make_wavelet_result_ggplot_obj(wt_321_date_depthmedian)
# # 
# # # 
# # test_plot <- ggplot(data = wt_df) +
# #   geom_tile(aes(x = date, y = period, fill = frequency_scaled),
# #             height = 50,
# #             position = "identity") +
# #   scale_y_reverse(expand =c(0,0)) +
# #   # scale_x_continuous(expand =c(0,0)) +
# #   scale_fill_viridis_c() +
# #   labs(x = "Date", y = "period in hours", fill = "Frequency") +
# #   theme_bw() +
# #   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# # test_plot #%>% ggplotly()
# # 
# # 
# # # comparison to the biwavelet plots
# # 
# # par(mfcol = c(1,1))
# # plot(wt_output,
# #      type = "power.corr", main = "", tol = 0.5
# #      # , #power.corr.norm
# #      # fill.cols = brewer.pal(11, "RdYlGn")
# #      )
# # # plot(x = data_321_summary_wt_all$date, y = -data_321_summary_wt_all$depth_median, type = "l")
