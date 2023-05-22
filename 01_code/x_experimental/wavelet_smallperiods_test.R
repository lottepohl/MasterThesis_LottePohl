


library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(tidyverse)
library(purrr)
library(RColorBrewer)

# rm(list = ls())

paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# source(paste0(getwd(), "/01_code/02_load_data/load_dst_summarystatistics.R"))

# I now have to change the unit from days to hours, bc that is what I want to have my periods in.
dt_smallperiods <-  0.5 #hour, for now: but that also means that I have to downsample the raw depthlog to 1 value per hour

# depthlog_308 <- masterias_depth_temp %>% 
#   ungroup() %>%
#   filter(tag_serial_number == "1293308",
#          row_number() %% ((dt_smallperiods * 60) / 2) == 0) # explanation: ... %% dt[hour] * 60[min] / 2[min] <- sample interval (every 2 min), e.g. for dt_smallperiods = 1: ...%% 30 (i.e. get every 30th val)
# 

compute_wavelet_hrperiod <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308", parameter_name = "depth_m", dt = dt_smallperiods, factor_smallest_scale = 8){
  # downsample raw depthlog
  depthlog_downsampled <- depthlog %>% 
    ungroup() %>%
    filter(tag_serial_number == tag_serial_num,
           row_number() %% ((dt_smallperiods * 60) / 2) == 0) %>%
    dplyr::select(parameter_name)# explanation: ... %% dt[hour] * 60[min] / 2[min] <- sample interval (every 2 min), e.g. for dt_smallperiods = 1: ...%% 30 (i.e. get every 30th val)

  # make time vector according to nrow() of parameter and given dt
  timevector <- seq(from = 0, to = (nrow(depthlog_downsampled) * dt) - dt, by = dt)
  wt_input <- cbind(timevector, depthlog_downsampled) %>% as.matrix()
  # make wt result
  wt_output <- biwavelet::wt(d = wt_input,
                             dt = dt,
                             do.sig = T,
                             s0 = factor_smallest_scale * dt) # this specifies the scale at which periods are looked at
  
  return(wt_output)
}

wavelet_output_compare_hrperiod <- function(depthlog = masterias_depth_temp, tag_serial_num = "1293308", parameter_name = "depth_m", dt = dt_smallperiods, wt_output = wt_output){
  
  # downsample raw depthlog
  depthlog_downsampled <- depthlog %>% 
    ungroup() %>%
    filter(tag_serial_number == tag_serial_num,
           row_number() %% ((dt_smallperiods * 60) / 2) == 0) %>%
    dplyr::select(c(parameter_name, date_time))# explanation: ... %% dt[hour] * 60[min] / 2[min] <- sample interval (every 2 min), e.g. for dt_smallperiods = 1: ...%% 30 (i.e. get every 30th val)
  
  # make time vector according to nrow() of parameter and given dt
  timevector <- seq(from = 0, to = (nrow(depthlog_downsampled) * dt) - dt, by = dt)
  
  dates <- depthlog_downsampled %>% dplyr::select(date_time) %>% 
    mutate(t = timevector,
           date = date_time %>% lubridate::date() %>% as.POSIXct.Date(), #sum by day
           date_posicxt = date_time,
           date_time = date_time %>% as.character())
  # extract important results
  period <- wt_output$period %>% as.data.frame() %>% `colnames<-`("period")
  xaxis <- wt_output$xaxis %>% as.data.frame() %>% `colnames<-`("time")
  signif <- wt_output$signif %>% as.data.frame() %>%
    purrr::set_names(as.character(dates$date_time)) %>%
    # purrr::set_names(as.character(dates$date)) %>% #sum by day, hier noch nicht
    cbind(period) %>%
    pivot_longer(cols = -last_col(offset = 0), names_to = "date_time") %>% #don't pivot the two last columns
    rename(significance = value)
  # wt_df <- wt_output$power.corr %>% as.data.frame() %>%
  wt_df <- wt_output$power %>% as.data.frame() %>%
    purrr::set_names(as.character(dates$date_time)) %>%
    # purrr::set_names(as.character(dates$date)) %>% #sum by day
    cbind(period) %>%
    # mutate(period_log = log2(period))# %>%
    arrange(desc(period))
  wt_df <- wt_df %>%
    # pivot_longer(cols = -c(last_col(offset = 1), last_col(offset = 0)), names_to = "date_time") %>% #don't pivot the two last columns
    pivot_longer(cols = -last_col(offset = 0), names_to = "date_time") %>%
    rename(power = value) %>%
    # relocate(date, period, power) %>%
    left_join(signif, by = join_by(period, date_time), multiple = "all") %>%
    left_join(dates, by = "date_time", multiple = "all") %>%
    group_by(date, period) %>%
    summarise(power = power %>% max(),
              significance = significance %>% max(),
              t = t %>% max()) %>%
    mutate(
      # date_time = date_time, # %>% as.POSIXct()
           # power_scaled = power %>% scale(),
           power_log = log2(power),
           power_log_scale = power_log %>% scale(),
           period_log = log2(period)) %>%
    mutate(t = sprintf("%04f", t %>% as.numeric()),
           sig = ifelse(significance >= 1, 1, 0))

  return(wt_df)
}

## do wavelet analysis ####

plot_wavelet_hrperiod <- function(wt_df = wt_df, type = c("power", "significance", "power_log"),
                         date = TRUE, max_period = 72 #hours = 3 days
                         ){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  # n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  y_breaks <- y_breaks[y_breaks <= max_period]
  # transform dates ####
  # wt_df$date_time <- wt_df$date_time %>% as.POSIXct(tz = "UTC") # not needed bc we have the posict objects
  
  # # x axis labels
  # ifelse(date %>% isTRUE(),
  #        x_breaks <- c(wt_df$date_time[1], wt_df$date_time[(1/5) * n_data], wt_df$date_time[(2/5) * n_data], wt_df$date_time[(3/5) * n_data],
  #                      wt_df$date_time[(4/5) * n_data], wt_df$date_time[(5/5) * n_data])
  # ,
  # x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
  # 
  #plot
  
  wt_df <- wt_df  %>% filter(period <= max_period)
  
  # change max and min date to include max x axis label completely
  max_date <- max(wt_df$date) + lubridate::days(10)
  min_date <- min(wt_df$date) -lubridate::days(10)
  
  
  ifelse(date %>% isTRUE(),
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = date, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.35) +
                  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_x_datetime(
                    # date_minor_breaks = "1 month",
                                   date_breaks = "1 month",
                                   date_labels = "%b %Y",
                                   expand = c(0,0),
                                   limits = c(min_date, max_date)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "", y = "Period in hours", fill = "log2(Power)") +
                  theme(legend.position = "bottom", # "bottom",
                        legend.box = "horizontal") +
                theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date_posicxt, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date_posicxt, y = period, fill = significance),
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
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date_posicxt, y = period, fill = power),
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



### dates ####
# dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
# dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

#### compute wavelets ####

# tag 321 ####

wt_321_depth_hr <- compute_wavelet_hrperiod(depthlog = masterias_depth_temp,
                                            tag_serial_num = "1293321",
                                            parameter_name = "depth_m",
                                            dt = dt_smallperiods,
                                            factor_smallest_scale = 8)

wt_df_321_depth_hr <- wavelet_output_compare_hrperiod(depthlog = masterias_depth_temp,
                                                      tag_serial_num = "1293321",
                                                      parameter_name = "depth_m",
                                                      dt = dt_smallperiods,
                                                      wt_output = wt_321_depth_hr)

p_wt_321_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_321_depth_hr,
                                           type = "power_log",
                                           date = TRUE,
                                           max_period = 72)

## tag 308 ####

wt_308_depth_hr <- compute_wavelet_hrperiod(depthlog = masterias_depth_temp,
                                            tag_serial_num = "1293308",
                                            parameter_name = "depth_m",
                                            dt = dt_smallperiods,
                                            factor_smallest_scale = 8)

wt_df_308_depth_hr <- wavelet_output_compare_hrperiod(depthlog = masterias_depth_temp,
                                                      tag_serial_num = "1293308",
                                                      parameter_name = "depth_m",
                                                      dt = dt_smallperiods,
                                                      wt_output = wt_308_depth_hr)

p_wt_308_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_308_depth_hr,
                                           type = "power_log",
                                           date = TRUE,
                                           max_period = 72)
