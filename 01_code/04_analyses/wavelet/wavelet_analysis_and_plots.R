# Script to conduct and plot wavelet analyses

library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(tidyverse)
library(purrr)
library(RColorBrewer)
# library(zoo)

# rm(list = ls())

# paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
source(paste0(getwd(), "/01_code/02_load_data/load_dst_summarystatistics.R"))

par(mfcol = c(2,1))
plot(wt_321_date_depthmedian, 
     type = "power.corr", main = "t = day, depth = raw, s0 = 5 * dt, do.sig = T"
     # , #power.corr.norm
     # fill.cols = brewer.pal(11, "RdYlGn")
     )
plot(x = data_321_summary_wt_all$date, y = -data_321_summary_wt_all$depth_median, type = "l")

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

  
wt_321_date_depthmedian <- compute_wavelet(parameter = masterias_depth_date %>% ungroup() %>% filter(tag_serial_number == "1293321") %>% # wt_321_daynight_depthmedian
                                                 dplyr::select(depth_median),
                                               dt = 0.5,
                                               factor_smallest_scale = 1 # 0.5 day period minimum
                                               )
# wt_output <- wt_321_date_depthmedian

make_wavelet_result_ggplot_obj <- function(wt_output){
  # extract important results
  period <- wt_output$period %>% as.data.frame() %>% `colnames<-`("period")
  xaxis <- wt_output$xaxis %>% as.data.frame() %>% `colnames<-`("time")
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
    rename(frequency = value)
  
  return(wt_ggplot_df)
}

plot_wt_321_date_depthmedian <- make_wavelet_result_ggplot_obj(wt_321_date_depthmedian)

test_plot <- ggplot(data = wt_ggplot_df) +
  geom_tile(aes(x = t, y = period, fill = frequency, height = height), #, height = height , colour = frequency
            position = "identity") +
  scale_y_reverse(expand =c(0,0)) +
  # scale_fill_viridis_c() +
  labs(x = "Time in days", y = "Period", fill = "Frequency") +
  theme_bw()

test_plot %>% ggplotly()

# wt_output <- wt_321_daynight_depthmedian
