# Script to conduct and plot wavelet analyses

library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(tidyverse)
# library(zoo)

# rm(list = ls())

# paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
source(paste0(getwd(), "/01_code/02_load_data/load_dst_summarystatistics.R"))

par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_321_summary_wt_all %>% dplyr::select(t_days, depth_median)%>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = day, depth = raw, s0 = 5 * dt, do.sig = T")
plot(x = data_321_summary_wt_all$date, y = -data_321_summary_wt_all$depth_median, type = "l")

compute_wavelet <- function(parameter, dt, factor_smallest_scale){
  # make time vector according to nrow() of parameter and given dt
  timevector <- seq(from = 0, to = (nrow(parameter) * dt) - dt, by = dt)
  wt_input <- cbind(timevector, parameter) %>% as.matrix()
  # make wt result
  wt_output <- biwavelet::wt(d = data_321_summary_wt_all %>% dplyr::select(t_days, depth_median)%>% as.matrix(),
                             dt = dt,
                             do.sig = T,
                             s0 = factor_smallest_scale * dt) # this specifies the scale at which periods are looked at
  
  return(wt_output)
}
  
wt_321_daynight_depthmedian <- compute_wavelet(parameter = masterias_depth_daynight %>% filter(tag_serial_number == "1293321", dusk == 0 & dawn == 0) %>% 
                                                 dplyr::select(depth_median),
                                               dt = 0.5,
                                               factor_smallest_scale = 1 # 0.5 day period minimum
                                               )


