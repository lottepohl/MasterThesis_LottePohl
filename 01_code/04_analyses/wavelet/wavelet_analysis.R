# Script to execute wavelet analysis on the two longterm dst datasets from tag 1293308 and tag 1293321

library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(zoo)

rm(list = ls())

paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()

# inspect raw data ####

# 319 ####
p_depth_319 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293319", 
                                                             row_number() %% 10 == 0,
                                                             t < 500), 
                      aes(x = t, y = -depth_m)) + #, color = temp_c
  geom_line() +
  # scale_colour_distiller(palette ="RdYlBu") +
  theme_minimal()
# theme_dark()
p_depth_319 %>% plotly::ggplotly()

# preparing data input for wt ####

## 319

# compute_wavelet_transform <- function(tag_serial_number, cutoff_date)
sample_interval = 2 # minutes
dt <- round(sample_interval/60, digits = 10)
# dt <- 2/60 %>% round(digits = 3) # funktioniert nicht...
depthlog <- masterias_depth_temp %>% 
  ungroup() %>%
  filter(tag_serial_number == "1293319",
         t < 500)
t_max <- round(((depthlog %>% nrow()) * dt), digits = 0)

data_319_wt_all <- depthlog %>% #, row_number() %% 5 == 0
  dplyr::select(t, depth_m) %>%
  rename(t_hr = t) %>%
  mutate(dt = t_hr - lag(t_hr),
         t_new = seq(from = 0, to = t_max, length = depthlog %>% nrow()),
         dt_new = t_new - lag(t_new),
         dt_new = round(dt_new, digits = 10), # round dt
         t_day = t_new / 24,
         # make moving average
         depth_roll_1hr = zoo::rollmean(depth_m, k = 60/sample_interval, fill = NA),
         depth_roll_30min = zoo::rollmean(depth_m, k = 30/sample_interval, fill = NA),
         depth_scaled = depth_m %>% base::scale()
         ) %>%
  drop_na()

# executing wt ####

## test: 319 ####
# wavelet_transform_raw <- wt(data_319_wt %>% as.matrix())
# wavelet_transform <- biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_day, depth_m) %>% as.matrix(),
#                         dt = dt,
#                         do.sig = T,
#                         s0 = 5 * dt)# `wt()` requires matrix!

### Raw depth ####
#### t = day, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_day.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_day, depth_m) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = day, depth = raw, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_day, y = -data_319_wt_all$depth_m, type = "l")
dev.off() # Close device

#### t = hour, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_hour.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_new, depth_m) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = hour, depth = raw,, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_new, y = -data_319_wt_all$depth_m, type = "l")
dev.off() # Close device

### roll mean 1hr depth ####
#### t = day, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_day_rollmean1hr.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_day, depth_roll_1hr) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = day, depth = roll mean 1hr, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_day, y = -data_319_wt_all$depth_roll_1hr, type = "l")
dev.off() # Close device

#### t = hour, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_hour_rollmean1hr.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_new, depth_roll_1hr) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = hour, depth = roll mean 1hr, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_new, y = -data_319_wt_all$depth_roll_1hr, type = "l")
dev.off() # Close device

### depth centered & scaled ####
#### t = day, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_day_scaled.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_day, depth_scaled) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = day, depth = centred & scaled, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_day, y = -data_319_wt_all$depth_scaled, type = "l")
dev.off() # Close device

#### t = hour, s0 = 5 * dt, do.sig = T ####
jpeg(paste0(getwd(),"/02_results/dst_wavelet/wavelet_319_t_hour_scaled.jpeg"), quality = 75, width = 860, height = 860, units = "px")
par(mfcol = c(2,1))
plot(biwavelet::wt(d = data_319_wt_all %>% dplyr::select(t_new, depth_scaled) %>% as.matrix(),
                   dt = dt,
                   do.sig = T,
                   s0 = 5 * dt), 
     type = "power.corr.norm", main = "t = hour, depth = centred & scaled, s0 = 5 * dt, do.sig = T")
plot(x = data_319_wt_all$t_new, y = -data_319_wt_all$depth_scaled, type = "l")
dev.off() # Close device

# plot(x = data_319_wt$t_new, y = data_319_wt$depth_m, type = "l")
# ggplot(data = data_319_wt, aes(x = t_new, y = -depth_m))

# old ####
# 
# 
# ## 308 ####
# ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308", row_number() %% 10 == 0), 
#        aes(x = t, y = -depth_m, color = temp_c)) +
#   geom_point() +
#   scale_colour_distiller(palette ="RdYlBu") +
#   theme_dark()
# 
# ## 321 ####
# p_depth_321 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321", row_number() %% 10 == 0), 
#                       aes(x = t, y = -depth_m)) + #, color = temp_c
#   geom_line() +
#   # scale_colour_distiller(palette ="RdYlBu") +
#   theme_minimal()
# # theme_dark()
# p_depth_321
# 
# ## 304 ####
# p_depth_304 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293304", row_number() %% 10 == 0), 
#                       aes(x = t, y = -depth_m)) + #, color = temp_c
#   geom_line() +
#   # scale_colour_distiller(palette ="RdYlBu") +
#   theme_minimal()
# # theme_dark()
# p_depth_304
# 
# ## 321 ####
# wt1 <- wt(data_321_wt %>% as.matrix()) # `wt()` requires matrix!
# # par(mfrow = c(1,2))
# plot(wt1, type = "power.corr.norm", main = "Bias-corrected wavelet power")
# plot(data_321_wt$depth_m)
# 
# 
# ## 321 ####
# data_321_wt <- masterias_depth_temp %>% 
#   ungroup() %>%
#   filter(tag_serial_number == "1293321", row_number() %% 5 == 0) %>%
#   dplyr::select(t, depth_m) %>%
#   rename(t_hr = t)
# 
# 
# # testing out package ####
# 
# ## reproducing example of bias at lower periods ####
# par(mfrow = c(1,1))
t1 <- sin(seq(from = 0, to = 2 * 5 * pi, length = 1000))
t2 <- sin(seq(from = 0, to = 2 * 15 * pi, length = 1000))
t3 <- sin(seq(from = 0, to = 2 * 40 * pi, length = 1000))
timeseries <- t1 + t2 + t3

# # my example
# t1 <- sin(seq(from = 0, to = 2 * 10 * pi, length = 1000))
# t2 <- sin(seq(from = 0, to = 2 * 5 * pi, length = 1000))
# t2[0:500] <- 0
# timeseries <- t1 + t2
data <- cbind(1:1000, timeseries) %>% as_tibble()
# 
# ggplot(data = data) +
#   geom_line(aes(x = V1, y = timeseries)) +
#   theme_minimal()
# 
wt1 <- wt(data %>% as.matrix()) # `wt()` requires matrix!
par(mfrow = c(1,1))
plot(wt1, type = "power.corr.norm", main = "Bias-corrected wavelet power")
# plot(wt1, type = "power.norm", main = "Biased wavelet power")
# 
# 
# 
# 
# ## further examples ####
# 
# t1 <- cbind(1:100, rnorm(100))
# 
# ## Continuous wavelet transform
# wt.t1 <- wt(t1)
# 
# ## Plot power
# par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1) ## Make room to the right for the color bar
# plot(wt.t1, plot.cb = TRUE, plot.phase = FALSE)
# 
# # Sample time-series
noise1 <- cbind(1:100, rnorm(100))
noise2 <- cbind(1:100, rnorm(100))
# 
# # Cross-wavelet
xwt_noise12 <- xwt(noise1, noise2)
# 
# # Make room to the right for the color bar
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(xwt_noise12, plot.cb = TRUE, plot.phase = TRUE,
     main = "Cross wavelet power and phase difference (arrows)")
