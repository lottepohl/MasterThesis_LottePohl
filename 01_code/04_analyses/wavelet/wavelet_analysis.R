# Script to execute wavelet analysis on the two longterm dst datasets from tag 1293308 and tag 1293321

library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)

rm(list = ls())

paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()

# inspect raw data ####

## 308 ####
ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308", row_number() %% 10 == 0), 
       aes(x = t, y = -depth_m, color = temp_c)) +
  geom_point() +
  scale_colour_distiller(palette ="RdYlBu") +
  theme_dark()

## 321 ####
p_depth_321 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321", row_number() %% 10 == 0), 
       aes(x = t, y = -depth_m)) + #, color = temp_c
  geom_line() +
  # scale_colour_distiller(palette ="RdYlBu") +
  theme_minimal()
  # theme_dark()
p_depth_321

## 304 ####
p_depth_304 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293304", row_number() %% 10 == 0), 
                      aes(x = t, y = -depth_m)) + #, color = temp_c
  geom_line() +
  # scale_colour_distiller(palette ="RdYlBu") +
  theme_minimal()
# theme_dark()
p_depth_304

# 319 ####
p_depth_319 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293319", 
                                                             row_number() %% 10 == 0,
                                                             t < 500), 
                      aes(x = t, y = -depth_m)) + #, color = temp_c
  geom_line() +
  # scale_colour_distiller(palette ="RdYlBu") +
  theme_minimal()
# theme_dark()
p_depth_319

# preparing data input for wt ####

## 321 ####
data_321_wt <- masterias_depth_temp %>% 
  ungroup() %>%
  filter(tag_serial_number == "1293321", row_number() %% 5 == 0) %>%
  dplyr::select(t, depth_m) %>%
  rename(t_hr = t)

## 319
data_319_wt <- masterias_depth_temp %>% 
  ungroup() %>%
  filter(tag_serial_number == "1293319", row_number() %% 5 == 0) %>%
  dplyr::select(t, depth_m) %>%
  rename(t_hr = t)

# executing wt ####

## 321 ####
wt1 <- wt(data_321_wt %>% as.matrix()) # `wt()` requires matrix!
# par(mfrow = c(1,2))
plot(wt1, type = "power.corr.norm", main = "Bias-corrected wavelet power")
plot(data_321_wt$depth_m)

## 319 ####
wt1 <- wt(data_319_wt %>% as.matrix()) # `wt()` requires matrix!
# par(mfrow = c(1,2))
plot(wt1, type = "power.corr.norm", main = "Bias-corrected wavelet power")
plot(data_319_wt$depth_m)

# testing out package ####

## reproducing example of bias at lower periods ####
par(mfrow = c(1,1))
t1 <- sin(seq(from = 0, to = 2 * 5 * pi, length = 1000))
t2 <- sin(seq(from = 0, to = 2 * 15 * pi, length = 1000))
t3 <- sin(seq(from = 0, to = 2 * 40 * pi, length = 1000))
timeseries <- t1 + t2 + t3

# my example
t1 <- sin(seq(from = 0, to = 2 * 10 * pi, length = 1000))
t2 <- sin(seq(from = 0, to = 2 * 5 * pi, length = 1000))
t2[0:500] <- 0
timeseries <- t1 + t2
data <- cbind(1:1000, timeseries) %>% as_tibble()

ggplot(data = data) +
  geom_line(aes(x = V1, y = timeseries)) +
  theme_minimal()

wt1 <- wt(data %>% as.matrix()) # `wt()` requires matrix!
# par(mfrow = c(1,2))
plot(wt1, type = "power.corr.norm", main = "Bias-corrected wavelet power")
plot(wt1, type = "power.norm", main = "Biased wavelet power")




## further examples ####
# 
# # Sample time-series
# noise1 <- cbind(1:100, rnorm(100))
# noise2 <- cbind(1:100, rnorm(100))
# 
# # Cross-wavelet
# xwt_noise12 <- xwt(noise1, noise2)
# 
# # Make room to the right for the color bar
# par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
# plot(xwt_noise12, plot.cb = TRUE, plot.phase = TRUE,
#      main = "Cross wavelet power and phase difference (arrows)")