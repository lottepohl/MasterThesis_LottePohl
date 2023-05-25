# Script to make linear model to assess correlation between minimum depth and moon fraction

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
path_models <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()

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

# tag 308 ####
# dates: from when shark starts to be vertically active again until summer migration potentially starts
start_date <- "2018-12-12" %>% as.POSIXct()
end_date <- "2019-03-11" %>% as.POSIXct()

## per date ####
# prepare data

data_lm_308 <- long_dst_date %>% 
  dplyr::filter(tag_serial_number == "1293308",
         date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293308",
                                            date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_308_sum <- data_lm_308 %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

## min depth

lm_308_depthmin_moonfraq <- stats::lm(data = data_lm_308, formula = depth_min_sgolay ~ moonfraq)
lm_308_depthmin_moonfraq %>% summary()

save_data(data = lm_308_depthmin_moonfraq, folder = path_models)

## median depth

lm_308_depthmedian_moonfraq <- stats::lm(data = data_lm_308, formula = depth_median_sgolay ~ moonfraq)
lm_308_depthmedian_moonfraq %>% summary()

save_data(data = lm_308_depthmedian_moonfraq, folder = path_models)

## day vs night ####

### day
data_lm_308_day <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293308",
         date %>% between(start_date, end_date),
         day == 1) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293308",
                                            date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

save_data(data = data_lm_308_day, folder = path_models)

data_lm_308_day_sum <- data_lm_308_day %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

# ## min depth
# 
# lm_308_day_depthmin_moonfraq <- stats::lm(data = data_lm_308_day, formula = depth_min_sgolay ~ moonfraq)
# lm_308_day_depthmin_moonfraq %>% summary()
# 
# save_data(data = lm_308_day_depthmin_moonfraq, folder = path_models)

## median depth

lm_308_day_depthmedian_moonfraq <- stats::lm(data = data_lm_308_day, formula = depth_median_sgolay ~ moonfraq)
lm_308_day_depthmedian_moonfraq <- lm_308_day_depthmedian_moonfraq %>% summary()

save_data(data = lm_308_day_depthmedian_moonfraq, folder = path_models)

### night
data_lm_308_night <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293308",
                date %>% between(start_date, end_date),
                day == 0) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293308",
                                                   date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)
save_data(data = data_lm_308_night, folder = path_models)

data_lm_308_night_sum <- data_lm_308_night %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

# ## min depth
# 
# lm_308_night_depthmin_moonfraq <- stats::lm(data = data_lm_308_night, formula = depth_min_sgolay ~ moonfraq)
# lm_308_night_depthmin_moonfraq <- lm_308_night_depthmin_moonfraq %>% summary()
# 
# save_data(data = lm_308_night_depthmin_moonfraq, folder = path_models)

## median depth

lm_308_night_depthmedian_moonfraq <- stats::lm(data = data_lm_308_night, formula = depth_median_sgolay ~ moonfraq)
lm_308_night_depthmedian_moonfraq <- lm_308_night_depthmedian_moonfraq %>% summary()

save_data(data = lm_308_night_depthmedian_moonfraq, folder = path_models)

### daynight together

data_lm_308_daynight <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293308",
                date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293308",
                                                   date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_308_daynight_sum <- data_lm_308_daynight %>% group_by(moonfraq, day) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay),
            depth_median_median = median(depth_median_sgolay))

# ggplot(data = data_lm_308_daynight_sum) + geom_point(aes(x = moonfraq, y = depth_median_median, colour = day))

lm_308_daynight_depthmedian_moonfraq <- stats::lm(data = data_lm_308_daynight, formula = depth_median_sgolay ~ moonfraq + day)
lm_308_daynight_depthmedian_moonfraq %>% summary()

save_data(data = lm_308_daynight_depthmedian_moonfraq, folder = path_models) # for depth median, factor day does not have an influence,
# i.e. the median depth does not depend on the moon and if it's day or night

lm_308_daynight_depthmin_moonfraq <- stats::lm(data = data_lm_308_daynight, formula = depth_min_sgolay ~ moonfraq + day)
lm_308_daynight_depthmin_moonfraq %>% summary()
# but the min depth is correlated with the moonfraq + day

save_data(data = lm_308_daynight_depthmin_moonfraq, folder = path_models)

# tag 321 ####
start_date <- "2018-12-09" %>% as.POSIXct()
end_date <- "2019-04-30" %>% as.POSIXct()

## per date ####

# prepare data

data_lm_321 <- long_dst_date %>% 
  dplyr::filter(tag_serial_number == "1293321",
         date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293321",
                                            date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(), 
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_321_sum <- data_lm_321 %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

# ## min depth
# 
# lm_321_depthmin_moonfraq <- stats::lm(data = data_lm_321, formula = depth_min_sgolay ~ moonfraq)
# lm_321_depthmin_moonfraq %>% summary()
# 
# save_data(data = lm_321_depthmin_moonfraq, folder = path_models)

# ## median depth
# 
# lm_321_depthmedian_moonfraq <- stats::lm(data = data_lm_321, formula = depth_median_sgolay ~ moonfraq)
# lm_321_depthmedian_moonfraq %>% summary()
# 
# save_data(data = lm_321_depthmedian_moonfraq, folder = path_models)

## day vs night ####

### day
data_lm_321_day <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293321",
                date %>% between(start_date, end_date),
                day == 1) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293321",
                                                   date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

save_data(data = data_lm_321_day, folder = path_models)

data_lm_321_day_sum <- data_lm_321_day %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

# ## min depth
# 
# lm_321_day_depthmin_moonfraq <- stats::lm(data = data_lm_321_day, formula = depth_min_sgolay ~ moonfraq)
# lm_321_day_depthmin_moonfraq <- lm_321_day_depthmin_moonfraq %>% summary()
# 
# save_data(data = lm_321_day_depthmin_moonfraq, folder = path_models)

## median depth

lm_321_day_depthmedian_moonfraq <- stats::lm(data = data_lm_321_day, formula = depth_median_sgolay ~ moonfraq)
lm_321_day_depthmedian_moonfraq <- lm_321_day_depthmedian_moonfraq %>% summary()

save_data(data = lm_321_day_depthmedian_moonfraq, folder = path_models)

### night
data_lm_321_night <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293321",
                date %>% between(start_date, end_date),
                day == 0) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293321",
                                                   date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

save_data(data = data_lm_321_night, folder = path_models)


data_lm_321_night_sum <- data_lm_321_night %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

# ## min depth
# 
# lm_321_night_depthmin_moonfraq <- stats::lm(data = data_lm_321_night, formula = depth_min_sgolay ~ moonfraq)
# lm_321_night_depthmin_moonfraq %>% summary()
# 
# save_data(data = lm_321_night_depthmin_moonfraq, folder = path_models)

## median depth

lm_321_night_depthmedian_moonfraq <- stats::lm(data = data_lm_321_night, formula = depth_median_sgolay ~ moonfraq)
lm_321_night_depthmedian_moonfraq <- lm_321_night_depthmedian_moonfraq %>% summary()

save_data(data = lm_321_night_depthmedian_moonfraq, folder = path_models)

### daynight together

data_lm_321_daynight <- long_dst_daynight %>% 
  dplyr::filter(tag_serial_number == "1293321",
                date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     dplyr::filter(tag_serial_number == "1293321",
                                                   date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_321_daynight_sum <- data_lm_321_daynight %>% group_by(moonfraq, day) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay),
            depth_median_median = median(depth_median_sgolay))

ggplot(data = data_lm_321_daynight_sum) + geom_point(aes(x = moonfraq, y = depth_median_median, colour = day))

lm_321_daynight_depthmedian_moonfraq <- stats::lm(data = data_lm_321_daynight, formula = depth_median_sgolay ~ moonfraq + day)
lm_321_daynight_depthmedian_moonfraq %>% summary()
# here, the moonfraq does not seem to have that big an influence but rather the factor if it's night or day. This speaks for a 
# lower influence of moon phase of shark 321, which could be because tidal currents don't differ that much in the central north sea

save_data(data = lm_321_daynight_depthmedian_moonfraq, folder = path_models) # for depth median, factor day does not have an influence,
# i.e. the median depth does not depend on the moon and if it's day or night

lm_321_daynight_depthmin_moonfraq <- stats::lm(data = data_lm_321_daynight, formula = depth_min_sgolay ~ moonfraq + day)
lm_321_daynight_depthmin_moonfraq %>% summary()
# but the min depth is correlated with the moonfraq + day

#in general: I should not give so much value to the depthmin, because this is a less robust statistic compared to the depth median, 
# i.e. the depthmin comes from a single value and the depth median reflects better the whole distribution of depth measurements

# t test difference depth day vs night ####

# visual inspection -> depth during night is shallower than during day
# min depth
ggplot() + geom_point(data = data_lm_308_day, aes(y = -depth_min, x = "day")) + geom_point(data = data_lm_308_night, aes(y = -depth_min, x = "night"))
ggplot() + geom_point(data = data_lm_321_day, aes(y = -depth_min, x = "day")) + geom_point(data = data_lm_321_night, aes(y = -depth_min, x = "night"))

# median depth
ggplot() + geom_point(data = data_lm_308_day, aes(y = -depth_median, x = "day")) + geom_point(data = data_lm_308_night, aes(y = -depth_median, x = "night"))
ggplot() + geom_point(data = data_lm_321_day, aes(y = -depth_median, x = "day")) + geom_point(data = data_lm_321_night, aes(y = -depth_median, x = "night"))

## test if depths of day and night are normally distributed --> some are not so let's treat all samples as if they were not normally distributed

### tag 308

#### day
stats::shapiro.test(data_lm_308_day$depth_min) # p val > 0.05: normally distributed
stats::shapiro.test(data_lm_308_day$depth_median) # no

#### night
stats::shapiro.test(data_lm_308_night$depth_min) # no
stats::shapiro.test(data_lm_308_night$depth_median) # no

### tag 321

#### day
stats::shapiro.test(data_lm_321_day$depth_min) # p val > 0.05: normally distributed
stats::shapiro.test(data_lm_321_day$depth_median) # no

#### night
stats::shapiro.test(data_lm_321_night$depth_min) # no
stats::shapiro.test(data_lm_321_night$depth_median) # no

## wilcox test for not-normally distributed samples

### tag 308
wilcox_308_depth_min_daynight <- stats::wilcox.test(data_lm_308_day$depth_min, data_lm_308_night$depth_min, alternative = "greater") # p < 0.05: medians are NOT equal (thus, sign. difference)
wilcox_308_depth_median_daynight <- stats::wilcox.test(data_lm_308_day$depth_median, data_lm_308_night$depth_median, alternative = "greater") # p < 0.05: medians are NOT equal (thus, sign. difference)

save_data(data = wilcox_308_depth_min_daynight, folder = path_models)
save_data(data = wilcox_308_depth_median_daynight, folder = path_models)

### tag 321
wilcox_321_depth_min_daynight <- stats::wilcox.test(data_lm_321_day$depth_min, data_lm_321_night$depth_min, alternative = "greater") # p < 0.05: medians are NOT equal (thus, sign. difference)
wilcox_321_depth_median_daynight <- stats::wilcox.test(data_lm_321_day$depth_median, data_lm_321_night$depth_median, alternative = "greater") # p < 0.05: medians are NOT equal (thus, sign. difference)

save_data(data = wilcox_321_depth_min_daynight, folder = path_models)
save_data(data = wilcox_321_depth_median_daynight, folder = path_models)

# 
# # plot lm 308
# 
# ## min daily depth
# ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_min_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily minimum depth in m (Savitzky-Golay filter)") #, title = 'daily min depth over moon fraq'
# 
# # plot residuals
# ggplot(lm_308_depthmin_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(lm_308_depthmin_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_308, aes(sample=-log(depth_min_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# ggplot(lm_308_depthmin_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# 
# ## median daily depth
# ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)") #, title = 'daily median depth over moon fraq'
# 
# # plot residuals
# ggplot(lm_308_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(lm_308_depthmedian_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_308, aes(sample=-log(depth_median_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# ggplot(lm_308_depthmedian_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
# 
# 
# # plot lm 321
# 
# ## min daily depth
# ggplot(data = data_lm_321, aes(x = moonfraq, y = -log(depth_min_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily minimum depth in m (Savitzky-Golay filter)") #, title = 'daily min depth over moon fraq'
# 
# # plot residuals
# ggplot(lm_321_depthmin_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(lm_321_depthmin_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_321, aes(sample=-log(depth_min_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# ggplot(lm_321_depthmin_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
#   
# 
# ## median daily depth
# ggplot(data = data_lm_321, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)") #, title = 'daily median depth over moon fraq'
# 
# # plot residuals
# ggplot(lm_321_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(lm_321_depthmedian_moonfraq, aes(sample = .resid)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_321, aes(sample=-log(depth_median_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# ggplot(lm_321_depthmedian_moonfraq, aes(x=.resid))+
#   geom_density(linewidth = 1) 
