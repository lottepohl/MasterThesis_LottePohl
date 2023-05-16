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

# prepare data

data_lm_308 <- long_dst_date %>% 
  filter(tag_serial_number == "1293308",
         date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     filter(tag_serial_number == "1293308",
                                            date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(),
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_308_sum <- data_lm_308 %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

## min depth ####

lm_308_depthmin_moonfraq <- stats::lm(data = data_lm_308, formula = depth_min_sgolay ~ moonfraq)
lm_308_depthmin_moonfraq %>% summary()

save_data(data = lm_308_depthmin_moonfraq, folder = path_models)

## median depth ####

lm_308_depthmedian_moonfraq <- stats::lm(data = data_lm_308, formula = depth_median_sgolay ~ moonfraq)
lm_308_depthmedian_moonfraq %>% summary()

save_data(data = lm_308_depthmedian_moonfraq, folder = path_models)


# tag 321 ####
start_date <- "2018-12-09" %>% as.POSIXct()
end_date <- "2019-04-30" %>% as.POSIXct()

# prepare data

data_lm_321 <- long_dst_date %>% 
  filter(tag_serial_number == "1293321",
         date %>% between(start_date, end_date)) %>%
  # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
  mutate(moonfraq = oce::moonAngle(t = long_dst_date %>% 
                                     filter(tag_serial_number == "1293321",
                                            date %>% between(start_date, end_date)) %>% 
                                     dplyr::select(date) %>% pull(), 
                                   longitude = 2.45, latitude = 51)$illuminatedFraction)

data_lm_321_sum <- data_lm_321 %>% group_by(moonfraq) %>% #make summary per moonfraq
  summarise(depth_min_median = median(depth_min_sgolay),
            depth_min_mean = mean(depth_min_sgolay))

## min depth ####

lm_321_depthmin_moonfraq <- stats::lm(data = data_lm_321, formula = depth_min_sgolay ~ moonfraq)
lm_321_depthmin_moonfraq %>% summary()

save_data(data = lm_321_depthmin_moonfraq, folder = path_models)

## median depth ####

lm_321_depthmedian_moonfraq <- stats::lm(data = data_lm_321, formula = depth_median_sgolay ~ moonfraq)
lm_321_depthmedian_moonfraq %>% summary()

save_data(data = lm_321_depthmedian_moonfraq, folder = path_models)


#-----------------------------------
# 
# # plot lm 308
# 
# ## min daily depth ####
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
# ## median daily depth ####
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
# # plot lm 321 ####
# 
# ## min daily depth ####
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
# ## median daily depth ####
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
