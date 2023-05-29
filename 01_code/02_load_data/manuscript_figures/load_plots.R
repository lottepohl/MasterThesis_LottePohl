# script to load plots for the thesis manuscript

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
 
# # run code to generate figures
# paste0(dir_path, "/01_code/05_plots_maps/thesis_manuscript_figures.R")

# plots ####

## 0. basic acoustic/dst plots ####

p_length_sex <- load_data(filestring = "p_length_sex", folder = plot_path)
p_abacus <- load_data(filestring = "p_abacus", folder = plot_path)
p_detections_heatmap <- load_data(filestring = "p_detections_heatmap", folder = plot_path)
p_detections_heatmap_OG10 <- load_data(filestring = "p_detections_heatmap_OG10", folder = plot_path)
p_OG10_boxplot <- load_data(filestring = "p_OG10_boxplot", folder = plot_path)
p_308_DST_acoustic <- load_data(filestring = "p_308_DST_acoustic", folder = plot_path)


### 0.6. vertical analysis ####
plot_depth_range <- load_data(filestring = "plot_depth_range", folder = plot_path)
plot_depth_range_heatmap <- load_data(filestring = "plot_depth_range_heatmap", folder = plot_path)


## 1. raw depth logs ####

### lines ####

p_308_depth <- load_data(filestring = "p_308_depth", folder = plot_path)
p_321_depth <- load_data(filestring = "p_321_depth", folder = plot_path)

p_308_temp <- load_data(filestring = "p_308_temp", folder = plot_path)
p_321_temp <- load_data(filestring = "p_321_temp", folder = plot_path)

## subsets ####
p_321_depth_winter <- load_data(filestring = "p_321_depth_winter", folder = plot_path)
p_321_depth_summer <- load_data(filestring = "p_321_depth_summer", folder = plot_path)
p_308_depth_winter <- load_data(filestring = "p_308_depth_winter", folder = plot_path)
p_308_depth_summer <- load_data(filestring = "p_308_depth_summer", folder = plot_path)

### points ####
# ToDo: invlude temperature logs
p_312_temp <- load_data(filestring = "p_312_temp", folder = plot_path)
p_312_depth <- load_data(filestring = "p_312_depth", folder = plot_path)
p_310_temp <- load_data(filestring = "p_310_temp", folder = plot_path)
p_310_depth <- load_data(filestring = "p_310_depth", folder = plot_path)
p_304_temp <- load_data(filestring = "p_304_temp", folder = plot_path)
p_304_depth <- load_data(filestring = "p_304_depth", folder = plot_path)
p_295_temp <- load_data(filestring = "p_295_temp", folder = plot_path)
p_295_depth <- load_data(filestring = "p_295_depth", folder = plot_path)
p_319_depth <- load_data(filestring = "p_319_depth", folder = plot_path)
p_319_temp <- load_data(filestring = "p_319_temp", folder = plot_path)
p_322_temp <- load_data(filestring = "p_322_temp", folder = plot_path)
p_322_depth <- load_data(filestring = "p_322_depth", folder = plot_path)

## 2. autocorrelation ####

p_acf_308 <- load_data(filestring = "p_acf_308", folder = plot_path)
p_acf_321 <- load_data(filestring = "p_acf_321", folder = plot_path)

p_308_rulsif_all <- load_data(filestring = "p_308_rulsif_all", folder = plot_path)
p_321_rulsif_all <- load_data(filestring = "p_321_rulsif_all", folder = plot_path)

## 3. summary statistics ####

p_308_sum_stats <- load_data(filestring = "p_308_sum_stats", folder = plot_path)
p_321_sum_stats <- load_data(filestring = "p_321_sum_stats", folder = plot_path)

# p_308_sum_daynight <- load_data(filestring = "p_308_sum_daynight", folder = plot_path)
# p_321_sum_daynight <- load_data(filestring = "p_321_sum_daynight", folder = plot_path)

### 3.1. daynight sum stats ####

p_308_sum_day <- load_data(filestring = "p_308_sum_day", folder = plot_path)
p_308_sum_night <- load_data(filestring = "p_308_sum_night", folder = plot_path)

p_321_sum_day <- load_data(filestring = "p_321_sum_day", folder = plot_path)
p_321_sum_night <- load_data(filestring = "p_321_sum_night", folder = plot_path)

## 4. fft results ####

p_fft_295 <- load_data(filestring = "p_fft_295", folder = plot_path)
p_fft_308 <- load_data(filestring = "p_fft_308", folder = plot_path)
p_fft_321 <- load_data(filestring = "p_fft_321", folder = plot_path)

## 5. wavelet results ####

p_308_wavelet_depth_median_sgolay <- load_data(filestring = "p_308_wavelet_depth_median_sgolay", folder = plot_path)
# p_308_wavelet_depth_min_sgolay <- load_data(filestring = "p_308_wavelet_depth_min_sgolay", folder = plot_path)
# p_308_wavelet_depth_max_sgolay <- load_data(filestring = "p_308_wavelet_depth_max_sgolay", folder = plot_path)
p_308_wavelet_depth_range_sgolay <- load_data(filestring = "p_308_wavelet_depth_range_sgolay", folder = plot_path)

p_321_wavelet_depth_median_sgolay <- load_data(filestring = "p_321_wavelet_depth_median_sgolay", folder = plot_path)
# p_321_wavelet_depth_min_sgolay <- load_data(filestring = "p_321_wavelet_depth_min_sgolay", folder = plot_path)
# p_321_wavelet_depth_max_sgolay <- load_data(filestring = "p_321_wavelet_depth_max_sgolay", folder = plot_path)
p_321_wavelet_depth_range_sgolay <- load_data(filestring = "p_321_wavelet_depth_range_sgolay", folder = plot_path)

# p_308_wavelet_depth_median_roll3 <- load_data(filestring = "p_308_wavelet_depth_median_roll3", folder = plot_path)
# p_321_wavelet_depth_median_roll3 <- load_data(filestring = "p_321_wavelet_depth_median_roll3", folder = plot_path)
# 
# p_308_wavelet_depth_median_change_roll3 <- load_data(filestring = "p_308_wavelet_depth_median_change_roll3", folder = plot_path)
# p_321_wavelet_depth_median_change_roll3 <- load_data(filestring = "p_321_wavelet_depth_median_change_roll3", folder = plot_path)

# p_308_wavelet_depth_min <- load_data(filestring = "p_308_wavelet_depth_min", folder = plot_path)
# p_321_wavelet_depth_min <- load_data(filestring = "p_321_wavelet_depth_min", folder = plot_path)

# p_308_wavelet_depth_max <- load_data(filestring = "p_308_wavelet_depth_max", folder = plot_path)
# p_321_wavelet_depth_max <- load_data(filestring = "p_321_wavelet_depth_max", folder = plot_path)

# p_308_wavelet_maxdepth_change_roll3 <- load_data(filestring = "p_308_wavelet_maxdepth_change_roll3", folder = plot_path)
# p_321_wavelet_maxdepth_change_roll3 <- load_data(filestring = "p_321_wavelet_maxdepth_change_roll3", folder = plot_path)

### 5.1. hourly wavelets ####

p_308_wavelet_depth_hr <- load_data(filestring = "p_308_wavelet_depth_hr", folder = plot_path)
p_321_wavelet_depth_hr <- load_data(filestring = "p_321_wavelet_depth_hr", folder = plot_path)

## 6. change point detection results ####

# p_308_scores_rulsif_2_5percent <- load_data(filestring = "p_308_scores_rulsif_2_5percent", folder = plot_path)
# p_308_ribbon_rulsif_2_5percent <- load_data(filestring = "p_308_ribbon_rulsif_2_5percent", folder = plot_path)
# 
# p_308_scores_rulsif_5percent <- load_data(filestring = "p_308_scores_rulsif_5percent", folder = plot_path)
# p_308_ribbon_rulsif_5percent <- load_data(filestring = "p_308_ribbon_rulsif_5percent", folder = plot_path)
# 
# p_308_scores_rulsif_10percent <- load_data(filestring = "p_308_scores_rulsif_10percent", folder = plot_path)
# p_308_ribbon_rulsif_10percent <- load_data(filestring = "p_308_ribbon_rulsif_10percent", folder = plot_path)
# 
# p_321_scores_rulsif_2_5percent <- load_data(filestring = "p_321_scores_rulsif_2_5percent", folder = plot_path)
# p_321_ribbon_rulsif_2_5percent <- load_data(filestring = "p_321_ribbon_rulsif_2_5percent", folder = plot_path)
# 
# p_321_scores_rulsif_5percent <- load_data(filestring = "p_321_scores_rulsif_5percent", folder = plot_path)
# p_321_ribbon_rulsif_5percent <- load_data(filestring = "p_321_ribbon_rulsif_5percent", folder = plot_path)
# 
# p_321_scores_rulsif_10percent <- load_data(filestring = "p_321_scores_rulsif_10percent", folder = plot_path)
# p_321_ribbon_rulsif_10percent <- load_data(filestring = "p_321_ribbon_rulsif_10percent", folder = plot_path)

## 7. lm moon illumination plots ####

p_308_lm_median_moonfraq_smooth <- load_data(filestring = "p_308_lm_median_moonfraq_smooth", folder = plot_path)
p_321_lm_median_moonfraq_smooth <- load_data(filestring = "p_321_lm_median_moonfraq_smooth", folder = plot_path)

# p_308_lm_min_moonfraq_residuals <- load_data(filestring = "p_308_lm_min_moonfraq_residuals", folder = plot_path)
# p_308_lm_min_moonfraq_qq <- load_data(filestring = "p_308_lm_min_moonfraq_qq", folder = plot_path)
# p_308_lm_min_moonfraq_density <- load_data(filestring = "p_308_lm_min_moonfraq_density", folder = plot_path)
# 
# p_321_lm_min_moonfraq_smooth <- load_data(filestring = "p_321_lm_min_moonfraq_smooth", folder = plot_path)
# p_321_lm_min_moonfraq_residuals <- load_data(filestring = "p_321_lm_min_moonfraq_residuals", folder = plot_path)
# p_321_lm_min_moonfraq_qq <- load_data(filestring = "p_321_lm_min_moonfraq_qq", folder = plot_path)
# p_321_lm_min_moonfraq_density <- load_data(filestring = "p_321_lm_min_moonfraq_density", folder = plot_path)

