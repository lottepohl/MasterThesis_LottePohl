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
plot_depth_range <- load_data(filestring = "plot_depth_range", folder = plot_path)
plot_depth_range_heatmap <- load_data(filestring = "plot_depth_range_heatmap", folder = plot_path)


## 1. raw depth logs ####

p_dst_raw_295 <- load_data(filestring = "p_dst_raw_295", folder = plot_path)
p_dst_raw_308 <- load_data(filestring = "p_dst_raw_308", folder = plot_path)
p_dst_raw_321 <- load_data(filestring = "p_dst_raw_321", folder = plot_path)
p_dst_raw_322 <- load_data(filestring = "p_dst_raw_322", folder = plot_path)
p_dst_raw_319 <- load_data(filestring = "p_dst_raw_319", folder = plot_path)
p_dst_raw_304 <- load_data(filestring = "p_dst_raw_304", folder = plot_path)
p_dst_raw_310 <- load_data(filestring = "p_dst_raw_310", folder = plot_path)
p_dst_raw_312 <- load_data(filestring = "p_dst_raw_312", folder = plot_path)

## 2. autocorrelation ####

p_acf_308 <- load_data(filestring = "p_acf_308", folder = plot_path)
p_acf_321 <- load_data(filestring = "p_acf_321", folder = plot_path)

p_308_rulsif_all <- load_data(filestring = "p_308_rulsif_all", folder = plot_path)
p_321_rulsif_all <- load_data(filestring = "p_321_rulsif_all", folder = plot_path)

## 3. summary statistics ####

p_308_sum_stats <- load_data(filestring = "p_308_sum_stats", folder = plot_path)
p_321_sum_stats <- load_data(filestring = "p_321_sum_stats", folder = plot_path)

## 4. fft results ####

p_fft_295 <- load_data(filestring = "p_fft_295", folder = plot_path)
p_fft_308 <- load_data(filestring = "p_fft_308", folder = plot_path)
p_fft_321 <- load_data(filestring = "p_fft_321", folder = plot_path)

## 5. wavelet results ####

p_308_wavelet_depth_median_sgolay <- load_data(filestring = "p_308_wavelet_depth_median_sgolay", folder = plot_path)
p_308_wavelet_depth_min_sgolay <- load_data(filestring = "p_308_wavelet_depth_min_sgolay", folder = plot_path)
p_308_wavelet_depth_max_sgolay <- load_data(filestring = "p_308_wavelet_depth_max_sgolay", folder = plot_path)

p_321_wavelet_depth_median_sgolay <- load_data(filestring = "p_321_wavelet_depth_median_sgolay", folder = plot_path)
p_321_wavelet_depth_min_sgolay <- load_data(filestring = "p_321_wavelet_depth_min_sgolay", folder = plot_path)
p_321_wavelet_depth_max_sgolay <- load_data(filestring = "p_321_wavelet_depth_max_sgolay", folder = plot_path)

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

## 6. change point detection results ####

p_308_scores_rulsif_2_5percent <- load_data(filestring = "p_308_scores_rulsif_2_5percent", folder = plot_path)
p_308_ribbon_rulsif_2_5percent <- load_data(filestring = "p_308_ribbon_rulsif_2_5percent", folder = plot_path)

p_308_scores_rulsif_5percent <- load_data(filestring = "p_308_scores_rulsif_5percent", folder = plot_path)
p_308_ribbon_rulsif_5percent <- load_data(filestring = "p_308_ribbon_rulsif_5percent", folder = plot_path)

p_308_scores_rulsif_10percent <- load_data(filestring = "p_308_scores_rulsif_10percent", folder = plot_path)
p_308_ribbon_rulsif_10percent <- load_data(filestring = "p_308_ribbon_rulsif_10percent", folder = plot_path)

p_321_scores_rulsif_2_5percent <- load_data(filestring = "p_321_scores_rulsif_2_5percent", folder = plot_path)
p_321_ribbon_rulsif_2_5percent <- load_data(filestring = "p_321_ribbon_rulsif_2_5percent", folder = plot_path)

p_321_scores_rulsif_5percent <- load_data(filestring = "p_321_scores_rulsif_5percent", folder = plot_path)
p_321_ribbon_rulsif_5percent <- load_data(filestring = "p_321_ribbon_rulsif_5percent", folder = plot_path)

p_321_scores_rulsif_10percent <- load_data(filestring = "p_321_scores_rulsif_10percent", folder = plot_path)
p_321_ribbon_rulsif_10percent <- load_data(filestring = "p_321_ribbon_rulsif_10percent", folder = plot_path)
