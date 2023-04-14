# script to load plots for the thesis manuscript

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
 
# # run code to generate figures
# paste0(dir_path, "/01_code/05_plots_maps/thesis_manuscript_figures.R")

# plots ####

## 1. raw depth logs ####

p_dst_raw_295 <- load_data(filestring = "p_dst_raw_295", folder = plot_path)
p_dst_raw_308 <- load_data(filestring = "p_dst_raw_308", folder = plot_path)
p_dst_raw_321 <- load_data(filestring = "p_dst_raw_321", folder = plot_path)

## 2. autocorrelation ####

p_acf_308 <- load_data(filestring = "p_acf_308", folder = plot_path)
p_acf_321 <- load_data(filestring = "p_acf_321", folder = plot_path)

## 3. summary statistics ####

p_308_sum_stats <- load_data(filestring = "p_308_sum_stats", folder = plot_path)
p_321_sum_stats <- load_data(filestring = "p_321_sum_stats", folder = plot_path)

## 4. fft results ####

p_fft_295 <- load_data(filestring = "p_fft_295", folder = plot_path)
p_fft_308 <- load_data(filestring = "p_fft_308", folder = plot_path)
p_fft_321 <- load_data(filestring = "p_fft_321", folder = plot_path)

## 5. wavelet results ####

p_308_wavelet_depth_median_roll3 <- load_data(filestring = "p_308_wavelet_depth_median_roll3", folder = plot_path)
p_321_wavelet_depth_median_roll3 <- load_data(filestring = "p_321_wavelet_depth_median_roll3", folder = plot_path)

p_308_wavelet_depth_median_change_roll3 <- load_data(filestring = "p_308_wavelet_depth_median_change_roll3", folder = plot_path)
p_321_wavelet_depth_median_change_roll3 <- load_data(filestring = "p_321_wavelet_depth_median_change_roll3", folder = plot_path)

p_308_wavelet_depth_min <- load_data(filestring = "p_308_wavelet_depth_min", folder = plot_path)
p_321_wavelet_depth_min <- load_data(filestring = "p_321_wavelet_depth_min", folder = plot_path)

p_308_wavelet_maxdepth_change_roll3 <- load_data(filestring = "p_308_wavelet_maxdepth_change_roll3", folder = plot_path)
p_321_wavelet_maxdepth_change_roll3 <- load_data(filestring = "p_321_wavelet_maxdepth_change_roll3", folder = plot_path)

