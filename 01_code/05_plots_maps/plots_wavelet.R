# Script to plot the wavelet analysis results

library(scales)
library(biwavelet)
library(ggplot2)
library(dplyr)

# rm(list = ls())

dir_path_new <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
source(paste0(dir_path_new, "/01_code/06_functions/functions.R"))
source(paste0(dir_path_new, "/01_code/02_load_data/load_wavelet_results.R"))
source(paste0(dir_path_new, "/01_code/06_functions/compute_wavelettransform.R"))
paste0(dir_path_new, "/01_code/05_plots_maps/plots_dst_summary.R") %>% base::source()
paste0(dir_path_new, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()

# dates ####
dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)