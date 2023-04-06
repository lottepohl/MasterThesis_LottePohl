# script to load the dst summary statistics calculated in `dst_summary_calc.R`
# Author: Lotte Pohl

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()
# source(paste0(dir_path, "/02_scripts/04_analyses/dst_summarystatistics/dst_summary_calc.R"))

# general
masterias_depth_temp_summary <- load_data(filestring = "masterias_depth_temp_summary", folder = paste0(dir_path, "/02_results/dst_summary/"))


# depth and vertical speed
masterias_depth_daynight <- load_data(filestring = "masterias_depth_daynight", folder = paste0(dir_path, "/02_results/dst_summary/"))
masterias_depth_date <- load_data(filestring = "masterias_depth_date", folder = paste0(dir_path, "/02_results/dst_summary/"))
masterias_depth_week <- load_data(filestring = "masterias_depth_week", folder = paste0(dir_path, "/02_results/dst_summary/"))
masterias_depth_month <- load_data(filestring = "masterias_depth_month", folder = paste0(dir_path, "/02_results/dst_summary/"))
long_dst_date <- load_data(filestring = "long_dst_date", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()
long_dst_daynight <- load_data(filestring = "long_dst_daynight", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()

# vertical movement behaviour
masterias_DVM_sum_day <- load_data(filestring = "masterias_DVM_sum_day", folder = paste0(dir_path, "/02_results/dst_summary/"))
masterias_DVM_sum_week <- load_data(filestring = "masterias_DVM_sum_week", folder = paste0(dir_path, "/02_results/dst_summary/"))
masterias_DVM_sum_month <- load_data(filestring = "masterias_DVM_sum_month", folder = paste0(dir_path, "/02_results/dst_summary/"))
