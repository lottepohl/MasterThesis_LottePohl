# script to load tables for thesis manuscript

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
tables_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/tables/")
metadata_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/raw_metadata/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

# # run code to generate tables
# paste0(dir_path, "/01_code/05_plots_maps/thesis_manuscript_tables.R")

dst_summary <- load_data(filestring = "dst_summary", folder = tables_path)
tagged_animal_info <- load_data(filestring = "tagged_animal_info", folder = tables_path)
release_locations <- load_data(filestring = "release_locations", folder = tables_path)
abbreviations_list <- load_data(filestring = "abbreviations_list", folder = tables_path)
detections_month <- load_data(filestring = "detections_month", folder = tables_path)
detections_sum_station <- load_data(filestring = "detections_sum_station", folder = tables_path)
detections_OG102019 <- load_data(filestring = "detections_OG102019", folder = tables_path)
OG10_2019_RI <- load_data(filestring = "OG10_2019_RI", folder = tables_path)

daynight_depth_308_ttest <- load_data(filestring = "daynight_depth_308_ttest", folder = tables_path)
daynight_depth_321_ttest <- load_data(filestring = "daynight_depth_321_ttest", folder = tables_path)

# lm_sum_308_depthmedian_day_night <- load_data(filestring = "lm_sum_308_depthmedian_day_night", folder = tables_path)
# lm_sum_321_depthmedian_day_night <- load_data(filestring = "lm_sum_321_depthmedian_day_night", folder = tables_path)

# rulsif_308_table_2_5percent <- load_data(filestring = "rulsif_308_table_2_5percent", folder = tables_path)
# rulsif_308_table_5percent <- load_data(filestring = "rulsif_308_table_5percent", folder = tables_path)
# rulsif_308_table_10percent <- load_data(filestring = "rulsif_308_table_10percent", folder = tables_path)
# 
# rulsif_321_table_2_5percent <- load_data(filestring = "rulsif_321_table_2_5percent", folder = tables_path)
# rulsif_321_table_5percent <- load_data(filestring = "rulsif_321_table_5percent", folder = tables_path)
# rulsif_321_table_10percent <- load_data(filestring = "rulsif_321_table_10percent", folder = tables_path)

# load metadata tables ####

acoustic_detections_metadata <- load_data(filestring = "acoustic_detections_metadata", folder = metadata_path)
DST_logs_metadata <- load_data(filestring = "DST_logs_metadata", folder = metadata_path)
geolocation_output_metadata <- load_data(filestring = "geolocation_output_metadata", folder = metadata_path)
tagged_individuals_metadata <- load_data(filestring = "tagged_individuals_metadata", folder = metadata_path)