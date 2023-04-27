# script to load tables for thesis manuscript

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
tables_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/tables/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

# # run code to generate tables
# paste0(dir_path, "/01_code/05_plots_maps/thesis_manuscript_tables.R")

dst_summary <- load_data(filestring = "dst_summary", folder = tables_path)
tagged_animal_info <- load_data(filestring = "tagged_animal_info", folder = tables_path)
release_locations <- load_data(filestring = "release_locations", folder = tables_path)
abbreviations_list <- load_data(filestring = "abbreviations_list", folder = tables_path)