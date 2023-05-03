# script to load results from the hackathon for the vertical space use analysis

# rm(list = ls())

# source(paste0(getwd(), "/01_code/04_analyses/change_point_detection_rulsif.R")) # load analysis script
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_results <- paste0(dir_path, "/02_results/acoustic_vertical_space_use/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()


summary_all2 <- load_data(filestring = "summary_all2", folder = path_results)
summary_wide2 <- load_data(filestring = "summary_wide2", folder = path_results)
