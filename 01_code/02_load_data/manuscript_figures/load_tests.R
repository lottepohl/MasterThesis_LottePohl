# Script to load statistical test results


# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()


## run tests
# paste0(dir_path, "/01_code/04_analyses/moon_fraction_illuminated_lm.R") %>% base::source()

# load models

wilcox_308_depth_median_daynight <- load_data(filestring = "wilcox_308_depth_median_daynight", folder = models_path)
wilcox_308_depth_min_daynight <- load_data(filestring = "wilcox_308_depth_min_daynight", folder = models_path)

wilcox_321_depth_median_daynight <- load_data(filestring = "wilcox_321_depth_median_daynight", folder = models_path)
wilcox_321_depth_min_daynight <- load_data(filestring = "wilcox_321_depth_min_daynight", folder = models_path)
