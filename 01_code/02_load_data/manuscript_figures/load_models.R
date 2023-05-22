# Script to load models 

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## run models
# paste0(dir_path, "/01_code/04_analyses/moon_fraction_illuminated_lm.R") %>% base::source()

# load models

lm_308_depthmin_moonfraq <- load_data(filestring = "lm_308_depthmin_moonfraq", folder = models_path)
lm_308_depthmedian_moonfraq <- load_data(filestring = "lm_308_depthmedian_moonfraq", folder = models_path)

lm_321_depthmin_moonfraq <- load_data(filestring = "lm_321_depthmin_moonfraq", folder = models_path)
lm_321_depthmedian_moonfraq <- load_data(filestring = "lm_321_depthmedian_moonfraq", folder = models_path)