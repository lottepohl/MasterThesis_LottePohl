# Script to load models 

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## run models
# paste0(dir_path, "/01_code/04_analyses/moon_fraction_illuminated_lm.R") %>% base::source()

# load model data

data_lm_308_day <- load_data(filestring = "data_lm_308_day", folder = models_path)
data_lm_308_night <- load_data(filestring = "data_lm_308_night", folder = models_path)

data_lm_321_day <- load_data(filestring = "data_lm_321_day", folder = models_path)
data_lm_321_night <- load_data(filestring = "data_lm_321_night", folder = models_path)

# load models
# 
# lm_308_day_depthmedian_moonfraq <- load_data(filestring = "lm_308_day_depthmedian_moonfraq", folder = models_path)
# lm_308_night_depthmedian_moonfraq <- load_data(filestring = "lm_308_night_depthmedian_moonfraq", folder = models_path)
# 
# lm_321_day_depthmedian_moonfraq <- load_data(filestring = "lm_321_day_depthmedian_moonfraq", folder = models_path)
# lm_321_night_depthmedian_moonfraq <- load_data(filestring = "lm_321_night_depthmedian_moonfraq", folder = models_path)
# 
# wilcox_308_depth_median_daynight <- load_data(filestring = "wilcox_308_depth_median_daynight", folder = models_path)
# wilcox_321_depth_median_daynight <- load_data(filestring = "wilcox_321_depth_median_daynight", folder = models_path)

# load hypothesis results

hyp1_results <- load_data(filestring = "hyp1_results", folder = models_path)
hypothesis2.2_testresults <- load_data(filestring = "hypothesis2.2_testresults", folder = models_path)
ks_results_hyp2.2 <- load_data(filestring = "ks_results_hyp2.2", folder = models_path)
