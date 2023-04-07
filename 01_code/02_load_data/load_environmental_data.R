# Script to load environmental data

# From the Scheldemonitor: https://www.scheldemonitor.org/dataproducts/en/download/periodic/explore# ####

# rm(list = ls())

# source(paste0(dir_path, "/functions.R"))
paste0("C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/01_code/06_functions/functions.R") %>% source()


# SM_salinity <- load_data(filestring = "SM_salinity", folder = path_envdata)
SM_salinity_summary <- load_data(filestring = "SM_salinity_summary", folder = path_envdata)
SM_salinity_summary_month <- load_data(filestring = "SM_salinity_summary_month", folder = path_envdata)
