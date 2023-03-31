# Script to load the depth and temperature logs

# rm(list = ls())

dir_path <- getwd() # "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()

# source(paste0(dir_path, "/02_scripts/02_load_data/load_acoustic_detections.R"))
# source(paste0(dir_path, "/02_scripts/03_wrangle_data/wrangle_depth_temp_logs.R"))

masterias_depth_temp <- load_data("masterias_depth_temp", paste0(dir_path, "/00_data/dst_rawdata/"))

death_dates <- tibble(
  tag_serial_number = c("1293295", "1293319", "1293322", "1293304", "1293310", "1293312", "1293308", "1293321"),
  death_date = c("2018-08-20", "2018-08-08", "2018-08-08", "2019-07-21", "2019-08-13", "2019-08-07", "2019-12-12", "2019-12-12"))
