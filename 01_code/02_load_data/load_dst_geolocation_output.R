# Script to load the dst geolocation output data

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()

# source(paste0(dir_path, "/02_scripts/03_wrangle_data/wrangle_dst_geolocation_output.R"))

masterias_dst_geolocation_output <- load_data("masterias_dst_geolocation_output", paste0(dir_path, "/00_data/dst_rawdata/"))
