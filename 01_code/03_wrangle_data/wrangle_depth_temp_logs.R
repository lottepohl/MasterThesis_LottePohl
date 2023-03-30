# Script to read in and wrangle depth and temperature data of the data storage tags

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/01_code/06_functions/functions_DST_presstemplogs.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_geolocation_output.R"))
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()

colnames <- c("ADST DATA LOG", "1.0.0", "AdstConverter-1.0.5", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11")

# Read in pressure/depth files
press_295 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293295/SN1293295_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_304 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293304/SN1293304_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_308 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293308/SN1293308_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_310 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293310/SN1293310_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_312 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293312/SN1293312_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_319 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293319/SN1293319_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_322 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293322/SN1293322_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)
press_321 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293321/SN1293321_pressure_log.csv"), show_col_types = FALSE, col_names = colnames)

# Read in temperature files
temp_295 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293295/SN1293295_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_304 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293304/SN1293304_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_308 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293308/SN1293308_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_310 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293310/SN1293310_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_312 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293312/SN1293312_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_319 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293319/SN1293319_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_322 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293322/SN1293322_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)
temp_321 <- read_csv(paste0(dir_path, "/dst_data/02_pressure_temperature/ADST_log_SN1293321/SN1293321_temperature_log.csv"), show_col_types = FALSE, col_names = colnames)

# Unite pressure and temperature files
press_temp_295 <- create_press_temp(press_295, temp_295, "1293295", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_304 <- create_press_temp(press_304, temp_304, "1293304", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_308 <- create_press_temp(press_308, temp_308, "1293308", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_310 <- create_press_temp(press_310, temp_310, "1293310", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_312 <- create_press_temp(press_312, temp_312, "1293312", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_319 <- create_press_temp(press_319, temp_319, "1293319", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_322 <- create_press_temp(press_322, temp_322, "1293322", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)
press_temp_321 <- create_press_temp(press_321, temp_321, "1293321", cutoff_days = 10, dst_output = masterias_dst_geolocation_output)

masterias_depth_temp <- rbind(press_temp_295 %>% mutate(tag_serial_number = "1293295"),
                              press_temp_304 %>% mutate(tag_serial_number = "1293304"),
                              press_temp_308 %>% mutate(tag_serial_number = "1293308"),
                              press_temp_310 %>% mutate(tag_serial_number = "1293310"),
                              press_temp_312 %>% mutate(tag_serial_number = "1293312"),
                              press_temp_319 %>% mutate(tag_serial_number = "1293319"),
                              press_temp_321 %>% mutate(tag_serial_number = "1293321"),
                              press_temp_322 %>% mutate(tag_serial_number = "1293322"))

# make time vector in hours
masterias_depth_temp <- masterias_depth_temp %>% group_by(tag_serial_number) %>%
  # make time vector
  mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric()) %>% 
  #filter out NAs
  filter(!is.na(depth_m))

save_data(masterias_depth_temp, folder = paste0(dir_path, "/03_data/dst_rawdata/"))

# remove temporary files
rm(press_295, press_304, press_308, press_310, press_312, press_319, press_321, press_322,
   press_temp_295, press_temp_304, press_temp_308, press_temp_310, press_temp_312, press_temp_319,
   press_temp_321, press_temp_322, temp_295, temp_304, temp_308, temp_310, temp_312, temp_319,
   temp_321, temp_322)
