# Script to calculate the Residency Index from acoustic detection data

# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_data.R"))

# overall residency index, detected days / days of liberty
batterytime <- 518 #days, as stated on the ETN website (acoustic-archival > tags > eg. "1293296" > Field: TAG_ESTIMATED_LIFETIME)
masterias_RI_all <- masterias_detections_clean %>% group_by(tag_serial_number, sex) %>% summarise(
  release = release_dateyear %>% unique(),
  min_detect = min(detection_dateyear), 
  max_detect = max(detection_dateyear), 
  days_at_liberty = (max_detect - release) %>% as.numeric(), 
  days_detected = detection_dateyear %>% unique() %>% length(),
  RI_all = days_detected / batterytime) 
masterias_RI_all %>% View()

# get days that an individual was detected in a certain area
masterias_RI_all <- masterias_RI_all %>% left_join(
  masterias_detections_clean  %>% mutate(count = 1) %>% group_by(tag_serial_number) %>% summarise(days_BPNS = sum(count[area == "BPNS"]), days_WS1 = sum(count[area == "WS1"]), days_WS2 = sum(count[area == "WS2"])),
  by = "tag_serial_number")

# # merge that with masterias_detections_clean
# masterias_detections_clean <- masterias_detections_clean %>% left_join(masterias_RI_all %>% dplyr::select(tag_serial_number, RI_all), by = "tag_serial_number")

# monthly RI per station
masterias_RI_month <- masterias_detections_clean %>% group_by(tag_serial_number, month_name, sex) %>%
  summarise(days_detected = detection_dateyear %>% unique() %>% length(),
            month_days = month %>% lubridate::days_in_month() %>% as.numeric() %>% unique(),
            RI_month = days_detected / month_days) 
# masterias_RI_month %>% View()

masterias_RI_month_summary <- masterias_RI_month %>% group_by(month_name, sex) %>% summarise(mean_RI_month = mean(RI_month), sd_RI_month = sd(RI_month))

masterias_RI_area <- masterias_detections_clean %>% group_by(tag_serial_number, month_name, area, sex) %>%
  summarise(days_detected = detection_dateyear %>% unique() %>% length(),
            month_days = month %>% lubridate::days_in_month() %>% as.numeric() %>% unique(),
            RI_month = days_detected / month_days) 
# masterias_RI_area %>% View()
