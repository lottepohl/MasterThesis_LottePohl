
# script to export info about m. asterias for other people to take a look
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

sharks_info <- masterias_info %>% dplyr::select(tag_serial_number,  
                                                scientific_name,
                                                # n_detect,
                                                release_date_time,
                                                release_latitude,
                                                release_longitude,
                                                length1,
                                                sex
                                                ) %>%
  rename(length_cm = length1)
# , 
         # times_detected = n_detect) 
# %>% mutate(times_detected = ifelse(is.na(times_detected), 0, times_detected))

# sharks_acoustic_detections <- masterias_detections_clean

sharks_geolocation_modeloutput <- mpts_all %>% dplyr::select(tag_serial_number,
                                                             date_time,
                                                             detection_latitude,
                                                             detection_longitude) %>%
  rename(latitude = detection_latitude,
         longitude = detection_longitude,
         date = date_time)

sharks_info$tag_serial_number %>% class()

sharks_depth_temperature <- rbind(press_temp_295 %>% mutate(tag_serial_number = "1293295"),
                                     press_temp_304 %>% mutate(tag_serial_number = "1293304"),
                                     press_temp_308 %>% mutate(tag_serial_number = "1293308"),
                                     press_temp_310 %>% mutate(tag_serial_number = "1293310"),
                                     press_temp_312 %>% mutate(tag_serial_number = "1293312"),
                                     press_temp_319 %>% mutate(tag_serial_number = "1293319"),
                                     press_temp_321 %>% mutate(tag_serial_number = "1293321"),
                                     press_temp_322 %>% mutate(tag_serial_number = "1293322"))

write_csv(sharks_info,paste0(dir_path,"/03_data/dst_rawdata/sharks_info.csv"))
write_csv(sharks_geolocation_modeloutput,paste0(dir_path,"/03_data/dst_rawdata/sharks_geolocation_modeloutput.csv"))
write_csv(sharks_depth_temperature, paste0(dir_path,"/03_data/dst_rawdata/sharks_depth_temperature.csv"))
