# Script to load and wrangle the DST geolocation model output

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
source(paste0(dir_path, "/functions.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_acoustic_dectections.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_dst_geolocation_output.R"))

# READ DST ####

## mpt_1293295_Det0 <- read_csv("../dst_data/01_modelled_tracks/1293295/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293295_mpt_track_positions.csv", show_col_types = FALSE)
## tag 295 ####
mpt_1293295_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293295/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293295_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293295")
mode_1293295_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293295/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293295_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293295")
mean_1293295_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293295/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293295_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293295")

all_295_Det1 <- mpt_1293295_Det1 %>% left_join(mean_1293295_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293295_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293295_Det1, mean_1293295_Det1, mode_1293295_Det1)

## tag 304 ####
mpt_1293304_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293304/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293304_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293304")
mode_1293304_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293304/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293304_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293304")
mean_1293304_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293304/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293304_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293304")

all_304_Det0 <- mpt_1293304_Det0 %>% left_join(mean_1293304_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293304_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293304_Det0, mean_1293304_Det0, mode_1293304_Det0)

## tag 308 ####
mpt_1293308_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293308/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293308_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293308")
mode_1293308_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293308/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293308_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293308")
mean_1293308_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293308/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293308_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293308")

all_308_Det1 <- mpt_1293308_Det1 %>% left_join(mean_1293308_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293308_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293308_Det1, mean_1293308_Det1, mode_1293308_Det1)

## tag 310 ####
mpt_1293310_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293310/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293310_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293310")
mode_1293310_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293310/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293310_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293310")
mean_1293310_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293310/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293310_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293310")

all_310_Det0 <- mpt_1293310_Det0 %>% left_join(mean_1293310_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293310_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293310_Det0, mean_1293310_Det0, mode_1293310_Det0)

## tag 312 ####
mpt_1293312_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293312/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293312_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293312")
mode_1293312_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293312/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293312_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293312")
mean_1293312_Det0 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293312/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293312_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293312")

all_312_Det0 <- mpt_1293312_Det0 %>% left_join(mean_1293312_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293312_Det0 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293312_Det0, mean_1293312_Det0, mode_1293312_Det0)

## tag 319 ####
## mpt_1293319_Det0 <- read_csv("../dst_data/01_modelled_tracks/1293319/Sat0_Catch1_Det0_Behav0_res1.0/D_mle/Tracks/MA_SN1293319_mpt_track_positions.csv"), show_col_types = FALSE)
mpt_1293319_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293319/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293319_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293319")
mode_1293319_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293319/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293319_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293319")
mean_1293319_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293319/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293319_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293319")

all_319_Det1 <- mpt_1293319_Det1 %>% left_join(mean_1293319_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293319_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293319_Det1, mean_1293319_Det1, mode_1293319_Det1)

## tag 321 ####
mpt_1293321_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293321/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293321_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293321")
mode_1293321_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293321/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293321_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293321")
mean_1293321_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293321/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293321_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293321")

all_321_Det1 <- mpt_1293321_Det1 %>% left_join(mean_1293321_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293321_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293321_Det1, mean_1293321_Det1, mode_1293321_Det1)

## tag 322 ####
mpt_1293322_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293322/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293322_mpt_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293322")
mode_1293322_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293322/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293322_mode_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293322")
mean_1293322_Det1 <- read_csv(paste0(dir_path, "/dst_data/01_modelled_tracks/1293322/Sat0_Catch1_Det1_Behav0_res1.0/D_mle/Tracks/MA_SN1293322_mean_track_positions.csv"), show_col_types = FALSE) %>% mutate(tag_serial_number = "1293322")

all_322_Det1 <- mpt_1293322_Det1 %>% left_join(mean_1293322_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date") %>% left_join(mode_1293322_Det1 %>% dplyr::select(date, long_dg, lat_dg), by = "date")
rm(mpt_1293322_Det1, mean_1293322_Det1, mode_1293322_Det1)


mpts_all <- rbind(all_295_Det1, all_304_Det0, all_308_Det1, all_310_Det0, all_312_Det0, all_319_Det1, all_321_Det1, all_322_Det1) %>% 
  rename(detection_latitude = lat_dg.x, detection_longitude = long_dg.x,
         detection_latitude_mean = lat_dg.y, detection_longitude_mean = long_dg.y,
         detection_latitude_mode = lat_dg, detection_longitude_mode = long_dg,
         date_time = date) %>%
  dplyr::select(tag_serial_number, date_time, detection_latitude, detection_longitude, detection_latitude_mean, detection_longitude_mean, detection_latitude_mode, detection_longitude_mode)


## change date format
mpts_all$date <- mpts_all$date_time %>% format("%m-%d")
# mpts_all$tag_serial_number <- mpts_all$tag_serial_number %>% as.factor()
# mpts_all$tag_serial_number <- mpts_all$tag_serial_number %>% as.character()

masterias_dst_geolocation_output <- mpts_all %>% left_join(masterias_info %>% dplyr::select(tag_serial_number, sex, life_stage, length1, weight), by = "tag_serial_number")

# mpts_short <- mpts_all %>% filter(!tag_serial_number %in% c("1293308", "1293321"))

# Save file
save_data(masterias_dst_geolocation_output, folder = paste0(dir_path, "/03_data/dst_rawdata/"))

# remove single files
# rm(mpt_1293295_Det1, mpt_1293304_Det0, mpt_1293308_Det1, mpt_1293310_Det0, mpt_1293312_Det0, mpt_1293319_Det1, mpt_1293321_Det1, mpt_1293322_Det1)
rm(mpts_all, all_295_Det1, all_304_Det0, all_308_Det1, all_310_Det0, all_312_Det0, all_319_Det1, all_321_Det1, all_322_Det1)

