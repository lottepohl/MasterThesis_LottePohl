# Script to execute wavelet analysis on the two longterm dst datasets from tag 1293308 and tag 1293321

# Workspace ####

## libraries ####
library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
library(zoo)

# rm(list = ls())

## data ####
# paste0(getwd(), "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
source(paste0(dir_path, "/01_code/06_functions/compute_wavelettransform.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R"))

data_path <- paste0(dir_path, "/02_results/dst_wavelet/")

# dates ####
dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

# compute wavelets ####

## tag 321 ####
# wt_321_mediandepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                               filter(tag_serial_number == "1293321") %>%
#                                               dplyr::select(depth_median_roll3),
#                                             dt = 1,
#                                             factor_smallest_scale = 2)
# wt_df_321_mediandepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_roll3)

wt_321_mediandepth <- compute_wavelet(parameter = long_dst_date %>% 
                                              filter(tag_serial_number == "1293321") %>%
                                              dplyr::select(depth_median),
                                            dt = 1,
                                            factor_smallest_scale = 2)
wt_df_321_mediandepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth)

wt_321_meandepth <- compute_wavelet(parameter = long_dst_date %>% 
                                        filter(tag_serial_number == "1293321") %>%
                                        dplyr::select(depth_mean),
                                      dt = 1,
                                      factor_smallest_scale = 2)
wt_df_321_meandepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_meandepth)

wt_321_mindepth <- compute_wavelet(parameter = long_dst_date %>% 
                                      filter(tag_serial_number == "1293321") %>%
                                      dplyr::select(depth_min),
                                    dt = 1,
                                    factor_smallest_scale = 2)
wt_df_321_mindepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth)

# wt_321_mindepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                      filter(tag_serial_number == "1293321") %>%
#                                      dplyr::select(depth_min_roll3),
#                                    dt = 1,
#                                    factor_smallest_scale = 2) #wt error
# wt_df_321_mindepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth_roll3)

wt_321_maxdepth <- compute_wavelet(parameter = long_dst_date %>% 
                                     filter(tag_serial_number == "1293321") %>%
                                     dplyr::select(depth_max),
                                   dt = 1,
                                   factor_smallest_scale = 2)
wt_df_321_maxdepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth)
# 
# wt_321_maxdepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                      filter(tag_serial_number == "1293321") %>%
#                                      dplyr::select(depth_max_roll3),
#                                    dt = 1,
#                                    factor_smallest_scale = 2)
# wt_df_321_maxdepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth_roll3)
# 
# wt_321_depthsd <- compute_wavelet(parameter = long_dst_date %>% 
#                                      filter(tag_serial_number == "1293321") %>%
#                                      dplyr::select(depth_sd),
#                                    dt = 1,
#                                    factor_smallest_scale = 2)
# wt_df_321_depthsd <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthsd)
# 
# wt_321_depthvar <- compute_wavelet(parameter = long_dst_date %>% 
#                                     filter(tag_serial_number == "1293321") %>%
#                                     dplyr::select(depth_var),
#                                   dt = 1,
#                                   factor_smallest_scale = 2)
# wt_df_321_depthvar <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthvar)
# 
# wt_321_vertspeedmax <- compute_wavelet(parameter = long_dst_date %>% 
#                                     filter(tag_serial_number == "1293321") %>%
#                                     dplyr::select(vertical_speed_max),
#                                   dt = 1,
#                                   factor_smallest_scale = 2)
# wt_df_321_vertspeedmax <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_vertspeedmax)

wt_321_depthrange <- compute_wavelet(parameter = long_dst_date %>% 
                                                     filter(tag_serial_number == "1293321") %>%
                                                     dplyr::select(depth_range),
                                                   dt = 1,
                                                   factor_smallest_scale = 2)
wt_df_321_depthrange <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange)
# 
# wt_321_depthrange_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                        filter(tag_serial_number == "1293321") %>%
#                                        dplyr::select(depth_range_roll3),
#                                      dt = 1,
#                                      factor_smallest_scale = 2)
# wt_df_321_depthrange_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange_roll3)
# 
# wt_321_mediandepth_change <- compute_wavelet(parameter = long_dst_date %>% 
#                                                      filter(tag_serial_number == "1293321") %>%
#                                                      dplyr::select(depth_median_change),
#                                                    dt = 1,
#                                                    factor_smallest_scale = 2)
# wt_df_321_mediandepth_change <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change)
# 
# wt_321_mediandepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                      filter(tag_serial_number == "1293321") %>%
#                                                      dplyr::select(depth_median_change_roll3),
#                                                    dt = 1,
#                                                    factor_smallest_scale = 2)
# wt_df_321_mediandepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change_roll3)
# 
# wt_321_mediandepth_change2 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                filter(tag_serial_number == "1293321") %>%
#                                                dplyr::select(depth_median_change2),
#                                              dt = 1,
#                                              factor_smallest_scale = 2)
# wt_df_321_mediandepth_change2 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change2)
# 
# wt_321_mediandepth_change2_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                 filter(tag_serial_number == "1293321") %>%
#                                                 dplyr::select(depth_median_change2_roll3),
#                                               dt = 1,
#                                               factor_smallest_scale = 2)
# wt_df_321_mediandepth_change2_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change2_roll3)
# 
# wt_321_depthrange_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                filter(tag_serial_number == "1293321") %>%
#                                                dplyr::select(depth_range_change_roll3),
#                                              dt = 1,
#                                              factor_smallest_scale = 2)
# wt_df_321_depthrange_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange_change_roll3)
# 
# wt_321_mindepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                     filter(tag_serial_number == "1293321") %>%
#                                                     dplyr::select(depth_min_change_roll3),
#                                                   dt = 1,
#                                                   factor_smallest_scale = 2)
# wt_df_321_mindepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth_change_roll3)
# 
# wt_321_maxdepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                     filter(tag_serial_number == "1293321") %>%
#                                                     dplyr::select(depth_max_change_roll3),
#                                                   dt = 1,
#                                                   factor_smallest_scale = 2)
# wt_df_321_maxdepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth_change_roll3)

### sgolay filtered #####
wt_321_mediandepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                        filter(tag_serial_number == "1293321") %>%
                                        dplyr::select(depth_median_sgolay),
                                      dt = 1,
                                      factor_smallest_scale = 2)
wt_df_321_mediandepth_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_sgolay)

wt_321_depthrange_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293321") %>%
                                               dplyr::select(depth_range_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_321_depthrange_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange_sgolay)


wt_321_mindepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293321") %>%
                                               dplyr::select(depth_min_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_321_mindepth_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth_sgolay)

wt_321_maxdepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293321") %>%
                                               dplyr::select(depth_max_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_321_maxdepth_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth_sgolay)


## tag 308 ####
wt_308_mediandepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
                                              filter(tag_serial_number == "1293308") %>%
                                              dplyr::select(depth_median_roll3),
                                            dt = 1,
                                            factor_smallest_scale = 2)
wt_df_308_mediandepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_roll3)

wt_308_mediandepth <- compute_wavelet(parameter = long_dst_date %>% 
                                        filter(tag_serial_number == "1293308") %>%
                                        dplyr::select(depth_median),
                                      dt = 1,
                                      factor_smallest_scale = 2)
wt_df_308_mediandepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth)

wt_308_meandepth <- compute_wavelet(parameter = long_dst_date %>% 
                                      filter(tag_serial_number == "1293308") %>%
                                      dplyr::select(depth_mean),
                                    dt = 1,
                                    factor_smallest_scale = 2)
wt_df_308_meandepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_meandepth)

wt_308_mindepth <- compute_wavelet(parameter = long_dst_date %>% 
                                     filter(tag_serial_number == "1293308") %>%
                                     dplyr::select(depth_min),
                                   dt = 1,
                                   factor_smallest_scale = 2)
wt_df_308_mindepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth)

# wt_308_mindepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                            filter(tag_serial_number == "1293308") %>%
#                                            dplyr::select(depth_min_roll3),
#                                          dt = 1,
#                                          factor_smallest_scale = 2) #wt error
# wt_df_308_mindepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth_roll3)

wt_308_maxdepth <- compute_wavelet(parameter = long_dst_date %>% 
                                     filter(tag_serial_number == "1293308") %>%
                                     dplyr::select(depth_max),
                                   dt = 1,
                                   factor_smallest_scale = 2)
wt_df_308_maxdepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth)
# 
# wt_308_maxdepth_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                            filter(tag_serial_number == "1293308") %>%
#                                            dplyr::select(depth_max_roll3),
#                                          dt = 1,
#                                          factor_smallest_scale = 2)
# wt_df_308_maxdepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth_roll3)
# 
# wt_308_depthsd <- compute_wavelet(parameter = long_dst_date %>% 
#                                     filter(tag_serial_number == "1293308") %>%
#                                     dplyr::select(depth_sd),
#                                   dt = 1,
#                                   factor_smallest_scale = 2)
# wt_df_308_depthsd <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthsd)
# 
# wt_308_depthvar <- compute_wavelet(parameter = long_dst_date %>% 
#                                      filter(tag_serial_number == "1293308") %>%
#                                      dplyr::select(depth_var),
#                                    dt = 1,
#                                    factor_smallest_scale = 2)
# wt_df_308_depthvar <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthvar)
# 
# wt_308_vertspeedmax <- compute_wavelet(parameter = long_dst_date %>% 
#                                          filter(tag_serial_number == "1293308") %>%
#                                          dplyr::select(vertical_speed_max),
#                                        dt = 1,
#                                        factor_smallest_scale = 2)
# wt_df_308_vertspeedmax <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_vertspeedmax)

wt_308_depthrange <- compute_wavelet(parameter = long_dst_date %>% 
                                       filter(tag_serial_number == "1293308") %>%
                                       dplyr::select(depth_range),
                                     dt = 1,
                                     factor_smallest_scale = 2)
wt_df_308_depthrange <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange)

# wt_308_depthrange_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                              filter(tag_serial_number == "1293308") %>%
#                                              dplyr::select(depth_range_roll3),
#                                            dt = 1,
#                                            factor_smallest_scale = 2)
# wt_df_308_depthrange_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange_roll3)
# 
# wt_308_mediandepth_change <- compute_wavelet(parameter = long_dst_date %>% 
#                                                filter(tag_serial_number == "1293308") %>%
#                                                dplyr::select(depth_median_change),
#                                              dt = 1,
#                                              factor_smallest_scale = 2)
# wt_df_308_mediandepth_change <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change)
# 
# wt_308_mediandepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                      filter(tag_serial_number == "1293308") %>%
#                                                      dplyr::select(depth_median_change_roll3),
#                                                    dt = 1,
#                                                    factor_smallest_scale = 2)
# wt_df_308_mediandepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change_roll3)
# 
# wt_308_mediandepth_change2 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                 filter(tag_serial_number == "1293308") %>%
#                                                 dplyr::select(depth_median_change2),
#                                               dt = 1,
#                                               factor_smallest_scale = 2)
# wt_df_308_mediandepth_change2 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change2)
# 
# wt_308_mediandepth_change2_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                       filter(tag_serial_number == "1293308") %>%
#                                                       dplyr::select(depth_median_change2_roll3),
#                                                     dt = 1,
#                                                     factor_smallest_scale = 2)
# wt_df_308_mediandepth_change2_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change2_roll3)
# 
# wt_308_depthrange_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                     filter(tag_serial_number == "1293308") %>%
#                                                     dplyr::select(depth_range_change_roll3),
#                                                   dt = 1,
#                                                   factor_smallest_scale = 2)
# wt_df_308_depthrange_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange_change_roll3)
# 
# wt_308_mindepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                   filter(tag_serial_number == "1293308") %>%
#                                                   dplyr::select(depth_min_change_roll3),
#                                                 dt = 1,
#                                                 factor_smallest_scale = 2)
# wt_df_308_mindepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth_change_roll3)
# 
# wt_308_maxdepth_change_roll3 <- compute_wavelet(parameter = long_dst_date %>% 
#                                                   filter(tag_serial_number == "1293308") %>%
#                                                   dplyr::select(depth_max_change_roll3),
#                                                 dt = 1,
#                                                 factor_smallest_scale = 2)
# wt_df_308_maxdepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth_change_roll3)

### sgolay filtered #####
wt_308_mediandepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293308") %>%
                                               dplyr::select(depth_median_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_308_mediandepth_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_sgolay)

wt_308_depthrange_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                              filter(tag_serial_number == "1293308") %>%
                                              dplyr::select(depth_range_sgolay),
                                            dt = 1,
                                            factor_smallest_scale = 2)
wt_df_308_depthrange_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange_sgolay)

wt_308_mindepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                            filter(tag_serial_number == "1293308") %>%
                                            dplyr::select(depth_min_sgolay),
                                          dt = 1,
                                          factor_smallest_scale = 2)
wt_df_308_mindepth_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth_sgolay)

wt_308_maxdepth_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                            filter(tag_serial_number == "1293308") %>%
                                            dplyr::select(depth_max_sgolay),
                                          dt = 1,
                                          factor_smallest_scale = 2)
wt_df_308_maxdepth_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth_sgolay)

# save data ####

## tag 321 ####
### raw wavelet result ####
# save_data(data = wt_321_mediandepth_roll3, folder = data_path)
save_data(data = wt_321_depthrange_sgolay, folder = data_path)
save_data(data = wt_321_mediandepth, folder = data_path)
# save_data(data = wt_321_mediandepth_change_roll3, folder = data_path)
save_data(data = wt_321_depthrange, folder = data_path)
# save_data(data = wt_321_depthrange_roll3, folder = data_path)
# save_data(data = wt_321_maxdepth_change_roll3, folder = data_path)
# save_data(data = wt_321_mindepth_change_roll3, folder = data_path)
# save_data(data = wt_321_depthrange_change_roll3, folder = data_path)
# save_data(data = wt_321_mediandepth_change2_roll3, folder = data_path)
# save_data(data = wt_321_mediandepth_change2, folder = data_path)
# save_data(data = wt_321_mediandepth_change, folder = data_path)
# save_data(data = wt_321_vertspeedmax, folder = data_path)
# save_data(data = wt_321_depthsd, folder = data_path)
# save_data(data = wt_321_depthvar, folder = data_path)
# save_data(data = wt_321_maxdepth_roll3, folder = data_path)
save_data(data = wt_321_maxdepth, folder = data_path)
# save_data(data = wt_321_mindepth_roll3, folder = data_path)
save_data(data = wt_321_mindepth, folder = data_path)
save_data(data = wt_321_meandepth, folder = data_path)
save_data(data = wt_321_mediandepth_sgolay, folder = data_path)
save_data(data = wt_321_mindepth_sgolay, folder = data_path)
save_data(data = wt_321_maxdepth_sgolay, folder = data_path)
### df for gg plotting ####
# save_data(data = wt_df_321_mediandepth_roll3, folder = data_path)
save_data(data = wt_df_321_depthrange_sgolay, folder = data_path)
save_data(data = wt_df_321_mediandepth, folder = data_path)
# save_data(data = wt_df_321_mediandepth_change_roll3, folder = data_path)
# save_data(data = wt_df_321_depthrange, folder = data_path)
# save_data(data = wt_df_321_depthrange_roll3, folder = data_path)
# save_data(data = wt_df_321_maxdepth_change_roll3, folder = data_path)
# save_data(data = wt_df_321_mindepth_change_roll3, folder = data_path)
# save_data(data = wt_df_321_depthrange_change_roll3, folder = data_path)
# save_data(data = wt_df_321_mediandepth_change2_roll3, folder = data_path)
# save_data(data = wt_df_321_mediandepth_change2, folder = data_path)
# save_data(data = wt_df_321_mediandepth_change, folder = data_path)
# save_data(data = wt_df_321_vertspeedmax, folder = data_path)
# save_data(data = wt_df_321_depthsd, folder = data_path)
# save_data(data = wt_df_321_depthvar, folder = data_path)
# save_data(data = wt_df_321_maxdepth_roll3, folder = data_path)
save_data(data = wt_df_321_maxdepth, folder = data_path)
# save_data(data = wt_df_321_mindepth_roll3, folder = data_path)
save_data(data = wt_df_321_mindepth, folder = data_path)
save_data(data = wt_df_321_meandepth, folder = data_path)
save_data(data = wt_df_321_mediandepth_sgolay, folder = data_path)
save_data(data = wt_df_321_mindepth_sgolay, folder = data_path)
save_data(data = wt_df_321_maxdepth_sgolay, folder = data_path)


## tag 308 ####
### raw wavelet result ####
# save_data(data = wt_308_mediandepth_roll3, folder = data_path)
save_data(data = wt_308_depthrange_sgolay, folder = data_path)
save_data(data = wt_308_mediandepth, folder = data_path)
# save_data(data = wt_308_mediandepth_change_roll3, folder = data_path)
save_data(data = wt_308_depthrange, folder = data_path)
# save_data(data = wt_308_depthrange_roll3, folder = data_path)
# save_data(data = wt_308_maxdepth_change_roll3, folder = data_path)
# save_data(data = wt_308_mindepth_change_roll3, folder = data_path)
# save_data(data = wt_308_depthrange_change_roll3, folder = data_path)
# save_data(data = wt_308_mediandepth_change2_roll3, folder = data_path)
# save_data(data = wt_308_mediandepth_change2, folder = data_path)
# save_data(data = wt_308_mediandepth_change, folder = data_path)
# save_data(data = wt_308_vertspeedmax, folder = data_path)
# save_data(data = wt_308_depthsd, folder = data_path)
# save_data(data = wt_308_depthvar, folder = data_path)
# save_data(data = wt_308_maxdepth_roll3, folder = data_path)
save_data(data = wt_308_maxdepth, folder = data_path)
# save_data(data = wt_308_mindepth_roll3, folder = data_path)
save_data(data = wt_308_mindepth, folder = data_path)
save_data(data = wt_308_meandepth, folder = data_path)
save_data(data = wt_308_mediandepth_sgolay, folder = data_path)
save_data(data = wt_308_mindepth_sgolay, folder = data_path)
save_data(data = wt_308_maxdepth_sgolay, folder = data_path)
### df for gg plotting ####
# save_data(data = wt_df_308_mediandepth_roll3, folder = data_path)
save_data(data = wt_df_308_depthrange_sgolay, folder = data_path)
save_data(data = wt_df_308_mediandepth, folder = data_path)
# save_data(data = wt_df_308_mediandepth_change_roll3, folder = data_path)
save_data(data = wt_df_308_depthrange, folder = data_path)
# save_data(data = wt_df_308_depthrange_roll3, folder = data_path)
# save_data(data = wt_df_308_maxdepth_change_roll3, folder = data_path)
# save_data(data = wt_df_308_mindepth_change_roll3, folder = data_path)
# save_data(data = wt_df_308_depthrange_change_roll3, folder = data_path)
# save_data(data = wt_df_308_mediandepth_change2_roll3, folder = data_path)
# save_data(data = wt_df_308_mediandepth_change2, folder = data_path)
# save_data(data = wt_df_308_mediandepth_change, folder = data_path)
# save_data(data = wt_df_308_vertspeedmax, folder = data_path)
# save_data(data = wt_df_308_depthsd, folder = data_path)
# save_data(data = wt_df_308_depthvar, folder = data_path)
# save_data(data = wt_df_308_maxdepth_roll3, folder = data_path)
save_data(data = wt_df_308_maxdepth, folder = data_path)
# save_data(data = wt_df_308_mindepth_roll3, folder = data_path)
save_data(data = wt_df_308_mindepth, folder = data_path)
save_data(data = wt_df_308_meandepth, folder = data_path)
save_data(data = wt_df_308_mediandepth_sgolay, folder = data_path)
save_data(data = wt_df_308_mindepth_sgolay, folder = data_path)
save_data(data = wt_df_308_maxdepth_sgolay, folder = data_path)

# hourly wavelet ####
# dt_smallperiods <-  (1/30) #0.5 #hour, for now: but that also means that I have to downsample the raw depthlog
dt_smallperiods <-  (1/6) # every 10 min
## tag 321 ####


wt_321_depth_hr <- compute_wavelet_hrperiod(depthlog = masterias_depth_temp,
                                            tag_serial_num = "1293321",
                                            parameter_name = "depth_m",
                                            dt = dt_smallperiods,
                                            factor_smallest_scale = 24) # value every 10 mins: factor 24 == min. 4hr period

wt_df_321_depth_hr <- wavelet_output_compare_hrperiod(depthlog = masterias_depth_temp,
                                                      tag_serial_num = "1293321",
                                                      parameter_name = "depth_m",
                                                      dt = dt_smallperiods,
                                                      wt_output = wt_321_depth_hr)

# p_wt_321_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_321_depth_hr,
#                                            type = "power_log",
#                                            date = TRUE,
#                                            max_period = 72)

## tag 308 ####

wt_308_depth_hr <- compute_wavelet_hrperiod(depthlog = masterias_depth_temp,
                                            tag_serial_num = "1293308",
                                            parameter_name = "depth_m",
                                            dt = dt_smallperiods,
                                            factor_smallest_scale = 24)

wt_df_308_depth_hr <- wavelet_output_compare_hrperiod(depthlog = masterias_depth_temp,
                                                      tag_serial_num = "1293308",
                                                      parameter_name = "depth_m",
                                                      dt = dt_smallperiods,
                                                      wt_output = wt_308_depth_hr)

# p_wt_308_depth_hr <- plot_wavelet_hrperiod(wt_df = wt_df_308_depth_hr,
#                                            type = "power_log",
#                                            date = TRUE,
#                                            max_period = 72)

# save data ####

save_data(data = wt_321_depth_hr, folder = data_path)
save_data(data = wt_df_321_depth_hr, folder = data_path)

save_data(data = wt_308_depth_hr, folder = data_path)
save_data(data = wt_df_308_depth_hr, folder = data_path)
