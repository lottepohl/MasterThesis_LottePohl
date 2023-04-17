# Script with functions to perform wavelet output wrangling, pca and k means clustering

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
# library(scales)
library(vegan)
library(ggpubr)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
## paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
## to do: choose df's to load to reduce workspace size
paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
## paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
## paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()

## set figure path ####
result_path <- paste0(dir_path, "/02_results/dst_clustering_wavelet/")

# function ####

make_bins <- function(wt_df, var_name){
  # get significant wavelet results
  wt_df_sig <- wt_df %>% filter(significance >= 1)
  
  bins <- wt_df_sig %>% group_by(date) %>%
    summarise(n_0_2_h = sum(period %>% dplyr::between(0,2)),
              n_2_4_h = sum(period %>% dplyr::between(2,4)),
              n_4_8_h = sum(period %>% dplyr::between(4,8)),
              n_8_16_h = sum(period %>% dplyr::between(8,16)),
              n_16_32_h = sum(period %>% dplyr::between(16,32)),
              n_32_64_h = sum(period %>% dplyr::between(32,64)),
              n_62_128_h = sum(period %>% dplyr::between(64,128)))
  # count %>% where(period %>% dplyr::between(0,4)) %>% sum() )
  
  bins <- bins %>%
    rename_with(~paste0(var_name, .), starts_with("n")) %>%
    mutate(date = date %>% as.POSIXct(tz = "UTC"))
  
  return(bins)
}

# with all relevant wt results ####

## tag 308 ####
# wt_df <- wt_df_321_mediandepth_roll3

med_r3_308 <- make_bins(wt_df = wt_df_308_mediandepth_roll3, var_name = "med_r3_")
med_c_r3_308 <- make_bins(wt_df = wt_df_308_mediandepth_change_roll3, var_name = "med_change_r3_")
max_r3_308 <- make_bins(wt_df = wt_df_308_maxdepth_roll3, var_name = "max_r3_")

cluster_df_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>%
  left_join(med_r3_308, by = "date") %>%
  left_join(med_c_r3_308, by = "date") %>%
  left_join(max_r3_308, by = "date") %>% 
  replace(is.na(.), 0)

## tag 321 ####

med_r3_321 <- make_bins(wt_df = wt_df_321_mediandepth_roll3, var_name = "med_r3_")
med_c_r3_321 <- make_bins(wt_df = wt_df_321_mediandepth_change_roll3, var_name = "med_change_r3_")
max_r3_321 <- make_bins(wt_df = wt_df_321_maxdepth_roll3, var_name = "max_r3_")

cluster_df_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>%
  left_join(med_r3_321, by = "date") %>%
  left_join(med_c_r3_321, by = "date") %>%
  left_join(max_r3_321, by = "date") %>% 
  replace(is.na(.), 0)

# next steps: cluster fuer PCA vorbereiten: nur numerical variables, center and normalise, PCA, dann clustering

# select relevant cols ####

cluster_df_308 <-  cluster_df_308 %>% 
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!c(year, vertical_speed_min)) #%>% View()

cluster_df_321 <-  cluster_df_321 %>% 
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!c(year, vertical_speed_min)) #%>% View()


# transform data ####

## tag 308 ####
ggpubr::ggqqplot(cluster_df_308$depth_median)
test <- cluster_df_308 %>% sqrt()
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_308 %>% log10()
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_308 %>% abs() %>% vegan::decostand(method = "alr")
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_308 %>% vegan::decostand(method = "hellinger") # best for 308
ggpubr::ggqqplot(test$depth_median)
test <- 1 / cluster_df_308$depth_median
ggpubr::ggqqplot(test)

cluster_df_308 <- cluster_df_308 %>%
  vegan::decostand(method = "hellinger") %>%
  replace(is.na(.), 0)

## tag 321 ####
ggpubr::ggqqplot(cluster_df_321$depth_median)
test <- cluster_df_321 %>% sqrt()
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_321 %>% log10()
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_321 %>% abs() %>% vegan::decostand(method = "alr")
ggpubr::ggqqplot(test$depth_median)
test <- cluster_df_321 %>% vegan::decostand(method = "hellinger") # best for 321
ggpubr::ggqqplot(test$depth_median)
test <- 1 / cluster_df_321$depth_median
ggpubr::ggqqplot(test)

cluster_df_321 <- cluster_df_321 %>%
  vegan::decostand(method = "hellinger") %>%
  replace(is.na(.), 0)


# cluster_df_308 %>% replace(is.na(.), 0) %>% is.na() %>% length()
# masterias_depth_summary_308_transformed <- masterias_depth_summary_308 %>% 
#   dplyr::select(where(is.numeric)) %>%
#   dplyr::select(!c(year, vertical_speed_min)) %>%
#   mutate(depth_range = depth_range + 0.01) %>%
#   log10() %>%
#   mutate_all(~ifelse(is.na(.) | is.infinite(.), 0.001, .)) %>%
#   select(-matches("roll"))
# # select_if(~!any(is.na(.)) & !any(is.infinite(.)))
# ggpubr::ggqqplot(masterias_depth_summary_308_transformed$depth_range)



# med_c_r3_308, max_r3_308
# make bins

# var <- "med_r3_"




# doesn't work 
# make_clustering_df <- function(df = long_dst_date, wt_df_list){ #, var_list
#   # length(wt_sf_list) == length(var_list)
#   # for(i in 1:length(wt_df_list)){
#     wt_df_bins <- make_bins(wt_df_list) #, var_list    wt_df_bins <- make_bins(wt_df_list[i], var_list[i])
#     # df <- rbind(df, wt_df_bins)
#   # }
#   # return(df)
# }
# 
# wt_list_308 <- list(wt_df_308_mediandepth_roll3, wt_df_308_mediandepth_change_roll3, wt_df_308_maxdepth_change_roll3)
# 
# var_list_308 <- list("med_r3_", "med_c_r3_", "max_c_r3_")
# 
# 
# # cluster_df_308 <- make_clustering_df(wt_df_list = wt_list_308, var_list = var_list_308)
# 
# cluster_df_308 <- lapply(x = wt_list_308, FUN = make_clustering_df)
# 
# # test (works)
# test_function <- function(df){
#   df %>% dplyr::select(significance) %>% head()
# }
# 
# wt_list_308_new <- lapply(x = wt_list_308, test_function)
# wt_list_308_new %>% View()
