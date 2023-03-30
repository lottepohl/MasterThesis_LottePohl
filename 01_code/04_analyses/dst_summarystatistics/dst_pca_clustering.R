# Script to perform PCA and clustering on the summary statistics of the dst depth logs

# 0. workspace ####
library(dplyr)
library(lubridate)
library(car)
library(vegan)
library(ggpubr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(tidyverse)
library(ggplot2)
# library(MASS)
library(plotly)
# library(pracma)
# library(psdr)
# library(ggplot2)
# library(StreamMetabolism)
# library(suncalc)

# rm(list = ls())


dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path <- paste0(dir_path, "/02_results/dst_pca_kmeans/")
# source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R"))

# 1. prepare datasets ####

# manual separation of overwintering, migrating and oversummering periods of longterm dst tracks
# 
# dst_longterm_periods <- tibble(tag_serial_number = c(rep("1293308", times = 6), rep("1293321", times = 8)),
#                                start_date = c("2018-08-02", "2018-08-22", "2018-10-10", "2019-03-23", "2019-05-13", "2019-08-04", 
#                                               "2018-07-18", "2018-08-21", "2018-10-23", "2019-04-22", "2019-05-11", "2019-08-31", "2019-09-26", "2019-11-17") %>% 
#                                  as.POSIXct(tz = "UTC"),
#                                activity = c("summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "recording_end",
#                                             "summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "winter_migration", "winter_residency", "recording_end")) %>%
#   mutate(year = start_date %>% lubridate::year(),
#          end_date = start_date %>% lead() %>% replace_na("2019-11-16" %>% as.POSIXct(tz = "UTC")),
#          start_date = start_date - lubridate::days(1))
# 
# 
# ## add period and full day summary #### 
# # dataframe with daily summary statistics and activity type
# 
# # TOdo: add depth range difference between days
# 
# masterias_depth_daynight_activity <- masterias_depth_daynight %>% 
#   filter(tag_serial_number %in% c("1293321", "1293308"), dusk == 0 & dawn == 0) %>% #filter out dusk and dawn: not used right now
#   left_join(dst_longterm_periods,
#             by = join_by(tag_serial_number == tag_serial_number,
#                          between(x = date, y_lower = start_date, y_upper = end_date))) %>%
#   dplyr::select(!c(start_date, end_date))
# 
# masterias_depth_daynight_activity <- masterias_depth_daynight_activity %>%
#   full_join(masterias_depth_daynight_activity %>% 
#               group_by(tag_serial_number, date) %>% 
#               reframe(depth_mean = sum(depth_mean)/2,
#                       depth_sd = sum(depth_sd)/2,
#                       depth_median = sum(depth_median)/2,
#                       depth_min = min(depth_min),
#                       depth_max = max(depth_max),
#                       depth_range = depth_max - depth_min,
#                       vertical_speed_mean = sum(vertical_speed_mean)/2,
#                       vertical_speed_sd = sum(vertical_speed_sd)/2,
#                       vertical_speed_median = sum(vertical_speed_median)/2,
#                       vertical_speed_min = min(vertical_speed_min),
#                       vertical_speed_max = max(vertical_speed_max),
#                       vertical_speed_range = vertical_speed_max - vertical_speed_min,
#                       activity = unique(activity),
#                       year = unique(year),
#                       day = "full",
#                       dusk = NA,
#                       dawn = NA))

masterias_depth_summary_308 <- long_dst_date %>% ungroup()%>% 
  filter(tag_serial_number == "1293308")

masterias_depth_summary_321 <- long_dst_date %>% ungroup()%>% 
  filter(tag_serial_number == "1293321") 
  
  
  # masterias_depth_daynight_activity %>% filter(tag_serial_number == "1293321") %>%
  # ungroup() %>% 
  # filter(day == "full") %>%
  # dplyr::select(where(is.numeric)) %>% 
  # dplyr::select(!c(year, vertical_speed_min)) 
  # 


# # dataframe with vertical movement behaviour and activity type
# 
# masterias_DVM_sum_day_activity <- masterias_DVM_sum_day %>% 
#   filter(tag_serial_number %in% c("1293308", "1293321")) %>% 
#   left_join(dst_longterm_periods,
#             by = join_by(tag_serial_number == tag_serial_number,
#                          between(x = date_24hcycle, y_lower = start_date, y_upper = end_date))) %>%
#   dplyr::select(!c(start_date, end_date))

## transform data ####

### 308 ####
# ggpubr::ggqqplot(masterias_depth_summary_308$depth_range)
# test <- masterias_depth_summary_308 %>% sqrt()
# ggpubr::ggqqplot(test$depth_range)
# test <- masterias_depth_summary_308 %>% log10()
# ggpubr::ggqqplot(test$depth_range)
# test <- masterias_depth_summary_308 %>% vegan::decostand(method = "alr")
# ggpubr::ggqqplot(test$depth_range)
# test <- masterias_depth_summary_308 %>% vegan::decostand(method = "hellinger")
# ggpubr::ggqqplot(test$depth_range)
# test <- 1 / masterias_depth_summary_308$depth_range
# ggpubr::ggqqplot(test)


#log10 ist die beste transformation
masterias_depth_summary_308_transformed <- masterias_depth_summary_308 %>% 
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!c(year, vertical_speed_min)) %>%
  mutate(depth_range = depth_range + 0.01) %>%
  log10() %>%
  mutate_all(~ifelse(is.na(.) | is.infinite(.), 0.001, .)) %>%
  select(-matches("roll"))
  # select_if(~!any(is.na(.)) & !any(is.infinite(.)))
ggpubr::ggqqplot(masterias_depth_summary_308_transformed$depth_range)

# #create histogram for original distribution
# hist(masterias_depth_summary_308$depth_range, col='steelblue', main='Original')

# #create histogram for log-transformed distribution 
# hist(masterias_depth_summary_308_transformed$depth_range, col='coral2', main='log10 Transformed')
# shapiro.test(masterias_depth_summary_308_transformed$depth_min)$p.value

#### scale  ####
masterias_depth_summary_308_trans_scaled <- masterias_depth_summary_308_transformed %>% 
  base::scale() %>% 
  as_tibble() %>%
  dplyr::select(!vertical_speed_median)
# hist(masterias_depth_summary_308_trans_scaled$depth_range, col='blue', main='sqrt Transformed + scaled')
# ggpubr::ggqqplot(masterias_depth_summary_308_trans_scaled$depth_range)

### 321 ####
# test <- masterias_depth_summary_321 %>% log10()
# ggpubr::ggqqplot(test$depth_range)
# shapiro.test(test$depth_min)$p.value
# test <- masterias_depth_summary_321 %>% vegan::decostand(method = "alr")
# ggpubr::ggqqplot(test$depth_range)
# test <- masterias_depth_summary_321 %>% vegan::decostand(method = "hellinger")
# ggpubr::ggqqplot(test$depth_range)
# test <- 1 / masterias_depth_summary_321$depth_range
# ggpubr::ggqqplot(test)
# # Apply the Box-Cox transformation
# lambda <- car::powerTransform(masterias_depth_summary_321$depth_range)$lambda
# depth_transformed <- ifelse(lambda == 0, log(depth), (depth^lambda - 1)/lambda)
# ggqqplot(depth_transformed)
# test <- (masterias_depth_summary_321$depth_min - mean(masterias_depth_summary_321$depth_min)) / sd(masterias_depth_summary_321$depth_min)
# ggpubr::ggqqplot(test)

masterias_depth_summary_321_transformed <- masterias_depth_summary_321 %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(!c(year, vertical_speed_min)) %>%
  mutate(depth_range = depth_range + 0.01) %>% 
  sqrt() %>%
  mutate_all(~ifelse(is.na(.) | is.infinite(.), 0.001, .)) %>%
  select(-matches("roll"))
ggpubr::ggqqplot(masterias_depth_summary_321_transformed$depth_range)
# srqt ist die beste

# #create histogram for original distribution
# hist(masterias_depth_summary_321$depth_range, col='steelblue', main='Original')

# #create histogram for log-transformed distribution 
# hist(masterias_depth_summary_321_transformed$depth_range, col='coral2', main='sqrt Transformed')
# shapiro.test(masterias_depth_summary_321_transformed$depth_min)$p.value

#### scale  ####
masterias_depth_summary_321_trans_scaled <- masterias_depth_summary_321_transformed %>% 
  base::scale() %>% 
  as_tibble() %>%
  na.omit()
# hist(masterias_depth_summary_321_trans_scaled$depth_range, col='blue', main='sqrt Transformed + scaled')


## PCA ####

tag_308_pca <- masterias_depth_summary_308_trans_scaled %>% missMDA::imputePCA(scale = F) %>% FactoMineR::PCA(scale.unit = F)

tag_321_pca <- masterias_depth_summary_321_trans_scaled %>% missMDA::imputePCA(scale = F) %>% FactoMineR::PCA(scale.unit = F)

# tag_321_pca %>% plot()
plot(tag_321_pca, autoLab="no", choix="var")
plot(tag_321_pca, autoLab="no", choix="ind")

tag_321_pca_var <- factoextra::get_pca(tag_321_pca, element = "var")

## k means clustering ####
# todo: determine optimal numbers of clusters!!!

### 308 ####

tag_308_kmeans_data <- tag_308_pca$ind$coord %>% as.data.frame() %>% dplyr::select(Dim.1, Dim.2)

tag_308_kmeans <- tag_308_kmeans_data %>% stats::kmeans(centers = 3,
                                                        iter.max = 100,
                                                        algorithm = "Hartigan-Wong")

tag_308_depthsum <- masterias_depth_summary_308 %>%
  mutate(cluster = tag_308_kmeans$cluster %>% as.factor,
         month = date %>% lubridate::month(),
         year = date %>% lubridate::year(),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month))



### merge df with monthly mean and assign predominant cluster to the month

tag_308_depthsum_month <- tag_308_depthsum %>% 
  mutate(month = date %>% lubridate::month(),
         year = date %>% lubridate::year()) %>%
  group_by(year, month, cluster) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, month) %>%
  slice(which.max(count)) %>%
  dplyr::select(!count) %>%
  mutate(tag_serial_number = "1293308") %>% 
  mutate(month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month)) %>%
  left_join(masterias_depth_month, by = join_by(tag_serial_number, year, month, monthyear),
            multiple = "all")
# tag_308_depthsum_month %>% View()


# join dfs to get predominant cluster per month

tag_308_depthsum <- tag_308_depthsum %>% #dplyr::select(!cluster) %>% 
  left_join(tag_308_depthsum_month %>% 
              rename(cluster_month = cluster) %>%
              dplyr::select(year, month, cluster_month),
            by = join_by(year, month), multiple = "all")

### 321 ####

tag_321_kmeans_data <- tag_321_pca$ind$coord %>% as.data.frame() %>% dplyr::select(Dim.1, Dim.2)

tag_321_kmeans <- tag_321_kmeans_data %>% stats::kmeans(centers = 3,
                                                        iter.max = 100,
                                                        algorithm = "Hartigan-Wong")

tag_321_depthsum <- masterias_depth_summary_321 %>%
  mutate(cluster = tag_321_kmeans$cluster %>% as.factor,
         month = date %>% lubridate::month(),
         year = date %>% lubridate::year(),
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month))



### merge df with monthly mean and assign predominant cluster to the month

tag_321_depthsum_month <- tag_321_depthsum %>% 
  mutate(month = date %>% lubridate::month(),
         year = date %>% lubridate::year()) %>%
  group_by(year, month, cluster) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(year, month) %>%
  slice(which.max(count)) %>%
  dplyr::select(!count) %>%
  mutate(tag_serial_number = "1293321",
         month = sprintf("%02d", month %>% as.numeric()),
         monthyear = paste0(year, "-", month)) %>% 
  left_join(masterias_depth_month, by = join_by(tag_serial_number, year, month, monthyear),
            multiple = "all")
# tag_321_depthsum_month %>% View()

# plot clusters for tag 321

# join dfs to get predominant cluster per month

tag_321_depthsum <- tag_321_depthsum %>% #dplyr::select(!cluster) %>% 
  left_join(tag_321_depthsum_month %>% 
              rename(cluster_month = cluster) %>%
              dplyr::select(year, month, cluster_month),
            by = join_by(year, month), multiple = "all")

# tag_321_depthsum %>% View()


# save data ####

save_data(data = tag_308_pca, folder = data_path)
save_data(data = masterias_depth_summary_308_trans_scaled, folder = data_path)
save_data(data = masterias_depth_summary_321_trans_scaled, folder = data_path)
save_data(data = tag_321_pca, folder = data_path)
save_data(data = tag_308_kmeans, folder = data_path)
save_data(data = tag_308_depthsum, folder = data_path)
save_data(data = tag_308_depthsum_month, folder = data_path)
save_data(data = tag_321_kmeans, folder = data_path)
save_data(data = tag_321_depthsum, folder = data_path)
save_data(data = tag_321_depthsum_month, folder = data_path)

