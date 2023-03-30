# script to plot the results of the pca and kmeans clustering for the depth data
# 0. workspace ####
library(dplyr)
library(lubridate)
library(car)
library(vegan)
library(ggpubr)
library(FactoMineR)
library(missMDA)
library(factoextra)
library(ggplot2)
# library(MASS)
library(plotly)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/02_results/dst_pca_kmeans/")
# source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_dst_pca_clustering_results.R"))
source(paste0(dir_path, "/01_code/04_analyses/dst_summarystatistics/dst_pca_clustering.R"))

long_dst_daynight <- long_dst_daynight %>%
  mutate(month = date %>% lubridate::month(),
       month = sprintf("%02d", month %>% as.numeric()),
       monthyear = paste0(year, "-", month))

## tag 308 ####

### pca ####

tag_308_pca <- masterias_depth_summary_308_trans_scaled %>% missMDA::imputePCA(scale = F) %>% FactoMineR::PCA(scale.unit = F)
# ggsave(filename = paste0(plot_path, "f_308_pca.pdf"), height = 12, width = 16, units = "cm")

### k means ####

plot_f_308_dailydepthmedian_cluster <- ggplot(data = tag_308_depthsum) + 
  geom_point(aes(y = -depth_median, x = date, colour = cluster), size = 3) +
  theme_minimal() +
  labs(title = "tag 308 (female)", x = "date", y = "median depth in m", colour = "k means cluster")
# plot_f_308_dailydepthmedian_cluster %>% ggplotly()
# ggsave(filename = paste0(plot_path, "f_308_dailydepthmedian_cluster.pdf"), plot = plot_f_308_dailydepthmedian_cluster, height = 12, width = 16, units = "cm")


plot_f_308_monthlymeandepthrange_cluster <- ggplot(data = tag_308_depthsum_month, aes(x = monthyear, y = depth_range_mean, colour = cluster)) +
  # geom_point(size = 3) +
  geom_bar(stat = "identity", aes(fill = cluster)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "tag 308 (female)", x = "month", y = "mean depth range in m")
# plot_f_308_monthlymeandepthrange_cluster
# ggsave(filename = paste0(plot_path, "f_308_monthlymeandepthrange_cluster.pdf"), plot = plot_f_308_monthlymeandepthrange_cluster, height = 12, width = 16, units = "cm")


# looks ugly
# plot_f_308_dailydepthrange_violin_cluster <- ggplot(data = tag_308_depthsum, aes(x = monthyear, y = -depth_range, fill = cluster)) +
#   geom_violin(colour = "transparent", weight = 0) +
#   geom_boxplot(width = .1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) +
#   labs(title = "tag 308 (female)", y = "daily depth range in m", x = "month", fill = "k means cluster")
# plot_f_308_dailydepthrange_violin_cluster #%>% ggplotly()
# ggsave(filename = paste0(plot_path, "f_308_dailydepthrange_violin_cluster.pdf"), plot = plot_f_308_dailydepthrange_violin_cluster, height = 12, width = 20, units = "cm")


## tag 321 ####

### pca ####

tag_321_pca <- masterias_depth_summary_321_trans_scaled %>% missMDA::imputePCA(scale = F) %>% FactoMineR::PCA(scale.unit = F)
# ggsave(filename = paste0(plot_path, "m_321_pca.pdf"), height = 12, width = 16, units = "cm")


### k means ####

plot_m_321_dailydepthmedian_cluster <- ggplot(data = tag_321_depthsum) + 
  geom_point(aes(y = -depth_median, x = date, colour = cluster), size = 3) +
  theme_minimal() +
  labs(title = "tag 321 (male)", x = "date", y = "median depth in m", colour = "k means cluster")
# plot_m_321_dailydepthmedian_cluster
# ggsave(filename = paste0(plot_path, "m_321_dailydepthmedian_cluster.pdf"), plot = plot_m_321_dailydepthmedian_cluster, height = 12, width = 16, units = "cm")


plot_m_321_monthlymeandepthrange_cluster <- ggplot(data = tag_321_depthsum_month, aes(x = monthyear, y = -depth_range_mean, colour = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  labs(title = "tag 321 (male)", x = "month", y = "mean depth range in m")
# plot_m_321_monthlymeandepthrange_cluster
# ggsave(filename = paste0(plot_path, "m_321_monthlymeandepthrange_cluster.pdf"), plot = plot_m_321_monthlymeandepthrange_cluster, height = 12, width = 16, units = "cm")

# uglyyyy! 
# plot_m_321_dailydepthrange_violin_cluster <- ggplot(data = tag_321_depthsum, aes(x = monthyear, y = -depth_range, fill = cluster)) +
#   geom_violin(colour = "transparent", weight = 0) +
#   geom_boxplot(width = .1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) +
#   labs(title = "tag 321 (male)", y = "daily depth range in m", x = "month", fill = "k means cluster")
# # plot_m_321_dailydepthrange_violin_cluster %>% ggplotly()
# ggsave(filename = paste0(plot_path, "m_321_dailydepthrange_violin_cluster.pdf"), plot = plot_m_321_kmeans_cluster_depthrange, height = 12, width = 20, units = "cm")

### vertical speed ####
# ToDo: plot with cluster 1 and 2 on x axis and max vertical speed per day/night as boxplots or violins.

# KANN MICH GERAD NICHT REINDENKEN-SPAETER

# ggplot(data = long_dst_daynight %>% filter(!day == "full"), aes(x = monthyear, y = vertical_speed_max, fill = day)) +
#   # geom_violin() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) +
#   geom_boxplot()
# 
# ggplot(data = long_dst_daynight %>% filter(!day == "full"), aes(x = cluster, y = vertical_speed_max, fill = day)) +
#   # geom_violin() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) +
#   geom_boxplot()
