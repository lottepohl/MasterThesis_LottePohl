# Script to make statistical tests for hypotheses

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(gridExtra)
library(pracma)
library(oce)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_models.R") %>% base::source()

## set path were all figures are saved ####
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")


# 1. Females show higher residency in the area of the PBARN receivers than males. ####

# exploration

plot_exploration <- ggplot(data = tagged_animal_info %>% group_by(tag_serial_number), aes(x = sex, y = residency_index)) +
  geom_point()

plot_exploration %>% ggplotly()

sample1 <- tagged_animal_info %>% dplyr::filter(sex == "m", days_detected > 1) %>% dplyr::select(residency_index) %>% drop_na() %>% dplyr::pull() 
sample2 <- tagged_animal_info %>% dplyr::filter(sex == "f", days_detected > 1) %>% dplyr::select(residency_index) %>% drop_na() %>% dplyr::pull()

median(sample1, na.rm = T)
median(sample2, na.rm = T)

# check for normality of the data 

# if p > 0.05: samples are normally distributed
shapiro.test(sample1)
shapiro.test(sample2)
# not both samples are normally distributed, thus: wilcoxon test

# if p < 0.05: median of samples is different
hypothesis1_result <- wilcox.test(sample1, sample2)
hypothesis1_result$p.value


save_data(data = hypothesis1_result, folder = models_path)
