# Script to try out sgolay filtering of the daily summary statistics 

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(plotly)
library(dplyr)
library(signal)
library(vegan) # for the transformation
library(ggpubr) # for the qq plots
library(missMDA) # for the PCA
library(FactoMineR) # for the PCA

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()

data <- long_dst_date %>% dplyr::filter(tag_serial_number == "1293321") %>%
  dplyr::select(date, depth_median) %>% 
  mutate(depth_median_change = depth_median - dplyr::lead(depth_median),
         depth_median_p3 = depth_median %>% signal::sgolayfilt(n = 11),
         depth_median_p3_change = depth_median_p3 - dplyr::lead(depth_median_p3))

p <- ggplot(data = data, aes(x = date)) +
  geom_line(aes(y = -depth_median), colour = "black") +
  geom_line(aes(y = -depth_median_p3), colour = "blue") +
  geom_line(aes(y = -depth_median_change), colour = "grey") +
  geom_line(aes(y = -depth_median_p3_change), colour = "red") +
  theme_minimal()

p %>% ggplotly()
