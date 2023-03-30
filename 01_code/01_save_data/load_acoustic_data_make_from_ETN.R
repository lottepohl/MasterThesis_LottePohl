# Script to read in acoustic data (from the etn database)

library(readr)
library(tidyverse)

source(paste0(dir_path, "/functions.R"))

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path <- paste0(dir_path, "/03_data/acoustic_detections/")

masterias_info <- readr::read_csv(paste0(dir_path, "/RData/masterias_info.csv"), show_col_types = FALSE)
masterias_info <- masterias_info %>% remove_double_cols()
# masterias_ind_per_station <- read_csv(paste0(dir_path, "/RData/masterias_ind_per_station.csv"), show_col_types = FALSE)
masterias_detections <- readr::read_csv(paste0(dir_path, "/RData/masterias_detections.csv"), show_col_types = FALSE)
# masterias_deployments <- read_csv(paste0(dir_path, "/RData/masterias_deployments.csv"), show_col_types = FALSE)
# detections_per_station <- read_csv(paste0(dir_path, "/RData/detections_per_station.csv"), show_col_types = FALSE)
# detections_per_deployment <- read_csv(paste0(dir_path, "/RData/detections_per_deployment.csv"), show_col_types = FALSE)
# close_stations <- read_csv(paste0(dir_path, "/RData/close_stations.csv"), show_col_types = FALSE)
deployments <- readr::read_csv(paste0(dir_path, "/RData/deployments.csv"), show_col_types = FALSE)
tag_info <- load_data("tag_info", data_path)