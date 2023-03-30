# Script executing the analysis of acoustic detections using the `actel` R package
# Author: Lotte Pohl

# 1. LIBRARIES ####

library(remotes)
# remotes::install_github("hugomflavio/actel", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
library(actel)
# library(c('gWidgets2', 'gWidgets2RGtk2', 'RGtk2'))
# library(RGtk2)

# 2. PREPARE WORKSPACE ####
rm(list = ls())
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/02_scripts/01_save_data/save_actel_files.R"))
source(paste0(dir_path, "/functions.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_actel_files.R"))

# dot <- 
#   "offshore -- coast -- WS1 -- WS2
# WS1 -- offshore"

# setwd(paste0(dir_path, "/03_data/actel_files"))

# actel::explore(tz = "Europe/Brussels", GUI = "never")

# 3. EXPLORE() ####

x <- actel::preload(biometrics = Biometrics, spatial = Spatial, deployments = Deployments, 
             detections = Detections, dot = dot, tz = "Europe/Brussels")
# You can then use the resulting object to run an actel analysis:
  
results_explore <- explore(datapack = x, report = TRUE)
results_migration <- migration(datapack = x, report = TRUE)
results_residency <- residency(datapack = x, report = TRUE)

save_data(data = results_residency, folder = paste0(dir_path, "/04_analysis_results/actel/residency/"))
  
# load("actel_explore_results.RData")
