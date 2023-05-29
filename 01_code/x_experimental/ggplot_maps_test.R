# Script testing ggplot maps for the thesis manuscript

# WORKSPACE ####
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(marmap)

library("rnaturalearth")
library("rnaturalearthdata")

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_envdata <- paste0(dir_path, "/00_data/environmental_layers/")
path_boundaries <- paste0(dir_path, "/00_data/marine_boundaries/")
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% source()
source(paste0(dir_path, "/01_code/02_load_data/load_environmental_data.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_human_activities.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_bathy.R"))

# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# class(world)

ggplot() +
  geom_sf(data = Europe) +
  # geom_sf(data = Belgium) +
  # geom_sf(data = France) +
  # geom_sf(data = Netherlands) +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  geom_sf(data = English_channel, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  geom_sf(data = Dutch_EEZ, colour = "gray40",linewidth = 0.5, fill = "transparent") +
  geom_sf(data = French_EEZ, colour = "gray40",linewidth = 0.5, fill = "transparent") +
  geom_sf(data = UK_EEZ, colour = "gray40",linewidth = 0.5, fill = "transparent") +
  geom_sf(data = Schelde_boundaries, fill = "transparent", colour = "darkgreen",linewidth = 0.75) +
  geom_sf(data = BPNS, colour = "darkorange",linewidth = 0.75, fill = "transparent") +
  theme(panel.background = element_rect(fill = "lightblue")) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(47, 54), xlim = c(-6, 6)) 
  # theme_bw()
