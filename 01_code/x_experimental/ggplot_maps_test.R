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
  geom_sf(data = Europe, colour = "gray85") +
  # geom_sf(data = Belgium) +
  # geom_sf(data = France) +
  # geom_sf(data = Netherlands) +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.5, fill = "transparent") +
  geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.5, fill = "transparent") +
  geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.5, fill = "transparent") +
  geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt"),linewidth = 0.75) +
  geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  theme(panel.background = element_rect(fill = "lightblue")) +
  scale_colour_manual(name = "", values = c("EEZ" = "gray40", 
                                            "marine_boundaries" = "gray0", 
                                            "Scheldt" = "darkgreen", 
                                            "BPNS" = "darkorange"
                                            )) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(47, 54), xlim = c(-6, 6)) 
  # theme_bw()

  # plot <- ggplot(data = data) +
  #   geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "daily depth range"), alpha = 0.75) +
  #   geom_line(aes(x = date, y = -depth_median_sgolay, colour = "daily median depth"), linewidth = 0.5) +
  #   theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  #   scale_y_continuous(expand = c(0, 0)) + #, sec.axis = sec_axis(trans = ~ (((. * (100/max(data$depth_max_sgolay))) + 100) * (.01)), name = "Illuminated Moon Fraction", labels = scales::label_percent())) +
  #   labs(x = "", y = "Depth in m") +
  #   scale_colour_manual(name = "", values = c("daily median depth" = "black", "daily depth range" = "lightgrey", "illuminated moon fraction" = "red")) +
  #   scale_fill_manual(name = "", values = c("daily depth range" = "lightgrey")) +
  #   theme(legend.position = "none", legend.box = "horizontal",   legend.margin = margin(t = -15)) + #"bottom"
  #   scale_x_datetime(date_breaks = "1 month", date_labels = "%b '%y", expand = c(0, 0), limits = c(min_date, max_date)) +
  #   theme(axis.text.x = element_text(angle = 15, hjust = 0.5))
  # 