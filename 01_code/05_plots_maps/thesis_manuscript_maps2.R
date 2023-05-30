# Script testing ggplot maps for the thesis manuscript

# WORKSPACE ####
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(marmap)
library(ggrepel)
# 
# library("rnaturalearth")
# library("rnaturalearthdata")

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
paste0(dir_path, "/01_code/02_load_data/load_dst_geolocation_output.R") %>% base::source()

# Study Area focus point 2 ####

map_overview_ggplot <- ggplot() +
  # Stuff for bathy legend
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "50 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "75 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1,linewidth = 0.75) +
  # now the map layers
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.25) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-50),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.5) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-75),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.75) +
  geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  geom_sf(data = Belgium, colour = "gray60") +
  geom_sf(data = France, colour = "gray60") +
  geom_sf(data = Netherlands, colour = "gray60") +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  
  # marine boundaries
  geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
  # theme(panel.background = element_rect(fill = "lightblue")) +
  labs(x = "Longitude", y = "Latitude") +
  scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
                                                             # "marine_boundaries" = "gray60", 
                                                             "Scheldt Estuary" = "darkgreen", 
                                                             "BPNS" = "darkorange")) +
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "20 m" = "#CDCDE9",
                                                    "50 m" = "#8585C7",
                                                    "75 m" = "#38389F")) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(47, 54), xlim = c(-7, 7)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                                    style = north_arrow_fancy_orienteering) +
  geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
            stat = "sf_coordinates", size = 2, nudge_y = 0.25, nudge_x = 0, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
            stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -0.85, family = "serif", fontface = "bold", fill = "white") +
  geom_text_repel(data = Hurd_deep, aes(label = preferredGazetteerName, geometry = geometry),
            stat = "sf_coordinates", size = 2, nudge_y = -0.35, nudge_x = -2, family = "serif") +
  geom_text_repel(data = Cap_de_la_Hague, aes(label = preferredGazetteerName, geometry = geometry),
            stat = "sf_coordinates", size = 2, nudge_y = -0.85, nudge_x = 2, family = "serif") +
  geom_text_repel(data = Norwich, aes(label = preferredGazetteerName, geometry = geometry),
            stat = "sf_coordinates", size = 2, nudge_y = 0.35, nudge_x = 1.5, family = "serif")

map_overview_ggplot
# geolocation model output ####

map_dst_321 <- ggplot() +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "35 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "50 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1, linewidth = 0.75) +
  # now the map layers
  geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  labs(x = "Longitude", y = "Latitude") +
  # scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
  #                                                            # "marine_boundaries" = "gray60", 
  #                                                            "Scheldt Estuary" = "darkgreen", 
  #                                                            "BPNS" = "darkorange")) +
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "20 m" = "#CDCDE9",
                                                    "35 m" = "#8585C7",
                                                    "50 m" = "#38389F")) +
  geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293321"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.85) +
  # geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293308"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.75) +
  geom_text_repel(data = Norwich, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -0.25, family = "serif") +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  # scale_colour_viridis_c() +
  scale_colour_datetime(name = "Date", low = "darkorange", high = "darkblue") + #colours = c("darkviolet", "darkorange", "darkblue")
  geom_contour(data = bathy_northsea, 
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-35),
               linewidth=c(0.35),
               colour="darkblue",
               # linetype = "11",
               alpha = 0.5)+
  geom_contour(data = bathy_northsea, 
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               colour="darkblue",
               # linetype = "11",
               alpha = 0.25) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-50),
               linewidth=c(0.35),
               # linetype = "11",
               colour="darkblue",
               alpha = 0.85) +
  geom_point(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293321"), 
             x = 3.631849, y = 51.61220, mapping = aes(shape = "tag 321 (m)" ), colour = "black", size = 3) +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -0.9, nudge_x = -0.45, family = "serif", fontface = "bold", fill = "white", alpha = 1) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(50.5, 53), xlim = c(-1, 4.5)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25, bar_cols = c("gray0", "white"), text_family = "serif",
                              pad_x = unit(0.25, "cm"),
                              pad_y = unit(0.25, "cm"),) +
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                    height =  unit(0.75, "cm"), width = unit(0.75, "cm"),
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                    style = north_arrow_fancy_orienteering) +
  scale_shape_manual(name = "Tagging Location", values = c("tag 321 (m)" = 3)) #+
  # theme(
  #   legend.box.spacing = unit(0.5, "lines")
  # ) +
  # Customizing the position of the shape and fill legends
  # guides(
  #   shape = guide_legend(title = "Tagging Location", direction = "horizontal", override.aes = list(fill = NULL), label.hjust = 1, title.hjust = 1),
  #   fill = guide_legend(title = "Bathymetry", direction = "horizontal", ncol = 3)
  # )
  # guides(shape=guide_legend(direction='horizontal', position = 'bottom'),
  #        fill=guide_legend(direction='horizontal', position = 'bottom'))  +
  # +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal", legend.margin=margin()) + #, legend.margin = margin(t = -15))
  # guides(colour = guide_legend(order = 1), 
  #        shape = guide_legend(order = 3), 
  #        fill = guide_legend(order = 2))
  # guides(
  #   # fill = guide_legend(title = "Bathymetry", direction = "horizontal", position = "bottom"),
  #   # shape = guide_legend(title = "Tagging Location", direction = "horizontal", position = "bottom"),
  #   color = guide_legend(title = "Date", direction = "vertical", position = "right")
  # )

map_dst_321

map_dst_308 <- ggplot() +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "50 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "75 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1,linewidth = 0.75) +
  # now the map layers
  geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  labs(x = "Longitude", y = "Latitude") +
  # scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
  #                                                            # "marine_boundaries" = "gray60", 
  #                                                            "Scheldt Estuary" = "darkgreen", 
  #                                                            "BPNS" = "darkorange")) +
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "20 m" = "#CDCDE9",
                                                    "50 m" = "#8585C7",
                                                    "75 m" = "#38389F")) +
  # geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293321"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.75) +
  geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293308"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.85) +
  geom_point(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293308"), 
             x = 3.40112411, y = 51.42878, mapping = aes(shape = "tag 308 (f)" ), colour = "black", size = 3) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  # scale_colour_viridis_c() +
  scale_colour_datetime(name = "Date", low = "darkorange", high = "darkblue") + #colours = c("darkviolet", "darkorange", "darkblue")
  geom_contour(data = bathy_northsea, 
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-50),
               linewidth=c(0.7),
               colour="darkblue",
               # linetype = "22",
               alpha = 0.35)+
  geom_contour(data = bathy_northsea, 
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               colour="darkblue",
               # linetype = "31",
               alpha = 0.25) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-75),
               linewidth=c(0.35),
               colour="darkblue",
               alpha = 0.85) +
  geom_text_repel(data = Hurd_deep, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.45, nudge_x = 0.45, family = "serif") +
  geom_text_repel(data = Cap_de_la_Hague, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = -0.55, nudge_x = 1.3, family = "serif") +
  geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = 0.2, nudge_x = 2.8, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -1.5, nudge_x = -0.75, family = "serif", fontface = "bold", fill = "white", alpha = 1) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(49, 52.5), xlim = c(-3.5, 4.5)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                    height =  unit(0.75, "cm"), width = unit(0.75, "cm"),
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                                    style = north_arrow_fancy_orienteering) +
  scale_shape_manual(name = "Tagging Location", values = c("tag 308 (f)" = 4))
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal", legend.margin = margin(t = -15))

map_dst_308

## map together ####

map_geolocation_ggplot <- ggplot() +
  # Stuff for bathy legend
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "50 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "75 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1,linewidth = 0.75) +
  # now the map layers
  geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  geom_sf(data = Belgium, colour = "gray60") +
  geom_sf(data = France, colour = "gray60") +
  geom_sf(data = Netherlands, colour = "gray60") +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  
  # marine boundaries
  # geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
  # geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  # theme(panel.background = element_rect(fill = "lightblue")) +
  labs(x = "Longitude", y = "Latitude") +
  # scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
  #                                                            # "marine_boundaries" = "gray60", 
  #                                                            "Scheldt Estuary" = "darkgreen", 
  #                                                            "BPNS" = "darkorange")) +
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "20 m" = "#CDCDE9",
                                                    "50 m" = "#8585C7",
                                                    "75 m" = "#38389F")) +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               linetype = "22",
               colour="darkblue",
               alpha = 0.25) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-50),
               linewidth=c(0.35),
               linetype = "22",
               colour="darkblue",
               alpha = 0.5) +
  geom_contour(data = bathy_northsea,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-75),
               linewidth=c(0.35),
               linetype = "22",
               colour="darkblue",
               alpha = 0.75) +
  geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293321"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.85) +
  geom_path(data = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == "1293308"), mapping = aes(x = detection_longitude, y = detection_latitude, colour = date_time), linewidth = 0.85) +
  geom_point(x = 3.40112411, y = 51.42878, mapping = aes(shape = "tag 308 (f)" ), colour = "black", size = 3) +
  geom_point(x = 3.631849, y = 51.61220, mapping = aes(shape = "tag 321 (m)" ), colour = "black", size = 3) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(48.5, 53.5), xlim = c(-7, 7)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
                                    height =  unit(1, "cm"), width = unit(1, "cm"),
                                    style = north_arrow_fancy_orienteering) +
  geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = 0.35, nudge_x = -0.2, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -0.55, nudge_x = -0.85, family = "serif", fontface = "bold", fill = "white") +
  geom_text_repel(data = Hurd_deep, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = -0.35, nudge_x = -2, family = "serif") +
  geom_text_repel(data = Cap_de_la_Hague, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = -0.85, nudge_x = 2, family = "serif") +
  geom_text_repel(data = Norwich, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = -0.25, nudge_x = -0.5, family = "serif") +
  scale_shape_manual(name = "Tagging Location", values = c("tag 308 (f)" = 3, "tag 321 (m)" = 4)) +
  scale_colour_datetime(name = "Date", low = "darkorange", high = "darkblue") #colours = c("darkviolet", "darkorange", "darkblue")

map_geolocation_ggplot

# detail Scheldt ####


map_detail_ggplot <- ggplot() +
  # Stuff for bathy legend
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "10 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, mapping = aes(fill = "30 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1,linewidth = 0.75) +
  # now the map layers
  # geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  geom_sf(data = Western_Scheldt_boundaries, fill = "darkgreen", alpha = 0.35,linewidth = 0.75) +
  geom_sf(data = Eastern_Scheldt_boundaries, fill = "lightgreen", alpha = 0.35,linewidth = 0.75) +
  geom_sf(data = Western_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Western Scheldt"),linewidth = 0.75) +
  geom_sf(data = Eastern_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Eastern Scheldt"),linewidth = 0.75) +
  geom_sf(data = Belgium, colour = "gray60") +
  geom_sf(data = France, colour = "gray60") +
  geom_sf(data = Netherlands, colour = "gray60") +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  
  # marine boundaries
  geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  # geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
  # theme(panel.background = element_rect(fill = "lightblue")) +
  labs(x = "Longitude", y = "Latitude") +
  scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
                                                             # "marine_boundaries" = "gray60", 
                                                             "Scheldt Estuary" = "darkgreen", 
                                                             "BPNS" = "darkorange",
                                                             "Eastern Scheldt" = "lightgreen",
                                                             "Western Scheldt" = "darkgreen")) +
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "Eastern Scheldt" = "transparent",
                                                    "Western Scheldt" = "transparent",
                                                    "10 m" = "#CDCDE9",
                                                    "20 m" = "#8585C7",
                                                    "30 m" = "#38389F")) +

  # guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  geom_contour(data = bathy_belgium_coarse,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-10),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.25) +
  geom_contour(data = bathy_belgium_coarse,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.5) +
  geom_contour(data = bathy_belgium_coarse,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-30),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.75) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(51, 52), xlim = c(2, 5)) +
  geom_point(data = receiver_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, shape = "Receiver Station"), size = 0.6, colour = "gray0") +
  ggspatial::annotation_scale(location = "tr", width_hint = 0.1, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    pad_x = unit(0.06, "in"), pad_y = unit(0.3, "in"),
                                    height =  unit(0.5, "cm"), width = unit(0.5, "cm"),
                                    style = north_arrow_fancy_orienteering) +
  # geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
  #            stat = "sf_coordinates", size = 2, nudge_y = 0.25, nudge_x = 0, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -6, family = "serif", fontface = "bold", fill = "white") +
  geom_text_repel(data = Neeltje_Jans, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.09, nudge_x = 0.25, family = "serif") +
  geom_text_repel(data = receiver_stations %>% dplyr::filter(station_name == "Birkenfels"), aes(label = station_name, x = deploy_longitude, y = deploy_latitude),
                  size = 2, nudge_y = 0.07, nudge_x = -0.35, family = "serif") +
  geom_text_repel(data = Bergen_op_Zoom, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.1, nudge_x = 0.23, family = "serif") +
  geom_text_repel(data = Vlissingen, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.05, nudge_x = 0.25, family = "serif") +
  scale_shape_manual(name = "PBARN", values = c("Receiver Station" = 19))

map_detail_ggplot

# detail WS ####

ws_stations <- receiver_stations %>% 
  dplyr::filter(deploy_latitude %>% between(51.3, 51.5),
                deploy_longitude %>% between(3.4, 4.05)) %>%
  mutate(area = ifelse(deploy_longitude < 3.6, "WS1", 
                       ifelse(deploy_longitude < 3.9, "WS2", "WS3")) %>%
           as.factor())

map_WS_ggplot <- ggplot() +
  # Stuff for bathy legend
  geom_sf(data = UK_EEZ, mapping = aes(fill = "10 m"), colour = "transparent", alpha = 1,linewidth = 0.75, show.legend = NA) +
  geom_sf(data = UK_EEZ, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75, show.legend = NA) +
  geom_sf(data = UK_EEZ, mapping = aes(fill = "30 m"), colour = "transparent", alpha = 1,linewidth = 0.75, show.legend = NA) +
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS1"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") + #
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS2"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") +
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS3"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") +
  geom_sf(data = UK_EEZ, fill = "white", colour = "white", alpha = 1,linewidth = 1) +
  # now the map layers
  # geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  # geom_sf(data = Western_Scheldt_boundaries, fill = "darkgreen", alpha = 0.35,linewidth = 0.75) +
  # geom_sf(data = Eastern_Scheldt_boundaries, fill = "lightgreen", alpha = 0.35,linewidth = 0.75) +
  # geom_sf(data = Western_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Western Scheldt"),linewidth = 0.75) +
  # geom_sf(data = Eastern_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Eastern Scheldt"),linewidth = 0.75) +
  geom_sf(data = Belgium, colour = "gray60") +
  geom_sf(data = France, colour = "gray60") +
  geom_sf(data = Netherlands, colour = "gray60") +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  
  # marine boundaries
  # geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  # geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
  # theme(panel.background = element_rect(fill = "lightblue")) +
  labs(x = "Longitude", y = "Latitude") +
  # scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
  #                                                            # "marine_boundaries" = "gray60", 
  #                                                            "Scheldt Estuary" = "darkgreen", 
  #                                                            "BPNS" = "darkorange",
  #                                                            "Eastern Scheldt" = "lightgreen",
  #                                                            "Western Scheldt" = "darkgreen")) +
  scale_color_manual(name = "Receiver Array", values = c("WS1" = "#ed7d31", "WS2" = "#3483ac", "WS3" = "#FF00BA")) + #, "#34b3bb" "black", 
  scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
                                                    "marine_boundaries" = "transparent", 
                                                    "Scheldt" = "transparent", 
                                                    "BPNS" = "transparent",
                                                    "Eastern Scheldt" = "transparent",
                                                    "Western Scheldt" = "transparent",
                                                    "10 m" = "#CDCDE9",
                                                    "20 m" = "#8585C7",
                                                    "30 m" = "#38389F")) +
  
  # guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  geom_contour(data = bathy_belgium,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-10),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.25) +
  geom_contour(data = bathy_belgium,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-20),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.5) +
  geom_contour(data = bathy_belgium,
               aes(x=longitude, y=latitude, z=depth_m),
               breaks=c(-30),
               linewidth=c(0.35),
               # linetype = "22",
               colour="darkblue",
               alpha = 0.75) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(51.2, 51.8), xlim = c(3.3, 4.4)) +
  geom_point(data = receiver_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, shape = "Receiver Station"), size = 1, colour = "gray0") +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS1"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#ed7d31",size = 1) +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS2"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#3483ac", size = 1) +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS3"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#FF00BA", size = 1) +
  # geom_point(data = ws_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, colour = area, shape = "Receiver Station"), size = 1) +
  # geom_point(data = ws_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, colour = area, shape = "Receiver Station"), size = 1) +
  ggspatial::annotation_scale(location = "tr", width_hint = 0.15, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    pad_x = unit(0.06, "in"), pad_y = unit(0.3, "in"),
                                    height =  unit(0.75, "cm"), width = unit(0.75, "cm"),
                                    style = north_arrow_fancy_orienteering) +
  # geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
  #            stat = "sf_coordinates", size = 2, nudge_y = 0.25, nudge_x = 0, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -6, family = "serif", fontface = "bold", fill = "white") +
  geom_text_repel(data = Neeltje_Jans, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.09, nudge_x = 0.25, family = "serif") +
  # geom_text_repel(data = receiver_stations %>% dplyr::filter(station_name == "Birkenfels"), aes(label = station_name, x = deploy_longitude, y = deploy_latitude),
  #                 size = 2, nudge_y = 0.07, nudge_x = -0.35, family = "serif") +
  # geom_text_repel(data = Bergen_op_Zoom, aes(label = preferredGazetteerName, geometry = geometry),
  #                 stat = "sf_coordinates", size = 2, nudge_y = 0.1, nudge_x = 0.23, family = "serif") +
  geom_text_repel(data = Vlissingen, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.05, nudge_x = 0.15, family = "serif") +
  scale_shape_manual(name = "PBARN", values = c("Receiver Station" = 19)) +
  guides(colour = guide_legend(override.aes = list(shape = 19, size = 4)),
         fill = guide_legend(override.aes = list(shape = 0, size = 6, alpha = 0.5))) #size = 6

map_WS_ggplot

# tests ####

ggplot() +
  # Stuff for bathy legend
  # geom_sf(data = Schelde_boundaries, mapping = aes(fill = "10 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  # geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  # geom_sf(data = Schelde_boundaries, mapping = aes(fill = "30 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS1"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") + #
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS2"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") +
  geom_sf(data = UK_EEZ, mapping = aes(colour = "WS3"), fill = "transparent", alpha = 1,linewidth = 0.75, show.legend = "point") +
  geom_sf(data = UK_EEZ, fill = "white", colour = "white", alpha = 1,linewidth = 1) +
  # now the map layers
  geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
  # geom_sf(data = Western_Scheldt_boundaries, fill = "darkgreen", alpha = 0.35,linewidth = 0.75) +
  # geom_sf(data = Eastern_Scheldt_boundaries, fill = "lightgreen", alpha = 0.35,linewidth = 0.75) +
  # geom_sf(data = Western_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Western Scheldt"),linewidth = 0.75) +
  # geom_sf(data = Eastern_Scheldt_boundaries, fill = "transparent", mapping = aes(colour = "Eastern Scheldt"),linewidth = 0.75) +
  # geom_sf(data = Belgium, colour = "gray60") +
  # geom_sf(data = France, colour = "gray60") +
  # geom_sf(data = Netherlands, colour = "gray60") +
  # geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
  # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
  # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
  
  # marine boundaries
  # geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
  # geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
  # theme(panel.background = element_rect(fill = "lightblue")) +
  labs(x = "Longitude", y = "Latitude") +
  # scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
  #                                                            # "marine_boundaries" = "gray60", 
  #                                                            "Scheldt Estuary" = "darkgreen", 
  #                                                            "BPNS" = "darkorange",
  #                                                            "Eastern Scheldt" = "lightgreen",
  #                                                            "Western Scheldt" = "darkgreen")) +
  scale_color_manual(name = "Receiver Array", values = c("WS1" = "#ed7d31", "WS2" = "#3483ac", "WS3" = "#FF00BA")) + #, "#34b3bb" "black", 
  # scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
  #                                                   "marine_boundaries" = "transparent", 
  #                                                   "Scheldt" = "transparent", 
  #                                                   "BPNS" = "transparent",
  #                                                   "Eastern Scheldt" = "transparent",
  #                                                   "Western Scheldt" = "transparent",
  #                                                   "10 m" = "#CDCDE9",
  #                                                   "20 m" = "#8585C7",
  #                                                   "30 m" = "#38389F")) +
  
  # guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
  # geom_contour(data = bathy_belgium,
  #              aes(x=longitude, y=latitude, z=depth_m),
  #              breaks=c(-10),
  #              linewidth=c(0.35),
  #              # linetype = "22",
  #              colour="darkblue",
  #              alpha = 0.25) +
  # geom_contour(data = bathy_belgium,
  #              aes(x=longitude, y=latitude, z=depth_m),
  #              breaks=c(-20),
  #              linewidth=c(0.35),
  #              # linetype = "22",
  #              colour="darkblue",
  #              alpha = 0.5) +
  # geom_contour(data = bathy_belgium,
  #              aes(x=longitude, y=latitude, z=depth_m),
  #              breaks=c(-30),
  #              linewidth=c(0.35),
  #              # linetype = "22",
  #              colour="darkblue",
  #              alpha = 0.75) +
  coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(51.2, 51.8), xlim = c(3.3, 4.4)) +
  # geom_point(data = receiver_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, shape = "Receiver Station"), size = 1, colour = "gray0") +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS1"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#ed7d31",size = 1) +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS2"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#3483ac", size = 1) +
  geom_point(data = ws_stations %>% dplyr::filter(area == "WS3"), mapping = aes(x = deploy_longitude, y = deploy_latitude), colour = "#FF00BA", size = 1) +
  # geom_point(data = ws_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, colour = area, shape = "Receiver Station"), size = 1) +
  # geom_point(data = ws_stations, mapping = aes(x = deploy_longitude, y = deploy_latitude, colour = area, shape = "Receiver Station"), size = 1) +
  ggspatial::annotation_scale(location = "tr", width_hint = 0.15, bar_cols = c("gray0", "white"), text_family = "serif") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", 
                                    pad_x = unit(0.06, "in"), pad_y = unit(0.3, "in"),
                                    height =  unit(0.75, "cm"), width = unit(0.75, "cm"),
                                    style = north_arrow_fancy_orienteering) +
  # geom_label(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
  #            stat = "sf_coordinates", size = 2, nudge_y = 0.25, nudge_x = 0, family = "serif", fontface = "bold", fill = "white") +
  geom_label(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
             stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -6, family = "serif", fontface = "bold", fill = "white") +
  geom_text_repel(data = Neeltje_Jans, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.09, nudge_x = 0.25, family = "serif") +
  # geom_text_repel(data = receiver_stations %>% dplyr::filter(station_name == "Birkenfels"), aes(label = station_name, x = deploy_longitude, y = deploy_latitude),
  #                 size = 2, nudge_y = 0.07, nudge_x = -0.35, family = "serif") +
  # geom_text_repel(data = Bergen_op_Zoom, aes(label = preferredGazetteerName, geometry = geometry),
  #                 stat = "sf_coordinates", size = 2, nudge_y = 0.1, nudge_x = 0.23, family = "serif") +
  geom_text_repel(data = Vlissingen, aes(label = preferredGazetteerName, geometry = geometry),
                  stat = "sf_coordinates", size = 2, nudge_y = 0.05, nudge_x = 0.15, family = "serif") +
  scale_shape_manual(name = "PBARN", values = c("Receiver Station" = 19)) +
  guides(colour = guide_legend(override.aes = list(shape = 19, size = 5))) 


# save maps ####

save_data(data = map_overview_ggplot, folder = path_maps)
save_data(data = map_dst_308, folder = path_maps)
save_data(data = map_dst_321, folder = path_maps)
save_data(data = map_geolocation_ggplot, folder = path_maps)
save_data(data = map_detail_ggplot, folder = path_maps)
save_data(data = map_WS_ggplot, folder = path_maps)


# old ####

# track_321 <- sf::st_as_sf(x = masterias_dst_geolocation_output %>% dplyr::filter(tag_serial_number == '1293321', coords = c("detection_latitude, detection_longitude")))
# 
# # Load required libraries
# library(sf)
# library(dplyr)
# 
# # Create a dataframe with coordinate columns
# df <- data.frame(lat = c(40.7128, 34.0522, 41.8781),
#                  lon = c(-74.0060, -118.2437, -87.6298),
#                  id = c("A", "B", "C"))
# 
# # Convert the dataframe to an sf object
# sf_df <- st_as_sf(df, coords = c("lon", "lat"))
# 
# # Set the CRS for the sf object
# st_crs(sf_df) <- "+proj=longlat +datum=WGS84"
# 
# # Transform the sf object
# # sf_df_transformed <- st_transform(sf_df, crs = "<target_CRS>")
# 
# # Group and summarize to create the polyline
# polyline <- sf_df_transformed %>%
#   group_by(id) %>%
#   summarize(geometry = st_cast(geometry, to = "LINESTRING"))
# 
# 
# 
# 
# 
# map_dstoutput_ggplot <- ggplot() +
#   # Stuff for bathy legend
#   geom_sf(data = Schelde_boundaries, mapping = aes(fill = "20 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
#   geom_sf(data = Schelde_boundaries, mapping = aes(fill = "50 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
#   geom_sf(data = Schelde_boundaries, mapping = aes(fill = "75 m"), colour = "transparent", alpha = 1,linewidth = 0.75) +
#   geom_sf(data = Schelde_boundaries, fill = "white", colour = "transparent", alpha = 1,linewidth = 0.75) +
#   # now the map layers
#   geom_sf(data = Europe) + #, colour = "gray85", fill = "gray85", linewidth = 0.75
#   geom_sf(data = polyline, colour = "gray60") +
#   geom_sf(data = France, colour = "gray60") +
#   geom_sf(data = Netherlands, colour = "gray60") +
#   geom_sf(data = Southern_North_Sea, fill = "transparent", colour = "gray0",linewidth = 0.75) +
#   # geom_sf(data = English_channel, fill = "transparent", mapping = aes(colour = "marine_boundaries"),linewidth = 0.75) +
#   # geom_sf(data = Dutch_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
#   # geom_sf(data = French_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
#   # geom_sf(data = UK_EEZ, mapping = aes(colour = "EEZ"),linewidth = 0.25, fill = "transparent") +
#   
#   # marine boundaries
#   # geom_sf(data = Schelde_boundaries, fill = "transparent", mapping = aes(colour = "Scheldt Estuary"),linewidth = 0.75) +
#   # geom_sf(data = BPNS, mapping = aes(colour = "BPNS"),linewidth = 0.75, fill = "transparent") +
#   # theme(panel.background = element_rect(fill = "lightblue")) +
#   labs(x = "Longitude", y = "Latitude") +
#   scale_colour_manual(name = "Marine Boundaries", values = c("EEZ" = "gray60", 
#                                                              # "marine_boundaries" = "gray60", 
#                                                              "Scheldt Estuary" = "darkgreen", 
#                                                              "BPNS" = "darkorange")) +
#   scale_fill_manual(name = "Bathymetry", values = c("EEZ" = "transparent", 
#                                                     "marine_boundaries" = "transparent", 
#                                                     "Scheldt" = "transparent", 
#                                                     "BPNS" = "transparent",
#                                                     "20 m" = "#CDCDE9",
#                                                     "50 m" = "#8585C7",
#                                                     "75 m" = "#38389F")) +
#   guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
#   geom_contour(data = bathy_northsea,
#                aes(x=longitude, y=latitude, z=depth_m),
#                breaks=c(-20),
#                linewidth=c(0.5),
#                colour="darkblue",
#                alpha = 0.25) +
#   geom_contour(data = bathy_northsea,
#                aes(x=longitude, y=latitude, z=depth_m),
#                breaks=c(-50),
#                linewidth=c(0.5),
#                colour="darkblue",
#                alpha = 0.5) +
#   geom_contour(data = bathy_northsea,
#                aes(x=longitude, y=latitude, z=depth_m),
#                breaks=c(-75),
#                linewidth=c(0.5),
#                colour="darkblue",
#                alpha = 0.75) +
#   # lat = c(40.7128, 34.0522, 41.8781),
# # lon = c(-74.0060, -118.2437, -87.6298),
#   coord_sf(crs = st_crs(4326), expand = FALSE, ylim = c(35, 45), xlim = c(-120, -70)) +
#   ggspatial::annotation_scale(location = "br", width_hint = 0.25, bar_cols = c("gray0", "white"), text_family = "serif") +
#   ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
#                                     pad_x = unit(0.15, "in"), pad_y = unit(0.25, "in"),
#                                     style = north_arrow_fancy_orienteering) +
#   # geom_text(data = English_channel, aes(label = preferredGazetteerName, geometry = the_geom),
#   #           stat = "sf_coordinates", size = 2, nudge_y = 0.25, nudge_x = 0, family = "serif", fontface = "bold") +
#   # geom_text(data = Southern_North_Sea, aes(label = preferredGazetteerName, geometry = the_geom),
#   #           stat = "sf_coordinates", size = 2, nudge_y = -0.15, nudge_x = -0.85, family = "serif", fontface = "bold") +
#   geom_text_repel(data = Hurd_deep, aes(label = preferredGazetteerName, geometry = geometry),
#                   stat = "sf_coordinates", size = 1.5, nudge_y = -0.35, nudge_x = -2, family = "serif") +
#   geom_text_repel(data = Cap_de_la_Hague, aes(label = preferredGazetteerName, geometry = geometry),
#                   stat = "sf_coordinates", size = 1.5, nudge_y = -0.85, nudge_x = 2, family = "serif") +
#   geom_text_repel(data = Norwich, aes(label = preferredGazetteerName, geometry = geometry),
#                   stat = "sf_coordinates", size = 1.5, nudge_y = 0.35, nudge_x = 1.5, family = "serif")

