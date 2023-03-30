# script to load bathymetry data from NOAA, queried with the marmap package
# Author: Lotte Pohl

library(readr)
library(tidyverse)
library(raster)
library(marmap)
library(sf)

rm(list =ls())
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/functions.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_marine_boundaries.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_bathy.R"))

rm(bathy_belgium_coarse, bathy_belgium_coarse_raster, bathy_northsea, bathy_northsea_raster)

map_bathy <- leaflet() %>%
  setView(3, 51.5, zoom = 6) %>%
  leafem::addMouseCoordinates() %>%
  addTiles() %>% 
  addRasterImage(bathy_belgium_raster, opacity = 1, colors = "viridis", group = "bathymetry") %>%
  addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "#ECE4BF", weight = 0, fillOpacity = 1, group = "bathymetry")
  # addCircleMarkers(data = bathy_belgium,
  #                  lat = ~latitude,
  #                  lng = ~longitude,
  #                  fillOpacity = 0,
  #                  opacity = 0,
  #                  label = ~paste0(depth_m %>% round(digits = 1), " m"))
# addLegend(pal = pal, values = c(-40, -20, -10, -5, -3, -2, -1, 0, 1), title = "Depth",
# position = "bottomright", opacity = 1)

map_bathy
