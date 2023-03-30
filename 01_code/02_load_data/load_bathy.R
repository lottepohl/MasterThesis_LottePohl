# script to load bathymetry data from NOAA, queried with the marmap package
# Author: Lotte Pohl

library(readr)
library(tidyverse)
library(raster)
library(marmap)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
source(paste0(dir_path, "/functions.R"))
# source(paste0(dir_path, "/02_scripts/03_wrangle_data/wrangle_acoustic_data.R"))


bathy_belgium <- read_csv(paste0(dir_path,"/03_data/environmental_layers/bathymetry/belgium/marmap_coord_1.9;50.9;4.7;52.4_res_0.25.csv"), show_col_types = F) %>% rename(longitude = V1, latitude = V2, depth_m = V3)
bathy_belgium_raster <- bathy_belgium %>% raster::rasterFromXYZ(crs = "EPSG:4326")
# raster::writeRaster(bathy_belgium_raster, paste0(dir_path), "/03_data/environmental_layers/bathymetry/bathy_NOAA_res_1.tif", format = "GTiff", overwrite = T)

bathy_belgium_coarse <- read_csv(paste0(dir_path,"/03_data/environmental_layers/bathymetry/belgium/marmap_coord_1.9;50.9;4.7;52.4_res_1.csv"), show_col_types = F) %>% rename(longitude = V1, latitude = V2, depth_m = V3)
bathy_belgium_coarse_raster <- bathy_belgium_coarse %>% raster::rasterFromXYZ(crs = "EPSG:4326")

bathy_northsea <- read_csv(paste0(dir_path,"/03_data/environmental_layers/bathymetry/north_sea/marmap_coord_-9;43;7;56_res_2.csv"), show_col_types = F) %>% rename(longitude = V1, latitude = V2, depth_m = V3)
bathy_northsea_raster <- bathy_northsea %>% raster::rasterFromXYZ(crs = "EPSG:4326")
# raster::writeRaster(bathy_belgium_raster, paste0(dir_path), "/03_data/environmental_layers/bathymetry/bathy_NOAA_res_1.tif", format = "GTiff", overwrite = T)





# Define the color palette for the legend
# pal <- colorNumeric(c("#2c7bb6", "#ffffbf", "#d7191c"), ansparent")
# pal <- colorNumeric(palette = marmap::etopo.colors(bathy_raster@data@values %>% unique() %>% length()), domain = bathy_raster@data@values)
# palette.bathy(mat, layers, land=FALSE, default.col="white")
# values(bathy_belgium_raster)
# pal <- colorNumeric(palette = c("#2541b2", "#2c72a5", "#2f8c8e", "#3b9e4d", "#9bbd5a",
#                                          "#e2d855", "#f7bc28", "#e38220", "#d54917", "#ba3325", "#a62a2a"), domain = bathy_belgium_raster@data@values)
#                                          
# pal <- colorNumeric(palette = c("#00656B", "#4D9696", "#9CA495", "#DCCD94", "#ECE4BF", "#FFF5B0", "#FFFADB"), domain = bathy_belgium_raster@data@values, reverse = T)


# Add the raster layer to the map
# m %>% addRasterImage(r, opacity = 0.8)

# Display the map
# m