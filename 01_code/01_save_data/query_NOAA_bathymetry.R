# script to query bathymetry data from NOAA
# Author: Lotte Pohl

library(readr)
library(tidyverse)
library(raster)
library(marmap)

rm(list =ls())
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source

bathy <- marmap::getNOAA.bathy(1.9, 4.7, 50.9, 52.4, resolution = 0.25,
              keep = T, antimeridian = FALSE, path = paste0(dir_path, "/03_data/environmental_layers/bathymetry/belgium"))
bathy_coarse <- marmap::getNOAA.bathy(1.9, 4.7, 50.9, 52.4, resolution = 1,
                               keep = T, antimeridian = FALSE, path = paste0(dir_path, "/03_data/environmental_layers/bathymetry/belgium"))

# bathy is automatically saved as csv

bathy_northsea <- marmap::getNOAA.bathy(-9, 7, 43, 56, resolution = 2,
                               keep = T, antimeridian = FALSE, path = paste0(dir_path, "/03_data/environmental_layers/bathymetry/north_sea"))
