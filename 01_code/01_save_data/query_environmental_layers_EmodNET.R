# Get environmental layers With EMODnetWFS R package ####
# following this tutorial: https://emodnet.github.io/EMODnetWFS/

## 0. load libraries ####
# devtools::install_github("EMODnet/EMODnetWFS")
library(EMODnetWFS)
library(leaflet)
library(mregions2)
library(tidyverse)
library(readr)
library(leafem)
library(sf)

# knitting <- T
# pre <- ifelse(knitting %>% isFALSE(), ".", "..") 

path_envdata <- paste0(getwd(), "/00_data/environmental_layers/")
path_boundaries <- paste0(getwd(), "/00_data/marine_boundaries/")

## 1. view available webservices ####
View(emodnet_wfs())

## 2. initiate clients ####
wfs_bio <- emodnet_init_wfs_client(service = "biology")
wfs_human <- emodnet_init_wfs_client(service = "human_activities")
wfs_physics <- emodnet_init_wfs_client(service = "physics")
wfs_bathy <- emodnet_init_wfs_client(service = "bathymetry")
wfs_seabeds <- emodnet_init_wfs_client(service = "geology_seabed_substrate_maps")
wfs_habitats <- emodnet_init_wfs_client(service = "seabed_habitats_general_datasets_and_products")

## 3. overview on layers available ####
# wfs_bio %>% emodnet_get_wfs_info() %>% View() # 
layer_names_bio <- c("OOPS_regions", "Species_gridded_abundances_10year") #(2year, 3year)
wfs_human %>% emodnet_get_wfs_info() %>% View() # interesting layers, fishingsubsurface, fishingstaticgears, eeacoastline,
# eez, faoareas, heritageshipwrecks, maritimebnds, portvessels, natura2000areas/wdpaareas, ospar, pcablesshom/pcablesrijks/pcablesnve/maltacables/sigcables/shomcables/cicacables/bshcontiscables
# , pipelines, windfarms/windfarmspoly/oenergy
# too many layers to load at once
layer_names_human <- c("eeacoastline", "eez", "faoareas", "maritimebnds", "portvessels", "natura2000areas", "wdpaareas", "ospar", "oenergy") #"fishingsubsurface", "fishingstaticgears", 
# wfs_physics %>% emodnet_get_wfs_info() %>% View() # not very relevant to me # interesting layers: EP_GEO_INT_UWNO_XX_PD_INR (evtl)
# wfs_bathy %>% emodnet_get_wfs_info() %>% View() # interesting layers: hr_bathymetry_area, contours, download_tiles, undersea_features (?)
layer_names_bathy <- c("hr_bathymetry_area", "contours", "download_tiles")
# wfs_seabeds %>% emodnet_get_wfs_info() %>% View() # interesting layers: seabed_substrate_100k, seabed_substrate_10k_multiscale
layer_names_seabeds <- c("seabed_substrate_100k", "seabed_substrate_10k_multiscale")
# wfs_habitats %>% emodnet_get_wfs_info() %>% View() # interesting layers: eusm2019_regions, habitat_point_bboxes
layer_names_habitats <- c("eusm2019_regions", "habitat_point_bboxes")

## 4. get layers ####

# for now: windfarms, cables and shipwrecks
# layer_names_human <- c("windfarms")

layers_windfarms <- wfs_human %>% emodnet_get_layers(layers = c("windfarms", "windfarms_poly"), crs = 4326) 
layers_wrecks <- wfs_human %>% emodnet_get_layers(layers = "heritageshipwrecks", crs = 4326) 
layers_cables <- wfs_human %>% emodnet_get_layers(layers = c("pcablesshom", "pcablesrijks", "pcablesnve", "maltacables", "sigcables", "shomcables", "cicacables", "bshcontiscables"), crs = 4326) 

# layers_bathy <- wfs_bathy %>% emodnet_get_layers(layers = layer_names_bathy, crs = 4326)
# layers_seabeds <- wfs_seabeds %>% emodnet_get_layers(layers = layer_names_seabeds, crs = 4326)
# layers_habitats <- wfs_habitats %>% emodnet_get_layers(layers = layer_names_habitats, crs = 4326)
layers_humans <- wfs_human %>% emodnet_get_layers(layers = layer_names_human, crs = 4326) 

### 4.a.extract single layers ####
wrecks <- layers_wrecks %>% purrr::pluck("heritageshipwrecks")
windfarms_polygons <- layers_windfarms %>% purrr::pluck("windfarmspoly")
windfarms_points <- layers_windfarms %>% purrr::pluck("windfarms")
pcablesshom <- layers_cables %>% purrr::pluck("pcablesshom") %>% mutate(name = "pcablesshom") %>% dplyr::select(name, the_geom)
pcablesrijks <- layers_cables %>% purrr::pluck("pcablesrijks") %>% mutate(name = "pcablesrikjs") %>% dplyr::select(name, the_geom)
sigcables <- layers_cables %>% purrr::pluck("sigcables") %>% mutate(name = "sigcables") %>% dplyr::select(name, the_geom)
shomcables <- layers_cables %>% purrr::pluck("shomcables") %>% mutate(name = "shomcables") %>% dplyr::select(name, the_geom)
# bathy <- layers_bathy %>% purrr::pluck("hr_bathymetry_area")
bathy_contours <- layers_bathy %>% purrr::pluck("contours")
# bathy_tiles <- layers_bathy %>% purrr::pluck("download_tiles")
# seabed_substrate_100k <- layers_seabeds %>% purrr::pluck("seabed_substrate_100k")
# seabed_substrate_10k <- layers_seabeds %>% purrr::pluck("seabed_substrate_10k_multiscale")
# habitats_regions <- layers_habitats %>% purrr::pluck("eusm2019_regions")
# habitats_points <- layers_habitats %>% purrr::pluck("habitat_point_bboxes")
# windfarms_points <- layers_windfarms %>% purrr::pluck("windfarms")




### 4.b. get Belgian shipwrecks ####

### 4.c. make one cable file
cables <- rbind(pcablesrijks, pcablesshom, shomcables, sigcables)


# downloaded from: https://wrakkendatabank.afdelingkust.be
wrecks_BE <- readr::read_delim(paste0(path_envdata, "wrecks-export-20-02-2023.csv"), 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# convert coords from Easting/Northing to lon/lat
wrecks_BE_lonlat <- wrecks_BE %>% dplyr::select(Easting, Northing) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 32631) %>% # Belgium CRS: 32631
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()

wrecks_BE <- wrecks_BE %>% 
  mutate(longitude = wrecks_BE_lonlat$X, latitude = wrecks_BE_lonlat$Y) %>%
  rename(sink_yr = `Gezonken op`)

## 5. crop geometries to bbox ####

bbox_geom <- c(xmin = -3, ymin = 49.5, xmax = 6, ymax = 54)

wrecks <- crop_geom(wrecks, bbox_geom)
# windfarms_polygons <- crop_geom(windfarms_polygons, bbox_geom)
windfarms_points <- crop_geom(windfarms_points, bbox_geom)
cables <- crop_geom(cables, bbox_geom)


## 7. save layers as rds ####
save_data(data = wrecks, folder = path_envdata)
save_data(data = windfarms_points, folder = path_envdata)
save_data(data = windfarms_polygons, folder = path_envdata)
save_data(data = BPNS, folder = path_boundaries)
save_data(data = wrecks_BE, folder = path_envdata)
save_data(data = cables, folder = path_envdata)
# save_data(data = habitats_points, folder = path_envdata)
# save_data(data = habitats_regions, folder = path_envdata)
# save_data(data = seabed_substrate_10k, folder = path_envdata)
# save_data(data = seabed_substrate_100k, folder = path_envdata)
# save_data(data = bathy_tiles, folder = path_envdata)
save_data(data = bathy_contours, folder = path_envdata)
save_data(data = bathy, folder = path_envdata)

## 8. functions ####

save_data <- function(data, folder){
  base::saveRDS(data, file = paste0(folder, deparse(substitute(data)), ".rds"))
}

crop_geom <- function(geom, bbox){
  cropped_geom <- sf::st_crop(geom, bbox)
  return(cropped_geom)
}

## 9. Remove unneccessary files
rm(path_envdata, path_boundaries, bbox_geom, crop_geom, save_data, wfs_physics, wfs_bathy, wfs_bio, wfs_habitats, wfs_human, wfs_seabeds,
   layers_humans, layers_habitats, layers_seabeds, layers_bathy, layer_names_habitats, layer_names_seabeds, 
   layer_names_bathy, layer_names_human, layer_names_bio)