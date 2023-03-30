# author: Lotte Pohl
# date: 20230220
# 
# title: load environmental layers into R

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# FIRST CASE: with sdmpredictors R package ####
# following this tutorial: http://lifewatch.github.io/sdmpredictors/articles/quickstart.html

## 0. load libraries ####
# if(!require("sdmpredictors")) install.packages("sdmpredictors")
library(devtools)
# library(remotes)
# devtools::install_github("lifewatch/sdmpredictors")
library(sdmpredictors)
library(raster)


## 1. data exploration ####
# exploring the marine datasets 
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

# exploring the marine layers 
layers <- list_layers(datasets)
# interesting layers: MS_bathy_5m, MS_biogeo5_dist_shore_5m, MS_sst01_5m (02,03,...), BO22_pH, BO2_tempmean_bdmax, BO_salinity

## 2. getting the Bibtex citation files ####
print(lapply(dataset_citations(datasets, astext = FALSE), toBibtex))

## 3. load layers

### bathymetry ####
bathy <- load_layers("MS_bathy_5m", datadir = path_envdata)
layer1 <- bathy@layers[[1]]

### salinity ####
sal <- load_layers("BO_salinity", datadir = path_envdata)
sal <- sal@layers[[1]]
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(sal),
                    na.color = "transparent")
sal_leaflet <- rasterImage(sal, opacity = 1, project = TRUE)
plot(sal)
# writeRaster(layer1, paste0(path_envdata,"biooracle_bathy_5m"), format = "GTiff", overwrite = TRUE)

sal2 <- load_layers("MS_sst04_5m", datadir = path_envdata)
sal2 <- sal2@layers[[1]]
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(sal2),
                    na.color = "transparent")
plot(sal2)
## 4. make map with raster images
leaflet() %>% addTiles() %>%
  addRasterImage(x = sal2, colors = pal) # doesnt work

leaflet::leaflet(
  options = leaflet::leafletOptions(crs = leaflet::leafletCRS("L.CRS.EPSG4326"))
) %>% addTiles() %>%
  addRasterImage(x = sal2, colors = pal)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# SECOND CASE: With EMODnetWFS R package ####
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

path_envdata <- "./03_data/environmental_layers/"
path_boundaries <- "./03_data/marine_boundaries/"

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
wfs_bio %>% emodnet_get_wfs_info() %>% View() # 
layer_names_bio <- c("OOPS_regions", "Species_gridded_abundances_10year") #(2year, 3year)
wfs_human %>% emodnet_get_wfs_info() %>% View() # interesting layers, fishingsubsurface, fishingstaticgears, eeacoastline, 	
# eez, faoareas, heritageshipwrecks, maritimebnds, portvessels, natura2000areas/wdpaareas, ospar, pcablesshom/pcablesrijks/pcablesnve/maltacables/sigcables/shomcables/cicacables/bshcontiscables
# , pipelines, windfarms/windfarmspoly/oenergy
# too many layers to load at once
layer_names_human <- c("eeacoastline", "eez", "faoareas", "maritimebnds", "portvessels", "natura2000areas", "wdpaareas", "ospar", "oenergy") #"fishingsubsurface", "fishingstaticgears", 
wfs_physics %>% emodnet_get_wfs_info() %>% View() # not very relevant to me # interesting layers: EP_GEO_INT_UWNO_XX_PD_INR (evtl)
wfs_bathy %>% emodnet_get_wfs_info() %>% View() # interesting layers: hr_bathymetry_area, contours, download_tiles, undersea_features (?)
layer_names_bathy <- c("hr_bathymetry_area", "contours", "download_tiles")
wfs_seabeds %>% emodnet_get_wfs_info() %>% View() # interesting layers: seabed_substrate_100k, seabed_substrate_10k_multiscale
layer_names_seabeds <- c("seabed_substrate_100k", "seabed_substrate_10k_multiscale")
wfs_habitats %>% emodnet_get_wfs_info() %>% View() # interesting layers: eusm2019_regions, habitat_point_bboxes
layer_names_habitats <- c("eusm2019_regions", "habitat_point_bboxes")

## 4. get layers ####

# for now: windfarms, cables and shipwrecks
# layer_names_human <- c("windfarms", "heritageshipwrecks", "eez", "faoareas", "portvessels", "natura2000areas", "pcablesshom", "pcablesrijks", "pcablesnve", "maltacables", "sigcables", "shomcables", "cicacables", "bshcontiscables", "windfarmspoly")
# check which cables are interesting go us: 
## Shom: southeast of Scheldt https://services.data.shom.fr/geonetwork/srv/api/records/BDML_CABLES.xml
## 
layers_windfarms <- wfs_human %>% emodnet_get_layers(layers = c("windfarms", "windfarmspoly"), crs = 4326) 
layers_wrecks <- wfs_human %>% emodnet_get_layers(layers = "heritageshipwrecks", crs = 4326) 
cables <- wfs_human %>% emodnet_get_layers(layers = c("pcablesshom", "pcablesrijks", "pcablesnve", "maltacables", "sigcables", "shomcables", "cicacables", "bshcontiscables"), crs = 4326) 

layers_bathy <- wfs_bathy %>% emodnet_get_layers(layers = layer_names_bathy, crs = 4326)
layers_seabeds <- wfs_seabeds %>% emodnet_get_layers(layers = layer_names_seabeds, crs = 4326)
layers_habitats <- wfs_habitats %>% emodnet_get_layers(layers = layer_names_habitats, crs = 4326)
layers_humans <- wfs_human %>% emodnet_get_layers(layers = layer_names_human, crs = 4326) 

### 4.a.extract single layers ####
wrecks <- layers_wrecks %>% purrr::pluck("heritageshipwrecks")
windfarms_points <- layers_windfarms %>% purrr::pluck("windfarms")
windfarms_polygons <- layers_windfarms %>% purrr::pluck("windfarmspoly")
bathy <- layers_bathy %>% purrr::pluck("hr_bathymetry_area")
bathy_contours <- layers_bathy %>% purrr::pluck("contours")
bathy_tiles <- layers_bathy %>% purrr::pluck("download_tiles")
seabed_substrate_100k <- layers_seabeds %>% purrr::pluck("seabed_substrate_100k")
seabed_substrate_10k <- layers_seabeds %>% purrr::pluck("seabed_substrate_10k_multiscale")
habitats_regions <- layers_habitats %>% purrr::pluck("eusm2019_regions")
habitats_points <- layers_habitats %>% purrr::pluck("habitat_point_bboxes")

# pcablesnve <- layers_cables$pcablesnve %>% st_collection_extract("POINT") #empty layer


### 4.b. get Belgian shipwrecks ####
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

## 5. cut shapes to bbox ####

bbox_geoms <- c(xmin = -1.5, ymin = 49, xmax = 6, ymax = 54)

bbox_test <- st_bbox(wrecks) + c(5, 5, -5, -5)

wrecks_cropped <- st_crop(wrecks, bbox_geoms)

# test map

leaflet() %>% addTiles() %>%
  addPolylines(data = shomcables,
                   color = "red",
                   group = "Shipwrecks") %>%
  leafem::addMouseCoordinates() %>%
  addCircleMarkers(data = wrecks_cropped,
                   fillColor = "blue",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 3,
                   label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
                   group = "Shipwrecks")


leaflet() %>% 
  addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.75)) %>% 
  addScaleBar(position = "topright", options = scaleBarOptions(maxWidth = 250, imperial = F)) %>%
  setView(3.5, 51.5, zoom = 9) %>% 
  #### BPNS ####
  addPolygons(data = BPNS, color = "grey", 
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0, 
              group = "Regions") %>%
  #### submarine cables ####
  addPolylines(data = layers_cables$pcablesshom,
               color = "lightblue",
               label = "pcablesshom",
               group = "Submarine cables") %>%
  addPolylines(data = layers_cables$pcablesrijks,
               color = "orange",
               label = "pcablesrijks",
               group = "Submarine cables") %>%
  addPolylines(data = layers_cables$sigcables,
               color = "darkgreen",
               label = "sigcables",
               group = "Submarine cables") %>%
  addPolylines(data = layers_cables$shomcables,
               color = "darkred",
               label = "shomcables",
               group = "Submarine cables") %>%
  #### shipwrecks ####
  addCircleMarkers(data = wrecks,
                 fillColor = "white",
                 opacity = 0,
                 fillOpacity = 1,
                 radius = 3,
                 label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
                 group = "Shipwrecks") %>%
  addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
                   lng = ~longitude,
                   lat = ~latitude,
                   fillColor = "white",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 3,
                   label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
                   group = "Shipwrecks") %>%
  #### windfarms ####
  addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
              color = "red",
              weight = 2,
              opacity = 1,
              fillOpacity = 0.3,
              label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
              group = "Offshore Wind Farms (OWF)") %>%
  #### habitats (doesnt work) ####
  # addPolygons(data = habitats_regions$geom,
  #              color = "grey",
  #              label = "habitat") %>%
  #### Layers Control ####
addLayersControl(overlayGroups = c("Shipwrecks", "Offshore Wind Farms (OWF)", "Submarine cables"), options = layersControlOptions(collapsed = F)) %>%
  leafem::addMouseCoordinates()



## 6. Change formats ####

# new test map
# leaflet() %>% addTiles() %>%
#   addCircleMarkers(data = bathy_contours2,
#               color = "grey")
# 
# bathy_contours2 <- bathy_contours %>% st_cast(to = "POINT")
# habitats_regions_atlantic <- habitats_regions %>% filter(euseamap_r == "Altanto-Arctic")


## 7. save layers as csv ####
save_data(data = wrecks, folder = path_envdata)
save_data(data = cables, folder = path_envdata)
# save_data(data = windfarms_points, folder = path_envdata)
save_data(data = windfarms_polygons, folder = path_envdata)
save_data(data = BPNS, folder = path_boundaries)
save_data(data = wrecks_BE, folder = path_envdata)
# save_data(data = habitats_points, folder = path_envdata)
# save_data(data = habitats_regions, folder = path_envdata)
# save_data(data = seabed_substrate_10k, folder = path_envdata)
# save_data(data = seabed_substrate_100k, folder = path_envdata)
# save_data(data = bathy_tiles, folder = path_envdata)
# save_data(data = bathy_contours, folder = path_envdata)
# save_data(data = bathy, folder = path_envdata)

# 8. functions ####

save_data <- function(data, folder){
  base::saveRDS(data, file = paste0(folder, deparse(substitute(data)), ".rds"))
}


