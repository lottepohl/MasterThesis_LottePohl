# Script to query Marine Boundary Data through the `mregions2` R package
#  Author: Lotte Pohl

# rm(list = ls())
library(dplyr)
library(mregions2)
library(sf)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"

# source(paste0(dir_path, "/functions.R"))
paste0(dir_path, "/01_code/06_functions/functions.R") %>% source()

# EEZs
Dutch_EEZ <-  mregions2::gaz_search(5668) %>% mregions2::gaz_geometry()
BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry()
French_EEZ <- mregions2::gaz_search(5677) %>% mregions2::gaz_geometry()
UK_EEZ <- mregions2::gaz_search(5696) %>% mregions2::gaz_geometry()

# Country boundaries

Belgium <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()
Netherlands <- mregions2::gaz_search(15) %>% mregions2::gaz_geometry()
Europe <- mregions2::gaz_search(1920) %>% mregions2::gaz_geometry()
France <- mregions2::gaz_search(17) %>% mregions2::gaz_geometry()

# marine boundaries
Schelde_boundaries <- mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()
Western_Scheldt_boundaries <- mregions2::gaz_search(4752) %>% mregions2::gaz_geometry()
Eastern_Scheldt_boundaries <- mregions2::gaz_search(5332) %>% mregions2::gaz_geometry()
English_channel <- mregions2::gaz_search(2389) %>% mregions2::gaz_geometry()
Southern_North_Sea <- mregions2::gaz_search(22253) %>% mregions2::gaz_geometry()
Northern_North_Sea <- mregions2::gaz_search(17977) %>% mregions2::gaz_geometry()
North_sea <- mregions2::gaz_search(2350) %>% mregions2::gaz_geometry()

# important locations

Zoetelande <- mregions2::gaz_search(14717) %>% mregions2::gaz_geometry()
Cap_de_la_Hague <- mregions2::gaz_search(20019) %>% mregions2::gaz_geometry()
Neeltje_Jans <- mregions2::gaz_search(17757) %>% mregions2::gaz_geometry()
Dishoek <- sf::st_point(x = c(3.52352, 51.47310))
Hurd_deep <- mregions2::gaz_search(3321) %>% mregions2::gaz_geometry()
Norwich <- mregions2::gaz_search(32698) %>% mregions2::gaz_geometry()
Vlissingen <- mregions2::gaz_search(9105) %>% mregions2::gaz_geometry()
Bergen_op_Zoom <- mregions2::gaz_search(23137) %>% mregions2::gaz_geometry()

save_data(data = Norwich, folder = path_boundaries)
save_data(data = Cap_de_la_Hague, folder = path_boundaries)
save_data(data = Bergen_op_Zoom, folder = path_boundaries)
save_data(data = Neeltje_Jans, folder = path_boundaries)
save_data(data = Vlissingen, folder = path_boundaries)
save_data(data = Dutch_EEZ, folder = path_boundaries)
save_data(data = French_EEZ, folder = path_boundaries)
save_data(data = UK_EEZ, folder = path_boundaries)
save_data(data = Europe, folder = path_boundaries)
save_data(data = France, folder = path_boundaries)
save_data(data = BPNS, folder = path_boundaries)
save_data(data = Northern_North_Sea, folder = path_boundaries)
save_data(data = Southern_North_Sea, folder = path_boundaries)
save_data(data = Eastern_Scheldt_boundaries, folder = path_boundaries)
save_data(data = Western_Scheldt_boundaries, folder = path_boundaries)
save_data(data = Schelde_boundaries, folder = path_boundaries)
save_data(data = Belgium, folder = path_boundaries)
save_data(data = Netherlands, folder = path_boundaries)
save_data(data = English_channel, folder = path_boundaries)
save_data(data = Hurd_deep, folder = path_boundaries)
save_data(data = North_sea, folder = path_boundaries)
save_data(data = Zoetelande, folder = path_boundaries)
save_data(data = Dishoek, folder = path_boundaries)

# include:
# relevant_mrgids <- c(2389, 3141, 2351, 2357, 22253, 17977, 2359, 24178)
# sea_areas <- mregions2::gaz_search(relevant_mrgids) %>% mregions2::gaz_geometry()


leaflet() %>% addTiles() %>%
  addMouseCoordinates() %>%
  addCircleMarkers(data = Dishoek)
