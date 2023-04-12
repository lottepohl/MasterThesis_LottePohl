# Script to query Marine Boundary Data through the `mregions2` R package
#  Author: Lotte Pohl

# rm(list = ls())
library(dplyr)
library(mregions2)
library(sf)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"

# source(paste0(dir_path, "/functions.R"))
paste0(dir_path, "/01_code/06_functions/functions.R") %>% source()


BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry()
Schelde_boundaries <- mregions2::gaz_search(4812) %>% mregions2::gaz_geometry()
English_channel <- mregions2::gaz_search(2389) %>% mregions2::gaz_geometry()

Hurd_deep <- mregions2::gaz_search(3321) %>% mregions2::gaz_geometry()
North_sea <- mregions2::gaz_search(2350) %>% mregions2::gaz_geometry()

Belgium <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()
Netherlands <- mregions2::gaz_search(15) %>% mregions2::gaz_geometry()

save_data(data = BPNS, folder = path_boundaries)
save_data(data = Schelde_boundaries, folder = path_boundaries)
save_data(data = Belgium, folder = path_boundaries)
save_data(data = Netherlands, folder = path_boundaries)
save_data(data = English_channel, folder = path_boundaries)
save_data(data = Hurd_deep, folder = path_boundaries)
save_data(data = North_sea, folder = path_boundaries)

# include:
# relevant_mrgids <- c(2389, 3141, 2351, 2357, 22253, 17977, 2359, 24178)
# sea_areas <- mregions2::gaz_search(relevant_mrgids) %>% mregions2::gaz_geometry()


