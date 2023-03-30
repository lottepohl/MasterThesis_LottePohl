# Script to query Marine Boundary Data through the `mregions2` R package
#  Author: Lotte Pohl

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/functions.R"))



BPNS <- mregions2::gaz_search(3293) %>% mregions2::gaz_geometry()

Belgium <- mregions2::gaz_search(14) %>% mregions2::gaz_geometry()

save_data(data = BPNS, folder = path_boundaries)
save_data(data = Belgium, folder = path_boundaries)

# include:
# relevant_mrgids <- c(2389, 3141, 2351, 2357, 22253, 17977, 2359, 24178)
# sea_areas <- mregions2::gaz_search(relevant_mrgids) %>% mregions2::gaz_geometry()


