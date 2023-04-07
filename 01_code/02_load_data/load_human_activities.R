# Script to load the environmental layers that were queried and saved in `query_and_save_environmental_layers_EmodNET.R`

# source(paste0(dir_path, "/functions.R"))
paste0("C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/01_code/06_functions/functions.R") %>% source()

windfarms_polygons <- load_data(filestring = "windfarms_polygons", folder = path_envdata) %>% filter(year <= 2020)
windfarms_points <- load_data(filestring = "windfarms_points", folder = path_envdata) %>% filter(year <= 2020)
wrecks <- load_data(filestring = "wrecks", folder = path_envdata)
wrecks_BE <- load_data(filestring = "wrecks_BE", folder = path_envdata)
cables <- load_data(filestring = "cables", folder = path_envdata)

