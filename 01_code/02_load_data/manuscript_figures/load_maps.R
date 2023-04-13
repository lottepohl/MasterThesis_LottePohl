# script to load the maps generated for the thesis manuscript

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% source()

map1_overview <- load_data(filestring = "map1_overview", folder = path_maps)
map2_detail <- load_data(filestring = "map2_detail", folder = path_maps)

# map1_overview
