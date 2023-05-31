# script to load the maps generated for the thesis manuscript

# library(leaflet)
# library(htmlwidgets)
# library(webshot)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

map_overview_ggplot <- load_data(filestring = "map_overview_ggplot", folder = path_maps)
map_dst_308 <- load_data(filestring = "map_dst_308", folder = path_maps)
map_dst_321 <- load_data(filestring = "map_dst_321", folder = path_maps)
# map_geolocation_ggplot <- load_data(filestring = "map_geolocation_ggplot", folder = path_maps)
map_detail_ggplot <- load_data(filestring = "map_detail_ggplot", folder = path_maps)
map_WS_ggplot <- load_data(filestring = "map_WS_ggplot", folder = path_maps)
# map1_overview <- load_data(filestring = "map1_overview", folder = path_maps)
# map2_detail <- load_data(filestring = "map2_detail", folder = path_maps)
# 
# map1_overview
# map2_detail

# # save maps ####
# htmlwidgets::saveWidget(map1_overview, file = "tmp_map.html", selfcontained = FALSE)
# # webshot::webshot("tmp_map.html", file = paste0(path_maps, "map1_overview.pdf"), vwidth = 850, vheight = 500, cliprect = NULL)
# webshot::webshot("tmp_map.html", file = paste0(path_maps, "map1_overview.png"), vwidth = 850, vheight = 500, cliprect = NULL)
# 
# htmlwidgets::saveWidget(map2_detail, file = "tmp_map.html", selfcontained = FALSE)
# # webshot::webshot("tmp_map.html", file = paste0(path_maps, "map1_overview.pdf"), vwidth = 850, vheight = 500, cliprect = NULL)
# webshot::webshot("tmp_map.html", file = paste0(path_maps, "map2_detail.png"), vwidth = 850, vheight = 500, cliprect = NULL)
# 
# 
# # Remove large files
# file.remove("tmp_map.html")
# unlink("tmp_map_files", recursive = TRUE)
