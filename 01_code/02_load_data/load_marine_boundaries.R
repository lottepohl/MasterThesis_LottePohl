# script to load marine boundaries
# Author: Lotte Pohl

# source(paste0(dir_path, "/functions.R"))
paste0("C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/01_code/06_functions/functions.R") %>% source()

BPNS <- load_data(filestring = "BPNS", folder = path_boundaries)
coastline <- st_read(paste0(path_boundaries, "ne_10m_coastline.shp"))

bbox_coastline <- c(xmin = -0.33, ymin = 49.5, xmax = 6, ymax = 54)
coastline <- crop_geom(coastline, bbox_coastline)
coastline_BE <- coastline$geometry[2]
new_point <- st_point(c(5.95, 49.39))
coastline_BE_poly <- st_linestring(rbind(st_coordinates(coastline_BE)[, c(1, 2)], st_coordinates(new_point)))


rm(bbox_coastline, new_point)
