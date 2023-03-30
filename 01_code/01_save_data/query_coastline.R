
library(oce)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/functions.R"))

# Define the bounding box (xmin, ymin, xmax, ymax)
# bbox <- c(-180, -90, 180, 90)
bbox <- c(-0.33, 51, 5, 51.9)

# Download the 1:10m scale coastline data within the bbox


coastline_file <- utils::download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
                                destfile = paste0(path_boundaries, "ne_10m_coastline.zip"), mode = "wb")

# utils::unzip("~/github_repos/ADST_Mustelus_asterias/03_data/marine_boundaries/ne_10m_coastline.zip") #, exdir = ".", overwrite = TRUE)

# TODO FIND OUT HOW TO UNZIP FILES 


