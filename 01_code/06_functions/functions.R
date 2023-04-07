# load libraries ####
library(leaflet)
library(tidyverse)
# install.packages("devtools")
library(devtools)
# devtools::install_github("lifewatch/mregions2")
library("mregions2")
library(webshot)
library(mapview)
library(stringr)
# webshot::install_phantomjs(force = TRUE)

# 1. general info ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
path_envdata <- paste0(dir_path, "/00_data/environmental_layers/")
path_boundaries <- paste0(dir_path, "/00_data/marine_boundaries/")
path_actel <- paste0(dir_path, "/00_data/actel_files/")
project_code <- "ADST-Shark"
scientific_name <- "Mustelus asterias"
masterias_recaptured_serials <- c("1293295", "1293304", "1293310", "1293312", "1293319", "1293308", "1293321", "1293322")
# bounding box
min_lon <- 0.8
max_lon <- 5
max_lat <- 53
min_lat <- 50

# create basic map ####
# 
# create_basic_map <- function(palette = "Greys", include_regions = TRUE){
#  
#   # make map
#   map <- leaflet() %>% addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(opacity = 1))
#   if(include_regions == TRUE){
#     # get geometries
#     relevant_mrgids <- c(2389, 3141, 2351, 2357, 22253, 17977, 2359, 24178)
#     sea_areas <- mregions2::gaz_search(relevant_mrgids) %>% mregions2::gaz_geometry()
#     
#     # make color palette
#     pal <- colorFactor(palette, domain = sea_areas$preferredGazetteerName)
#     
#     map <- sea_areas %>% filter(status == "standard") %>% 
#         leaflet() %>% addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(opacity = 1)) %>%
#         addPolygons(stroke = T, weight = 2, opacity = 0.5, 
#                     color = "grey", fillColor = ~pal(preferredGazetteerName) ,
#                     fillOpacity = 0, label = ~preferredGazetteerName, 
#                     labelOptions = labelOptions(noHide = F, textOnly = F, direction = "center", textsize = "12px", sticky = FALSE),
#                     group = "Regions")
#     # addPolygons(stroke = T, weight = 2, opacity = 0.5, color = "grey", fillColor = ~pal(preferredGazetteerName) , fillOpacity = 0.3, label = ~preferredGazetteerName, labelOptions = labelOptions(noHide = F, textOnly = F, direction = "center", textsize = "12px", sticky = FALSE))
#     }
#   # map <- sea_areas %>% filter(status == "standard") %>% 
#   #   leaflet() %>% addProviderTiles("Esri.WorldStreetMap", options = providerTileOptions(opacity = 1)) %>%
#   #   addPolygons(stroke = T, weight = 2, opacity = 0.5, color = "grey", fillColor = ~pal(preferredGazetteerName) , fillOpacity = 0.3, label = ~preferredGazetteerName, labelOptions = labelOptions(noHide = F, textOnly = F, direction = "center", textsize = "12px", sticky = FALSE))
#   # 
#   return(map)
# }

# save html map ####
# not working for pdfs yet - always saves them wrong
save_leaflet <- function(map, path, filename, filetype, height = 1200, width = 900){
  if(filetype == "pdf"){
    htmlwidgets::saveWidget(map, paste0(filename, ".html"), selfcontained = FALSE)
    webshot::webshot(paste0(filename, ".html"), file = paste0(path,"/",filename,".",filetype), vwidth = width, vheight = height, cliprect = NULL)
  } 
  else{
    # shitty for pdfs
    mapview::mapshot(map,
                    file = paste0(path,"/",filename,".",filetype),
                    remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))
  }
  # Remove large files
  file.remove(paste0(filename, ".html"))
  unlink(paste0(filename, "_files"), recursive = TRUE)
}
  

# htmlwidgets::saveWidget(map_masterias_dist, "map_masterias_dist.html", selfcontained = FALSE)
# webshot::webshot("map_masterias_dist.html", file = "./visualisation_playground/maps_plots/map_masterias_dist.png", vwidth = 1200, vheight = 900, cliprect = NULL)
# 
## Remove large files ####
# file.remove("map_masterias_dist.html")
# unlink("map_masterias_dist_files", recursive = TRUE)

# for ETN data: get rid of double columns
remove_double_cols <- function(animals){
  animals$tag_serial_number <- gsub(",.*","", animals$tag_serial_number)
  animals$tag_type <- gsub(",.*","", animals$tag_type)
  animals$tag_subtype <- gsub(",.*","", animals$tag_subtype)
  animals$acoustic_tag_id <- gsub(",.*","", animals$acoustic_tag_id)
  return(animals)
}


# change date format for DST data ####
change_date_format <- function(data){
  data$date %>% as.character() %>% str_trunc(10, "right")
}

# clean animal info
# homogenise sex

clean_animal_info <- function(animal_info, animal_detections){
  # homogenise sex
  animal_info$sex[animal_info$sex == "F"] <- "female"
  animal_info$sex[animal_info$sex == "M"] <- "male"
  animal_info$sex[animal_info$sex == "female"] <- "f"
  animal_info$sex[animal_info$sex == "male"] <- "m"
  
  # change date and year format
  animal_info$release_year <- animal_info$release_date_time %>% format("%y")
  animal_info$release_date <- animal_info$release_date_time %>% format("%d. %b")
  animal_info$release_dateyear <- animal_info$release_date_time %>% as.Date("%Y%m%d", tz = "UTC")
  animal_info$release_time <- animal_info$release_date_time %>% format("%H:%M")
  animal_info$recapture_dateyear <- animal_info$recapture_date_time %>% format("%F")
  animal_info$recapture_time <- animal_info$recapture_date_time %>% format("%H:%M")
  
  # make column with yes no if the tag was detected at all by an acoustic receiver
  animal_info$detected <- ifelse(animal_info$tag_serial_number %in% animal_detections$tag_serial_number, "yes", "no")

  # output
  return(animal_info)
}


# get EMODNET tiles into leaflet map ####

# base_map <- function(){
  # emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/inspire_quad/{z}/{x}/{y}.png"
  # emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer_land/inspire_quad/{z}/{x}/{y}.png"
  
  # # Assertions
  # z=1;x=1;y=1
  # url_test <- glue::glue(emodnet_tiles)
  # url_not_up <- httr2::request(url_test) %>%
  #   httr2::req_method("HEAD") %>%
  #   httr2::req_user_agent(mr_user_agent) %>%
  #   httr2::req_perform() %>%
  #   httr2::resp_is_error()
  # 
  # if(url_not_up){
  #   cli::cli_abort(c(
  #     "x" = "Connection to {.url {url_test}} failed",
  #     "i" = "Check status of {.url https://portal.emodnet-bathymetry.eu/}"
  #   ))
  # }
  # 
  # Add HTML class to citation
  # cite_emodnet <- "<a href='https://emodnet.ec.europa.eu'>EMODnet</a>"
  # attr(cite_emodnet, "class") <- c("html", "character")
  # 
  # Perform
  # base_map <- leaflet::leaflet(
  #   options = leaflet::leafletOptions(crs = leaflet::leafletCRS("L.CRS.EPSG4326"))
  # ) %>%
  #   leaflet::addTiles(
  #     urlTemplate = emodnet_tiles,
  #     # options = leaflet::tileOptions(tms = FALSE),
  #     attribution = cite_emodnet
  #   )
  
  # return(base_map)
# }
# 
# base_map %>% 
#   setView(3.4, 51.48, zoom = 8) %>%
#   addCircleMarkers(data = close_stations,
#                    lng = ~deploy_longitude,
#                    lat = ~deploy_latitude,
#                    radius = 3,
#                    fillColor = "black",
#                    weight = 0, 
#                    fillOpacity = 1)



# save data as.rds ####
# folder needs to have a '/' as last character

save_data <- function(data, folder){
  base::saveRDS(data, file = paste0(folder, deparse(substitute(data)), ".rds"))
}

# load rds data ####

load_data <- function(filestring, folder){
  data <- base::readRDS(file = paste0(folder, filestring, ".rds"))
  return(data)
}

# crop geometries
crop_geom <- function(geom, bbox){
  cropped_geom <- sf::st_crop(geom, bbox)
  return(cropped_geom)
}
bbox_geom <- c(xmin = -1.7, ymin = 49.5, xmax = 6, ymax = 54)