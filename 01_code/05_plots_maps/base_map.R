# Script to make a base map with leaflet in the BPNS and scheldt

library(leaflet.extras)
library(leafem)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
path_envdata <- paste0(dir_path, "/03_data/environmental_layers/")
path_boundaries <- paste0(dir_path, "/03_data/marine_boundaries/")
source(paste0(dir_path, "/functions.R"))
# source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_environmental_data.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_human_activities.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_marine_boundaries.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_bathy.R"))
# crop bathy belgium to 3.455, 3.532,51.383, 51.485
bathy_belgium <- bathy_belgium %>% filter(dplyr::between(latitude, 51.343, 51.485) & dplyr::between(longitude, 3.455, 3.77))


col_scale_areas <- c("grey","#E67D1F", "#EFC000")
# for emodnet layer
emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
cite_emodnet <- "<a href='https://emodnet.ec.europa.eu'>EMODnet</a>"
attr(cite_emodnet, "class") <- c("html", "character")

base_map <- leaflet::leaflet(
  # options = leaflet::leafletOptions(crs = leaflet::leafletCRS("L.CRS.EPSG4326"))
) %>%
  # ADD BASELAYERS ####
addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  addRasterImage(bathy_belgium_raster, opacity = 1, colors = "Spectral", group = "bathymetry") %>%
  addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "grey", weight = 0, fillOpacity = 0.7, group = "bathymetry") %>% #"#ECE4BF"
  addTiles(group = "OpenStreetMap") %>%
  # SCALEBAR ####
addScaleBar(position = "topright", options = scaleBarOptions(maxWidth = 250, imperial = F)) %>%
  setView(3.3, 51.5, zoom = 8) %>% 
  #### BPNS ####
addPolygons(data = BPNS, color = col_scale_areas[1], 
            weight = 2,
            opacity = 1.0,
            fillOpacity = 0.2, 
            group = "Regions") %>%
  #### area rectangles ####
addRectangles( #WS1
  lng1 = 3.387, lat1 = 51.365, lng2 = 3.577, lat2 = 51.513,
  fillOpacity = 0.2, weight = 2, color = col_scale_areas[2],
  group = "areas") %>%
  addRectangles( #WS2
    lng1 = 3.656, lat1 = 51.314, lng2 = 3.859, lat2 = 51.447,
    fillOpacity = 0.2, weight = 2, color = col_scale_areas[3],
    group = "areas") %>%
  #### submarine cables ####
addPolylines(data = cables,
             color = "blue",
             opacity = 0.3,
             label = ~name,
             group = "wrecks, OWFs, cables") %>%
  #### more bathy ####
addCircleMarkers(data = bathy_belgium, lat = ~latitude, lng = ~longitude,
                 opacity = 0, fillOpacity = 0,
                 label = ~paste0(depth_m %>% round(digits = 2), " m")) %>%
  #### windfarms ####
addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
            color = "red",
            weight = 2,
            opacity = 1,
            fillOpacity = 0.3,
            label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
            group = "wrecks, OWFs, cables") %>%
  #### shipwrecks ####
addCircleMarkers(data = wrecks,
                 fillColor = "lightgreen",
                 opacity = 0,
                 fillOpacity = 1,
                 radius = 3,
                 label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
                 group = "wrecks, OWFs, cables") %>%
  addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
                   lng = ~longitude,
                   lat = ~latitude,
                   fillColor = "lightgreen",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 3,
                   label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
                   group = "wrecks, OWFs, cables") %>%
  #### ADD RELEASED TAGS ####
addCircleMarkers(data=masterias_info,
                 lat = ~release_latitude,
                 lng = ~release_longitude,
                 weight= 0,# increase if black circle wanted
                 color = "black",
                 fillOpacity = 0.5,
                 radius = 6,
                 fillColor = "grey",
                 label = ~paste0("tag ", str_trunc(tag_serial_number, 3, "left", ellipsis = ""), " (release: ", release_dateyear, "), ", sex),
                 group = "<span style=color:grey>released tags</span>") %>%
  #### ADD STATIONS ####
addCircleMarkers(data = close_stations,
                 lat = ~deploy_latitude,
                 lng = ~deploy_longitude,
                 fillColor = "black",
                 color = "white",
                 weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
                 radius = 3,
                 opacity = 1,
                 fillOpacity = 0.8,
                 label = ~station_name,
                 # highlightOptions = highlightOptions(bringToFront = TRUE),
                 group = "stations") %>%
  # ADD CONTROLS #####
leafem::addMouseCoordinates() %>%
  addFullscreenControl() %>%
  addLayersControl(position = "topright",
                   baseGroups = c("EMODnet bathymetry", "satellite", "bathymetry", "OpenStreetMap"),
                   overlayGroups = c("<span style=color:grey>released tags</span>", "stations", "wrecks, OWFs, cables"),
                   options = layersControlOptions(collapsed = FALSE)) #%>%
  # addLegend(position = "bottomright",
  #           opacity = 0.6,
  #           colors = col_scale_areas,
  #           labels = c("BPNS", "WS1", "WS2"),
  #           title = "Areas",
  #           group = "areas")

base_map
