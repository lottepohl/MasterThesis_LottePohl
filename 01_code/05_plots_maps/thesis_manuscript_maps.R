# Script to make maps for the thesis manuscript

library(leaflet)
library(leaflet.extras)
library(leafem)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_envdata <- paste0(dir_path, "/00_data/environmental_layers/")
path_boundaries <- paste0(dir_path, "/00_data/marine_boundaries/")
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
# source(paste0(dir_path, "/functions.R"))
paste0(dir_path, "/01_code/06_functions/functions.R") %>% source()
# source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_environmental_data.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_human_activities.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_bathy.R"))
# crop bathy belgium to 3.455, 3.532,51.383, 51.485
bathy_belgium <- bathy_belgium %>% filter(dplyr::between(latitude, 51.343, 51.485) & dplyr::between(longitude, 3.455, 3.77))

labels_latlng <- tibble(name = c("North Sea", "Hurd deep", "English Channel", "France", "Belgium"),
                        lat = c(51.92, Hurd_deep$latitude, 50.5, 49.8, 50.86),
                        lng = c(3.75, Hurd_deep$longitude, -1.6, 2.7, 3.84))

outlines <- rbind(Belgium, Netherlands, North_sea, English_channel)

col_scale_areas <- c("grey","#E67D1F", "#EFC000")
# for emodnet layer
emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
cite_emodnet <- "<a href='https://emodnet.ec.europa.eu'>EMODnet</a>"
attr(cite_emodnet, "class") <- c("html", "character")

# 1. Overview Map ####
map1_overview <- leaflet::leaflet(
  options = leafletOptions(zoomControl = FALSE,
                           minZoom = 7, maxZoom = 7,
                           dragging = FALSE
                           )
) %>%
  # ADD BASELAYERS #
# addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  # addRasterImage(bathy_belgium_raster, opacity = 1, colors = "Spectral", group = "bathymetry") %>%
  # addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "grey", weight = 0, fillOpacity = 0.7, group = "bathymetry") %>% #"#ECE4BF"
  # addTiles(group = "OpenStreetMap") %>%
  # BPNS #
addPolygons(data = BPNS, color = "grey",
            weight = 2,
            opacity = 0,
            fillOpacity = 0.2) %>%
  # area rectangles #
# addRectangles( #WS1
#   lng1 = 3.387, lat1 = 51.365, lng2 = 3.577, lat2 = 51.513,
#   fillOpacity = 0.2, weight = 2, color = col_scale_areas[2],
#   group = "areas") %>%
#   addRectangles( #WS2
#     lng1 = 3.656, lat1 = 51.314, lng2 = 3.859, lat2 = 51.447,
#     fillOpacity = 0.2, weight = 2, color = col_scale_areas[3],
#     group = "areas") %>%
  # submarine cables #
addPolylines(data = cables,
             color = "blue",
             weight = 1,
             opacity = 0.6,
             label = ~name,
             group = "wrecks, OWFs, cables") %>%
  # shipwrecks #
addCircleMarkers(data = wrecks,
                 fillColor = "green",
                 opacity = 0,
                 fillOpacity = 1,
                 radius = 0.9,
                 label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
                 group = "wrecks, OWFs, cables") %>%
  addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
                   lng = ~longitude,
                   lat = ~latitude,
                   fillColor = "green",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 0.9,
                   label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
                   group = "wrecks, OWFs, cables") %>%
  # more bathy #
# addCircleMarkers(data = bathy_belgium, lat = ~latitude, lng = ~longitude,
#                  opacity = 0, fillOpacity = 0,
#                  label = ~paste0(depth_m %>% round(digits = 2), " m")) %>%
  # windfarms #
addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
            color = "red",
            weight = 1,
            opacity = 1,
            fillOpacity = 0.3,
            label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
            group = "wrecks, OWFs, cables") %>%
  addPolygons(data = BPNS, color = "darkgrey",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  # ADD RELEASED TAGS #
# addCircleMarkers(data=masterias_info,
#                  lat = ~release_latitude,
#                  lng = ~release_longitude,
#                  weight= 0,# increase if black circle wanted
#                  color = "black",
#                  fillOpacity = 0.5,
#                  radius = 6,
#                  fillColor = "grey",
#                  label = ~paste0("tag ", str_trunc(tag_serial_number, 3, "left", ellipsis = ""), " (release: ", release_dateyear, "), ", sex),
#                  group = "<span style=color:grey>released tags</span>") %>%
  # ADD STATIONS #
# addCircleMarkers(data = close_stations,
#                  lat = ~deploy_latitude,
#                  lng = ~deploy_longitude,
#                  fillColor = "black",
#                  color = "white",
#                  weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
#                  radius = 3,
#                  opacity = 1,
#                  fillOpacity = 0.8,
#                  label = ~station_name,
#                  # highlightOptions = highlightOptions(bringToFront = TRUE),
#                  group = "stations") %>%
 # GRATICULE #
  addSimpleGraticule(interval = 1) %>%
  # addMiniMap(position = "bottomright",
  #            width = 100,
  #            height = 100,
  #            zoomLevelOffset = -3,
  #            zoomLevelFixed = T,
  #            tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"#providers$Esri.WorldStreetMap)
  # ) %>%
  # OUTLINES BOUNDARIES #
  addPolygons(data = outlines, color = "black",
            weight = 1,
            opacity = 1.0,
            # label = "North Sea",
            fillOpacity = 0,
            labelOptions = labelOptions(noHide = T, textOnly = F, offset = c(-170, 500), permanent = T)) %>% #
  addPolygons(data = Schelde_boundaries, color = "yellow",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  addCircleMarkers(data = labels_latlng,
                   lat = ~lat,
                   lng = ~lng,
                   weight = 2,
                   opacity = 0,
                   fillOpacity = 0,
                   label = ~name,
                   labelOptions = labelOptions(noHide = T, 
                                               textOnly = T,
                                               style = list("font-style" = "bold",
                                                            "font-weight" = "bold",
                                                            "font-size" = "12px"))) %>%
  # ADD CONTROLS #
# leafem::addMouseCoordinates() %>%
  setView(1.35, 50.75, zoom = 6.5) %>%
#   addFullscreenControl() %>%
  # addLayersControl(position = "topright",
  #                  baseGroups = c("EMODnet bathymetry", "satellite", "bathymetry", "OpenStreetMap"),
  #                  overlayGroups = c("<span style=color:grey>released tags</span>", "stations", "wrecks, OWFs, cables"),
  #                  options = layersControlOptions(collapsed = FALSE)) #%>%
addLegend(position = "bottomright",
          opacity = 1,
          colors = c("darkgrey", "yellow", "red", "blue", "green"),
          labels = c("BPNS", "Scheldt estuary", "Windfarms", "Submarine cables", "Shipwrecks"),
          # title = "Legend",
          labFormat = labelFormat(textOnly = T)) %>%
  # SCALEBAR #
addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 150, imperial = F))
  

map1_overview

# 2. Detail Map ####

tagging_info <- masterias_info %>% mutate(date = release_date_time %>% lubridate::date(),
                                          release_loc = ifelse(release_latitude > 51.53, "Neeltje Jans", "Westerschelde")) %>% 
  group_by(release_loc) %>% reframe(lat = release_latitude %>% median(),
                                    lng = release_longitude %>% median()) %>%
  mutate(label = c(1,2))


labels_latlng_detail <- tibble(name = c("North Sea", "BPNS", "Scheldt Estuary", "Netherlands", "Belgium"),
                        lat = c(55.92, 51.62, 51.5, 51.72, 51.1),
                        lng = c(3.75, 2.55, -1.6, 4.22, 3.9))

labels_release <- tibble(name = c("Neeltje Jans", "Western Scheldt"),
                         lat = c(51.65, 51.47),
                         lng = c(3.664, 3.39),
                         label = c("1", "2"))

map2_detail <- leaflet::leaflet(
  options = leafletOptions(zoomControl = FALSE,
                           # minZoom = 7, maxZoom = 7,
                           dragging = FALSE)) %>%
  # ADD BASELAYERS #
  # addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  # addRasterImage(bathy_belgium_raster, opacity = 1, colors = "Spectral", group = "bathymetry") %>%
  # addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "grey", weight = 0, fillOpacity = 0.7, group = "bathymetry") %>% #"#ECE4BF"
  # addTiles(group = "OpenStreetMap") %>%
  # BPNS #
  addPolygons(data = BPNS, color = "grey",
              weight = 2,
              opacity = 0,
              fillOpacity = 0) %>%
  # area rectangles #
  # addRectangles( #WS1
  #   lng1 = 3.387, lat1 = 51.365, lng2 = 3.577, lat2 = 51.513,
  #   fillOpacity = 0.2, weight = 2, color = col_scale_areas[2],
  #   group = "areas") %>%
  #   addRectangles( #WS2
  #     lng1 = 3.656, lat1 = 51.314, lng2 = 3.859, lat2 = 51.447,
  #     fillOpacity = 0.2, weight = 2, color = col_scale_areas[3],
  #     group = "areas") %>%
  # submarine cables #
  addPolylines(data = cables,
               color = "blue",
               weight = 1,
               opacity = 0.6,
               label = ~name,
               group = "wrecks, OWFs, cables") %>%
  # shipwrecks #
  addCircleMarkers(data = wrecks,
                   fillColor = "green",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 1.25,
                   label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
                   group = "wrecks") %>%
  addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
                   lng = ~longitude,
                   lat = ~latitude,
                   fillColor = "green",
                   opacity = 0,
                   fillOpacity = 1,
                   radius = 1.25,
                   label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
                   group = "wrecks") %>%
  # more bathy #
  # addCircleMarkers(data = bathy_belgium, lat = ~latitude, lng = ~longitude,
  #                  opacity = 0, fillOpacity = 0,
  #                  label = ~paste0(depth_m %>% round(digits = 2), " m")) %>%
  # windfarms #
  addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
              color = "red",
              weight = 1,
              opacity = 1,
              fillOpacity = 0.15,
              label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
              group = "wrecks, OWFs, cables") %>%
  addPolygons(data = BPNS, color = "#00000075",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  # ADD RELEASED TAGS #
  addCircleMarkers(data=tagging_info,
                   lat = ~lat,
                   lng = ~lng,
                   weight= 1,# increase if black circle wanted
                   color = "violet",
                   fillOpacity = 1,
                   radius = 6,
                   # fillColor = "violet",
                   # label = ~label,
                   labelOptions = labelOptions(noHide = T, textOnly = T, textsize = "12px", offset = c(0,-5))) %>%
# ADD STATIONS #
addCircleMarkers(data = close_stations,
                 lat = ~deploy_latitude,
                 lng = ~deploy_longitude,
                 fillColor = "orange",
                 color = "black",
                 weight = 0.3, #ifelse(close_stations$Array == "offshore", 1.5, 0),
                 radius = 4,
                 opacity = 1,
                 fillOpacity = 0.8,
                 label = ~station_name,
                 # highlightOptions = highlightOptions(bringToFront = TRUE),
                 group = "stations") %>%
# GRATICULE #
addSimpleGraticule(interval = 0.5) %>%
  addMiniMap(position = "bottomright",
             width = 100,
             height = 100,
             zoomLevelOffset = -5,
             zoomLevelFixed = T,
             tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png", #providers$Esri.WorldStreetMap)
             aimingRectOptions = list(color = "darkgrey", weight = 1, clickable = FALSE),
             ) %>%
  # OUTLINES BOUNDARIES #
  addPolygons(data = outlines %>% filter(preferredGazetteerName %in% c("Belgium", "Netherlands")), color = "black", 
              weight = 1,
              opacity = 1.0,
              # label = "North Sea",
              fillOpacity = 0,
              labelOptions = labelOptions(noHide = T, textOnly = F, offset = c(-170, 500), permanent = T)) %>% #
  addPolygons(data = Schelde_boundaries, color = "yellow",
              weight = 2,
              opacity = 1.0,
              fillOpacity = 0) %>%
  addCircleMarkers(data = labels_latlng_detail,
                   lat = ~lat,
                   lng = ~lng,
                   weight = 2,
                   opacity = 0,
                   fillOpacity = 0,
                   label = ~name,
                   labelOptions = labelOptions(noHide = T, 
                                               textOnly = T,
                                               style = list("font-style" = "bold",
                                                            "font-weight" = "bold",
                                                            "font-size" = "12px"))) %>%
  addCircleMarkers(data = labels_release,
                   lat = ~lat,
                   lng = ~lng,
                   weight = 2,
                   opacity = 0,
                   fillOpacity = 0,
                   label = ~label,
                   labelOptions = labelOptions(noHide = T, 
                                               textOnly = T,
                                               style = list("font-style" = "bold",
                                                            # "font-weight" = "bold",
                                                            "font-size" = "12px"))) %>%
  # ADD CONTROLS #
  # leafem::addMouseCoordinates() %>%
  setView(3.5, 51.35, zoom = 8) %>%
  # setView(3.5, 51.35, zoom = 9) %>%
  addLegend(position = "bottomleft",
            opacity = 1,
            colors = c("orange", "violet", "red", "blue"), #, "green",
            labels = c("Receiver stations", "Release stations", "Windfarms", "Submarine cables"), #, "Shipwrecks"
            # title = "Legend",
            labFormat = labelFormat(textOnly = T)) %>%
  # SCALEBAR #
  addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 150, imperial = F)) %>%
  hideGroup("wrecks")

map2_detail

# save data #####
save_data(data = map1_overview, folder = path_maps)
save_data(data = map2_detail, folder = path_maps)
