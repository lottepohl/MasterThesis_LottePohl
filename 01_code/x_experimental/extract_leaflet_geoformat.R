library(sf)
install.packages("geojsonsf")
library(geojsonsf)

# Create a "COMPOUNDCURVE" geometry using sf
curve1 <- st_linestring(matrix(c(0, 0, 1, 1, 2, 0), ncol = 2))
curve2 <- st_linestring(matrix(c(2, 0, 3, 1, 4, 0), ncol = 2))
compound_curve <- st_multilinestring(list(curve1, curve2))

# Convert the geometry to GeoJSON
geojson_feature <- st_sf(geometry = compound_curve) %>%
  sf_geojson()

# Print the GeoJSON feature
cat(geojson_feature, "\n")

# {
#   "type":"Feature",
#   "geometry":{
#     "type":"MultiLineString",
#     "coordinates":[
#       [[0,0],[1,1],[2,0]],
#       [[2,0],[3,1],[4,0]]
#     ]
#   },
#   "bbox":[0,0,4,1]
# }

library(leaflet)

# Create a Leaflet map
map <- leaflet() %>%
  setView(lng = -0.09, lat = 51.505, zoom = 13)

# Add a tile layer to the map
map <- addTiles(map, urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                attribution = "Map data &copy; OpenStreetMap contributors")

# Add the GeoJSON feature to the map as a layer
map <- addGeoJSON(map, data = geojson_feature)

# Display the map
map
