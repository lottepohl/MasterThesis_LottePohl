# library(leaflet)
# library(htmltools)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add a legend to the map
# map <- addLegend(map, position = "bottomright", 
#                  title = "Legend Title", 
#                  labels = c("Label 1", "Label 2", "Label 3"),
#                  colors = c("black", "grey", "blue")
#                  ,labFormat = labelFormat(style = list("color" = "red"))
#                  )
#                  
#                  # ,
#                  #                                                       "font-family" = "serif",
#                  #                                                       "font-style" = "italic",
#                  #                                                       "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
#                  #                                                       "font-size" = "12px",
#                  #                                                       "border-color" = "rgba(0,0,0,0.5)"))
# 
# # Change the font of the legend text to serif
# # map$dependencies[[3]]$stylesheet <- list(href = "https://fonts.googleapis.com/css2?family=Merriweather&display=swap")
# 
# # Display the map
# map
# 

# library(leaflet)
# library(htmltools)
# library(htmlwidgets)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Create a CSS dependency
# css_dependency <- htmlDependency(
#   "style", "1.0.0",
#   src = "style.css",
#   stylesheet = TRUE
# )
# 
# # Add the dependency to the map
# map %>% 
#   addProviderTiles("CartoDB.Positron") %>%
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   onRender("
#     function() {
#       $('head').append('<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\" />');
#     }
#   ", deps = list(css_dependency))
# 
# # Display the map
# map

# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add CSS to change the font family of the legend to serif
#   addProviderTiles("CartoDB.Positron") %>% 
#   onRender("
#     function() {
#       var legendLabel = $('.leaflet-control-layers label');
#       legendLabel.css('font-family', 'serif');
#     }
#   ")


# library(leaflet)
# 
# # Create a custom CSS style
# css <- "
#   .leaflet-container .leaflet-control .leaflet-control-layers,
#   .leaflet-control-layers-toggle {
#     font-family: serif;
#   }
# "
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add the custom CSS style to the map
#   addProviderTiles("CartoDB.Positron") %>% 
#   addStyle(css)


# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS style to change the font family of the legend to serif
#   addProviderTiles("CartoDB.Positron") %>% 
#   onRender("
#     function() {
#       var legendLabel = $('.leaflet-control-layers label');
#       legendLabel.css('font-family', 'serif');
#     }
#   ")


# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to change the font family of the legend to serif
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style2.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
#   ")

# 
# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to change the font family of the legend to serif
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
#   ")

# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to modify the legend border
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
#   ")


# library(leaflet)
# library(htmltools)
# 
# # example from ?leaflet
# m = leaflet() %>% addTiles()
# 
# m <- m %>% 
#     addMarkers(lng = -122.4194, lat = 37.7749) %>%
#     addLegend("bottomright",
#               title = "My Legend",
#               colors = "red",
#               labels = "My Label",
#               opacity = 1)
# 
# #  2.  you can add dependencies to your leaflet map
# #  this mechanism will smartly handle duplicates
# #  but carries a little more overhead
# str(m$dependencies)  # should be null to start
# # 
# m$dependencies <- list(
#   htmlDependency(
#     name = "font-awesome"
#     ,version = "4.3.0"
#     # if local file use file instead of href below
#     #  with an absolute path
#     , src = "style.css"
#     # ,src = c(file="C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/01_code/x_experimental/style.css")
#     # ,src = c(href="http://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css")
#     # ,stylesheet = "font-awesome.min.css"
#   )
# )
# 
# m

# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to modify the legend font weight
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
#   ")

# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to modify the legend font weight
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
#   ")
# 
# library(leaflet)
# 
# # Create a leaflet map
# map <- leaflet() %>% addTiles()
# 
# # Add markers and legend
# map %>% 
#   addMarkers(lng = -122.4194, lat = 37.7749) %>%
#   addLegend("bottomright", 
#             title = "My Legend", 
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add a custom CSS file to modify the legend font weight
#   addProviderTiles("CartoDB.Positron") %>% 
#   htmlwidgets::onRender("
#     function(el, x) {
#       var headID = document.getElementsByTagName('head')[0];
#       var cssNode = document.createElement('link');
#       cssNode.type = 'text/css';
#       cssNode.rel = 'stylesheet';
#       cssNode.href = 'style.css';
#       cssNode.media = 'screen';
#       headID.appendChild(cssNode);
#     }
# #   ")
# 
# library(leaflet)
# library(htmltools)
# library(htmlwidgets)
# 
# # create sample data
# data <- data.frame(
#   lat = c(45.5231, 37.7749, 40.7128),
#   lng = c(-122.6765, -122.4194, -74.0060),
#   name = c("San Francisco", "San Francisco", "New York City")
# )
# 
# # create map
# m <- leaflet(data) %>%
#   addTiles() %>%
#   addMarkers() %>%
#   addLegend(
#     html = HTML('<span style="font-family: serif;">My Label</span>'),
#     position = "bottomright",
#     title = "test",
#     labels = "<span style=color:#FF0000>April</span>",
#     colors = "black",
#     opacity = 1
#   ) 
# 
# # display map
# m


m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)

# Add a text label to the map with serif font
m %>% addControl(
  html = HTML('<span style="font-family: serif;">My Label</span>'),
  position = "topright"
)

# 
# library(leaflet)
# 
# # Create leaflet map
# m <- leaflet() %>% addTiles()
# 
# # Create HTML content for legend
# html_legend <- paste("<div>",
#                      "<h4 style='font-family: serif;'>Legend</h4>",
#                      "<p style='font-family: serif;'>My Label</p>",
#                      "</div>")
# 
# # Add legend to map
# m %>% addLegend(position = "bottomright", 
#                 html = HTML(html_legend))

# library(leaflet)
# library(htmlwidgets)
# 
# # Create a leaflet map with a legend
# leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R") %>%
#   addLegend(position = "bottomright",
#             title = "My Legend",
#             colors = "red",
#             labels = "My Label",
#             opacity = 1) %>%
#   # Add custom CSS to change font family to serif
#   onRender("
#            function(map) {
#            var legend = $('.leaflet-control legend')[0];
#            $(legend).css('font-family', 'serif');
#            }
#            ")
# 
# library(leaflet)
# 
# # Create some example data
# data(quakes)
# 
# # Define the CSS for the legend
# legend_css <- "
#   .leaflet-control legend {
#     font-family: serif;
#     font-weight: bold;
#   }
# "
# 
# # Create the map and embed the CSS
# leaflet(quakes) %>%
#   addTiles() %>%
#   addMarkers() %>%
#   addLegend(position = "bottomright", 
#             title = "My Title", 
#             labels = c("My Label"),
#             opacity = 1, 
#             style = list(legend_css))
# 

# library(leaflet)
# 
# # Create the leaflet map
# m <- leaflet() %>%
#   addTiles()
# 
# # Define the CSS style for the legend
# legend_css <- "
# .custom-legend {
#   background-color: #fff;
#   padding: 10px;
#   font-size: 14px;
#   line-height: 18px;
#   color: #555;
#   border-radius: 6px;
#   box-shadow: 0 0 15px rgba(0,0,0,0.2);
#   border: 1px solid #ccc;
# }
# 
# .custom-legend i {
#   width: 18px;
#   height: 18px;
#   float: left;
#   margin-right: 8px;
#   opacity: 0.7;
# }"
# 
# # Add a custom control (which will look like a legend)
# m <- m %>% addControl(
#   html = paste0('<div class="custom-legend">',
#                 '<div><i style="background:#E31A1C"></i> My Label</div>',
#                 '</div>'),
#   position = "bottomright"
# )
# 
# # Add the CSS style to the map
# m <- m %>% 
#   addProviderTiles("OpenStreetMap.Mapnik") %>%
#   addTiles() %>%
#   addStyle(legend_css)
# 
# # Print the map
# m


library(leaflet)

# Create a leaflet map
map <- leaflet() %>% addTiles()

# Define custom legend labels
labels <- c("My Label")

# Define custom CSS style for the legend label
legend_css <- "font-family: serif" #; font-weight: bold;"

# Create the legend
legend <- paste0("<span style='", legend_css, "'>", labels, "</span>")

# Add the legend as a control
map %>% addControl(html = legend, position = "bottomright"#, title = "My Title"
                   )

# -------------------------------------

# library(leaflet)
# 
# # Create a leaflet map
# leaflet() %>%
#   addTiles() %>%
#   
#   # Add a legend control to the top right of the map
#   addControl(
#     position = "topright",
#     title = "My Legend",
#     label = "My Label",
#     html = "<span style='font-family: serif; font-weight: bold;'>My Label</span>"
#   )
# 
# library(leaflet)
# library(leaflet.extras)
# 
# # Create a leaflet map
# leaflet() %>%
#   addTiles() %>%
#   
#   # Add a black/white "zebra" raster indicating the lat and lon values
#   addGraticule(
#     showLabel = TRUE,
#     dashArray = 2,
#     labelFormat = labelFormat(
#       style = list(color = "black", fontweight = "bold", "font-family" = "serif"),
#       decimals = 2
#     )
#   ) %>%
#   
#   # Set the bounds of the map so the lat/lon values appear outside of the frame
#   fitBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)
# 
# 
# # Define HTML for the legend
# html_legend <- '<div id="map-legend" class="leaflet-control"><div class="legend-title">My Title</div><div class="legend-labels"><span style="font-family: serif; font-weight: bold;">My Label</span></div></div>'
# 
# # Add the HTML to the map as a control
# m %>% 
#   addControl(html = html_legend, position = "topright")
# 
# # Add the CSS style to the head of the HTML document
# m %>% 
#   htmlwidgets::onRender("
#     function() {
#       var legend_css = '.legend-title { font-family: serif; font-size: 16px; font-weight: bold; margin-bottom: 5px; } .legend-labels { font-family: serif; font-size: 14px; }';
#       var style = document.createElement('style');
#       if (style.styleSheet) {
#         style.styleSheet.cssText = legend_css;
#       } else {
#         style.appendChild(document.createTextNode(legend_css));
#       }
#       document.getElementsByTagName('head')[0].appendChild(style);
#     }
#   ")



library(leaflet)

# create leaflet map
m <- leaflet() %>%
  setView(lng = -122.43, lat = 37.77, zoom = 8) %>%
  # addProviderTiles(providers$Stamen.TonerLite) 
  addProviderTiles("Stamen.Watercolor") 

m
# add graticule
m %>% 
  # addGraticule(interval = 0.5, style = list(color = "grey", weight = 1)) %>%
  addSimpleGraticule(interval = 0.5) %>%
  addMiniMap(position = "topright",
             width = 100,
             height = 100,
             zoomLevelOffset = -3,
             zoomLevelFixed = T,
             tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"#providers$Esri.WorldStreetMap)
  )

# add scale bar
m %>% addScaleBar(options = scaleBarOptions(position = "bottomleft", maxWidth = 100, metric = TRUE))

# add legend outside of map to the right
html_legend <- '<div id="maplegend" class="leaflet-control" style="background-color: rgba(255, 255, 255, 0.8); border-radius: 5px; border: 2px solid #777777; padding: 10px; font-size: 14px; line-height: 18px; font-weight: bold; font-family: serif;">
                  <div class="legend-title">My Legend</div>
                  <hr>
                  <div class="legend-label"><span style="color:#FF0000;">My Label</span></div>
                </div>'

m %>% addControl(html = html_legend, position = "bottomright")


leaflet() %>%
  addTiles() %>%
  addSimpleGraticule()
