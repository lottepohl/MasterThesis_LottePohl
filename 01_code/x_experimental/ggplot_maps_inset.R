library(ggplot2)
library(sf)
library(rnaturalearth)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" 
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R") %>% base::source()

# # 
# # # Obtain map data for Belgium
# # belgium <- ne_download(category = "admin_0_countries", type = "countries", 
# #                        group = "Countries", name = "Belgium", returnclass = "sf")
# # 
# # # Obtain map data for Europe
# # europe <- ne_download(category = "continent", type = "continent", 
# #                       name = "Europe", returnclass = "sf")
# 
# # Create the main map of Belgium
# belgium_map <- ggplot() +
#   geom_sf(data = Belgium) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_void()  # Use a blank background for the main map
# 
# # Create the inset map of Europe
# europe_map <- ggplot() +
#   geom_sf(data = Europe) +
#   coord_sf(crs = st_crs(4326)) +
#   theme_void()  # Use a blank background for the inset map
# 
# # Combine the main map and the inset map
# combined_map <- belgium_map +
#   annotation_custom(grob = ggplotGrob(belgium_map)
#                     # ,xmin = <x_min>, xmax = <x_max>, ymin = <y_min>, ymax = <y_max>
#                       ) +
#   theme(plot.margin = margin(1, 1, 1, 1, "cm"))  # Adjust the margin as needed
# 
# # Display the combined map
# print(combined_map)

#### load packages ####

library(ggplot2)
library(dplyr)

############################################################################
###### map synthesis lakes ("primary data") at pooled station level ########
########## and location of lakes in literature review ######################
############################################################################

##################### data processing ###########################

# tot_pts <- read.csv("tot_pts.csv", stringsAsFactors = FALSE)

##################### mapping ###################################

# Create the main map of Belgium
belgium_map <- ggplot() +
  geom_sf(data = Belgium) +
  coord_sf(crs = st_crs(4326)) #+
  # theme_void()  # Use a blank background for the main map

# Create the main map of Belgium
europe_map <- ggplot() +
  geom_sf(data = Europe) +
  coord_sf(crs = st_crs(4326)) +
  theme_void() +  # Use a blank background for the main map
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.background = element_rect(color = NA, fill = "white"))

europe_map_grob <- ggplot2::ggplotGrob(europe_map)

#add zoomed in Europe as inset and zoomed in US midwest
map_belgium_inset <- belgium_map + 
  #Europe zoom
  annotation_custom(grob=europe_map_grob, xmin = 5.5, xmax = Inf, ymin = -Inf, ymax=50) #+
  # #US midwest zoom
  # annotation_custom(grob=map.us.grob, xmin = -180, xmax = -105, ymin = -40, ymax=10)

map_belgium_inset

#### BASE MAP - continent outlines ####

#get world data
world.data <- map_data("world")

map.grey <- ggplot(data=world.data, aes(long, lat, group = group)) + 
  #map continents
  geom_polygon(col = "grey80", fill = "grey80") +
  #lakes from compiled data set and literature data set
  # geom_point(data = tot_pts, 
  #            aes(x = stationlong, y = stationlat, group=1, 
  #                shape = factor(from), color = factor(from), na.rm = TRUE),
  #            size = 2.1) +
  scale_color_manual(name = "Lake source",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c("#F8766D", "#00BFC4")) +
  scale_shape_manual(name = "Lake source",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c(19, 17)) + 
  ylab("Latitude") +
  xlab("Longitude") +
  #labs(title = "Lake Locations") +
  #sets lat/long breaks, expands to fill (e.g.)
  scale_x_continuous(breaks = seq(-180, 180, by = 30), expand=c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30), expand=c(0.02, 0.02)) +
  coord_fixed() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 9),
        legend.position = c(0.9, 0.6),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 16))


#### EUROPE MAP - country outlines/borders ####

map.europe <- ggplot(data=world.data, aes(x=long, y=lat, group=group)) + 
  #map continents - with all country borders
  geom_polygon(fill = "grey80", color = "grey30", alpha = 0.6, linewidth = 0.3) +
  #lakes from compiled data set and literature data set
  # geom_point(data = tot_pts, 
  #            aes(x = stationlong, y = stationlat, group=1, 
  #                shape = factor(from), color = factor(from), na.rm = TRUE),
  #            size = 1.4) +
  scale_color_manual(name = "Lake from",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c("#F8766D", "#00BFC4")) +
  scale_shape_manual(name = "Lake from",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c(19, 17)) + 
  #zoom in on Europe
  coord_fixed(xlim = c(-5, 27), ylim = c(40, 70)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.4),
  )

#make it a grob for later inset
map.europe.grob <- ggplotGrob(map.europe)


#### US MIDWEST map - state borders ####

#create map to get gaps (empty) showing outline of Great Lakes
world.usa <- filter(world.data, region == "USA")
world.canada <- filter(world.data, region == "Canada")

nor_am <- ggplot() +
  geom_polygon(data=world.usa, aes(long, lat, group=group), fill = "grey80") +
  geom_polygon(data=world.canada, aes(long, lat, group=group), fill = "grey80") +
  coord_fixed(xlim = c(-130, -60), ylim = c(30, 60)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(linetype = "solid", color = "black", fill = NA),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 18))

#zoom to US Midwest - state borders
states.data <- map_data("state")

map.us <- nor_am + 
  geom_polygon(data = states.data, aes(x=long, y = lat, group = group), 
               fill = NA, color = "grey30", alpha = 0.6, size = 0.3) + 
  #lakes from compiled and literature data sets
  # geom_point(data = tot_pts, 
  #            aes(x = stationlong, y = stationlat, group=1, 
  #                shape = factor(from), color = factor(from), na.rm = TRUE),
  #            size = 1.4) +
  scale_color_manual(name = "Lake from",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c("#F8766D", "#00BFC4")) +
  scale_shape_manual(name = "Lake from",
                     breaks = c("compiled", "lit"),
                     labels = c("Primary data", "Published literature"),
                     values = c(19, 17)) + 
  coord_fixed(xlim = c(-100, -76), ylim = c(37, 53)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect(linetype = "dashed", color = "grey20", fill = NA, size = 0.3),
  )

#make it a grob for later inset
map.us.grob <- ggplotGrob(map.us)


#### Bounding polygons ####

#making a bounding polygon that shows where the zoomed in inset is coming from - US Midwest
long.us <- c(-100, -76, -76, -100)
lat.us <- c(53, 53, 37, 37)
group <- c(1, 1, 1, 1)
latlong.us <- data.frame(long.us, lat.us, group)


#making a bounding polygon that shows where the zoomed in inset is coming from - Europe
long.eur <- c(-5, 27, 27, -5)
lat.eur <- c(70, 70, 40, 40)
group <- c(1, 1, 1, 1)
latlong.eur <- data.frame(long.eur, lat.eur, group)


#add bounding polygons showing where zoomed in maps are coming form
map.bound <- map.grey + 
  #bounding polygon for Europe
  geom_polygon(data=latlong.eur, aes(long.eur, lat.eur, group=group), fill = NA, color = "grey20", 
               linetype = "dashed", size = 0.3, alpha = 0.8) +
  #bounding polygon for US Midwest
  geom_polygon(data=latlong.us, aes(long.us, lat.us, group=group), fill = NA, color = "grey20", 
               linetype = "dashed", size = 0.3, alpha = 0.8)


#### Insets ####

#add zoomed in Europe as inset and zoomed in US midwest
map.bound.inset <- map.bound + 
  #Europe zoom
  annotation_custom(grob=map.europe.grob, xmin = 40, xmax = 100, ymin = -62, ymax=-6) +
  #US midwest zoom
  annotation_custom(grob=map.us.grob, xmin = -180, xmax = -105, ymin = -40, ymax=10)

map.bound.inset


#### Connecting lines ####

#lines connecting Europe pop out box (first two) and US (last two)

map.full <-  map.bound.inset +
  #left line - Europe
  geom_segment(aes(x=-5, xend=41, y=40, yend=-6), 
               linetype = "dashed", color = "grey20", size = 0.3) +
  #right line - Europe
  geom_segment(aes(x=27, xend=98, y=40, yend=-5), 
               linetype = "dashed", color = "grey20", size = 0.3) +
  #left line - US
  geom_segment(aes(x=-101, xend=-178.5, y=36, yend=9.5), 
               linetype = "dashed", color = "grey20", size = 0.3) +
  #right line - US
  geom_segment(aes(x=-76, xend=-106, y=35.5, yend=9), 
               linetype = "dashed", color = "grey20", size = 0.3)

#final map
plot(map.full)
