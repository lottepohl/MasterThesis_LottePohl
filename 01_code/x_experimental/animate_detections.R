# library(ggplot2)
# library(gganimate)
# library(nycflights13)
# 
# # filter flights departing from JFK in January 2013
# flights_jfk_jan <- filter(flights, origin == "JFK", month == 1)
# 
# # create a map of the United States
# usa_map <- map_data("state")
# 
# # add the initial position of the flights
# p <- ggplot() +
#   geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "gray", color = "white") +
#   geom_point(data = flights_jfk_jan[1, ], aes(x = dest_lon, y = dest_lat), color = "red", size = 3) +
#   xlim(range(usa_map$long)) + ylim(range(usa_map$lat)) +
#   theme_void()
# 
# # animate the flights departing from JFK
# p_anim <- p +
#   transition_time(time_hour) +
#   shadow_mark(size = 3, alpha = 0.5)
# 
# animate(p_anim, fps = 10, duration = 10)
set.seed(123) # for reproducibility
n_points <- 20
max_speed <- 0.05 # in degrees of latitude/longitude per second

# generate random initial positions and speeds
points <- data.frame(
  id = 1:n_points,
  lon = runif(n_points, -125, -65),
  lat = runif(n_points, 25, 50),
  speed_lon = rnorm(n_points, 0, max_speed),
  speed_lat = rnorm(n_points, 0, max_speed)
)

# create a map of the United States
usa_map <- map_data("state")

# add the initial positions of the points
p <- ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "gray", color = "white") +
  geom_point(data = points, aes(x = lon, y = lat), color = "red", size = 3) +
  xlim(range(points$lon[is.finite(points$lon) & !is.na(points$lon)])) + ylim(range(points$lat[is.finite(points$lat) & !is.na(points$lat)])) +
  theme_void()

# animate the moving points
p_anim <- p +
  transition_time(1:length(points)) +
  shadow_mark(size = 3, alpha = 0.5)

animate(p_anim, fps = 10, duration = 10)



library(ggplot2)
library(gganimate)

# Create initial data frame with start locations
start_df <- data.frame(
  id = 1:20,
  lon = runif(20, -125, -65),
  lat = runif(20, 25, 50)
)

# Create sequence of data frames for each time step
n_frames <- 10
frames_list <- list(start_df)
for (i in 1:n_frames) {
  prev_df <- frames_list[[i]]
  new_df <- data.frame(
    id = prev_df$id,
    lon = prev_df$lon + rnorm(nrow(prev_df), sd = 0.01),
    lat = prev_df$lat + rnorm(nrow(prev_df), sd = 0.01)
  )
  frames_list[[i+1]] <- new_df
}

# Combine data frames into single data frame
df <- do.call(rbind, frames_list)

# Create base map using ggplot2
map_data <- map_data("state")
p <- ggplot() +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), fill = "gray", color = "white") +
  theme_void()

# Add points and animate
p_anim <- p +
  geom_point(data = df, aes(x = lon, y = lat)) +
  transition_states(states = 1:n_frames, transition_length = 1, state_length = 1)

animate(p_anim, fps = 10, duration = 10)

base_map
