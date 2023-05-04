# set.seed(526379)                    # Create example data
data <- data.frame(axis1 = paste0("Q", 1:4),
                   axis2 = rep(2021:2025, each = 4),
                   x = 1:20,
                   y = 1:20 + rnorm(20))
data$axis2[duplicated(data$axis2)] <- NA
head(data) 

data <- acf_308_df %>% mutate(dates = xvals) # %>% as.character())
# data$dates %>% class()


ggp <- ggplot(data = data, aes(x = dates, y = acf)) +    # Draw ggplot2 plot with one axis
  geom_line()
ggp

ggp +
  geom_text(aes(x = seq(from = 0, to = nrow(data), by = 50), y = -1, label = seq(from = 0, to = nrow(data), by = 50))) #+
  # geom_text(aes(x = station_name, y = -47.5, label = paste0(n_detect)), angle = 20, family = "serif", size = 3) +

ggp +                               # Draw ggplot2 plot with multiple axes
  # theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_blank()) +
  coord_cartesian(clip = "off") +
  annotate(geom = "text",
           # x = 1:nrow(data),
           x = seq(from = 0, to = nrow(data), by = 50),
           y = -0.5,
           label = seq(from = 0, to = nrow(data), by = 50),
           vjust = 3) #+
  annotate(geom = "text",
           x = 1:nrow(data),
           y = min(data$y),
           label = data$axis2,
           vjust = 5)
