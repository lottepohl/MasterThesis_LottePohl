# Script to make and save plots of the DST data (depth and temperature)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path_dst <- paste0(dir_path, "/04_analysis_results/dst_overview/depth/")

  # source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_depth_temp_logs.R"))

# ideas: time vector in the background with colors. 
# todo: make ggplot style uniform i.e. make own theme - at a later stage

make_dst_plot <- function(data, time_vector, tag_serial_number_short){
  dst_plot <- ggplot2::ggplot(data = data) + geom_point(aes(x = .data[[time_vector]], y = -depth_m), size = 0.4) + 
    theme_bw(base_size = 12) +
    scale_y_continuous(limits = c(-80, 5), breaks = seq(-70, 0, by = 10)) +
    # scale_x_(n.breaks = 10) +
    # scale_x_datetime(date_breaks = "2 weeks") +
    labs(x = "time", y = "depth in m") #title = paste0("tag ", tag_serial_number_short), 
  
  ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".pdf"), plot = dst_plot, width = 18, height = 12, units = "cm")
  ggplot2::ggsave(filename = paste0(plot_path_dst, "depth_", tag_serial_number_short, ".png"), plot = dst_plot, width = 18, height = 12, units = "cm")
  return(dst_plot)
}

plot_dst_295 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293295"),
                              time_vector = "date_time",
                              tag_serial_number_short = "295")

plot_dst_304 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293304" & t < 300),
                              time_vector = "date_time",
                              tag_serial_number_short = "304")

plot_dst_308 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"),
                              time_vector = "date_time",
                              tag_serial_number_short = "308")

plot_dst_310 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293310"),
                              time_vector = "date_time",
                              tag_serial_number_short = "310")

plot_dst_312 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293312"),
                              time_vector = "date_time",
                              tag_serial_number_short = "312")

plot_dst_319 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293319"),
                              time_vector = "date_time",
                              tag_serial_number_short = "319")

plot_dst_321 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"),
                              time_vector = "date_time",
                              tag_serial_number_short = "321")

plot_dst_322 <- make_dst_plot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293322"),
                              time_vector = "date_time",
                              tag_serial_number_short = "322")

ggplot(data = masterias_depth_temp %>% filter(tag_serial_number== "1293308")) + geom_point(aes(x = t, y = -depth_m))
