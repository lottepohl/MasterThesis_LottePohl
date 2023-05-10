# Script with plots for the master symposium on May 12, 2023


# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(pracma)
library(sf)
library(leaflet)
library(leafem)
library(leaflet.extras)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
## functions ####
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
source(paste0(dir_path, "/01_code/06_functions/compute_wavelettransform.R"))
## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
source(paste0(dir_path, "/01_code/02_load_data/load_environmental_data.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_human_activities.R"))
source(paste0(dir_path, "/01_code/02_load_data/load_marine_boundaries.R"))
# source(paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R"))
# source(paste0(dir_path, "/01_code/02_load_data/load_bathy.R"))


## set path were all figures are saved ####
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

# Set the theme ####
presentation_theme <- ggplot2::theme(
  plot.title = element_text(size = 11, face = "bold"),
  plot.subtitle = element_text(family = "sans", size = 14),
  axis.title = element_text(family = "sans", size = 14),
  axis.text = element_text(family = "sans", size = 12),
  legend.title = element_text(family = "sans", size = 14),
  legend.text = element_text(family = "sans", size = 12),
  # plot.background = element_blank()#,
  panel.background = element_blank(),
  legend.key = element_rect(fill = "transparent", colour = "transparent"), # Add this line
  # panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_line(color = "gray40", linetype = "solid"),
  panel.grid.minor = element_line(color = "gray60", linetype = "dashed"),
)
ggplot2::theme_set(presentation_theme) # or theme minimal


plot_wavelet <- function(wt_df, type = c("power", "significance", "power_log"),
                         # y_breaks = c(4, 8, 16, 32, 64, 128),
                         # x_breaks = c("000", "100", "200", "300", "400", "500"),
                         date = TRUE){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  
  # transform dates 
  wt_df$date <- wt_df$date %>% as.POSIXct(tz = "UTC")
  # # x axis labels
  # ifelse(date %>% isTRUE(),
  #        x_breaks <- c(wt_df$date[1], wt_df$date[(1/5) * n_data], wt_df$date[(2/5) * n_data], wt_df$date[(3/5) * n_data],
  #                      wt_df$date[(4/5) * n_data], wt_df$date[(5/5) * n_data])
  # ,
  # x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
  # 
  #plot
  ifelse(date %>% isTRUE(),
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = date, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.65) +
                  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_x_datetime(date_breaks = "1 month",
                                   # date_minor_breaks = "1 month",
                                   date_labels = "%b'%y",
                                   expand = c(0,0)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "", y = "Period in days", fill = "log2(Power)") +
                theme(legend.position = "bottom",
                      legend.box = "horizontal")
                # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = significance),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "significance") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       ,
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = date, y = period, fill = power),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = date, y = period, fill = power),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "power") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       
                )
         )
         ,
         
         ifelse(type == "power_log",
                
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = t, y = period, fill = power_log),
                            position = "identity",
                            alpha = 0.65) +
                  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log),
                            position = "identity") +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                  # scale_x_discrete(breaks = x_breaks) +
                  scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "date", y = "period in hours", fill = "log2(power)") #+
                # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                ,
                
                ifelse(type == "significance",
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = t, y = period, fill = significance),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = significance),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "significance") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                       ,
                       
                       plot <- ggplot(data = wt_df) +
                         geom_tile(aes(x = t, y = period, fill = power),
                                   position = "identity",
                                   alpha = 0.65) +
                         geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power),
                                   position = "identity") +
                         scale_y_continuous(trans = my_trans,
                                            breaks = y_breaks, 
                                            expand = c(0,0)) +
                         # scale_x_discrete(breaks = x_breaks) +
                         scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "date", y = "period in hours", fill = "power") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                )
         )
  )
  return(plot)
}

plot_summary_stats <- function(data_depth, tag_serial_num){
  ggplot(data = data_depth %>% ungroup() %>% 
           filter(tag_serial_number == tag_serial_num) %>% 
           mutate(t_days = t_days %>% as.numeric())) +
  geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "daily depth range"), alpha = 0.75) +
    geom_line(aes(x = date, y = -depth_median_sgolay, colour = "daily median depth"), linewidth = 1) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + #angle = 30
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "Depth in m") +  
    scale_colour_manual(name = "", values = c("daily median depth" = "black", "daily depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
                                              "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
    scale_fill_manual(name = "", values = c("daily depth range" = "lightgrey", "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #"range" = "grey", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
    
    theme(legend.position = "bottom",
          legend.box = "horizontal") +
    scale_x_datetime(
      # date_minor_breaks = "2 weeks", # make vector with 15.1., 15.2. to take as minor breaks
      date_breaks = "1 month",
      # date_minor_breaks = "1 month",
      # date_breaks = "2 months",
      date_labels = "%b'%y"
      ,expand = c(0,0)
    )
}


# DST ####
## raw logs ####
### depthlog 
# tag 308
tag308_raw_depth <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"), aes(x = date_time, y = -depth_m)) + 
  # geom_point(aes(color = temp_c), size = 0.5) + 
  geom_line() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Depth in m", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag308_raw_depth

### templog 
tag308_raw_temp <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308"), aes(x = date_time, y = temp_c)) + 
  geom_line(colour = "black") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Temperature in °C", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag308_raw_temp

gridExtra::grid.arrange(tag308_raw_depth, tag308_raw_temp, ncol = 1)

# tag 321
tag321_raw_depth <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = -depth_m)) + 
  # geom_point(aes(color = temp_c), size = 0.5) + 
  geom_line() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
 labs(x = "", y = "Depth in m", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag321_raw_depth

### templog 
tag321_raw_temp <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = temp_c)) + 
  geom_line(colour = "black") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b'%y",
                   expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Temperature in °C", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 

# tag321_raw_temp

gridExtra::grid.arrange(tag321_raw_depth, tag321_raw_temp, ncol = 1)

### depthtemp 

# tag321_raw_depthtemp <- ggplot2::ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293321"), aes(x = date_time, y = -depth_m)) + 
#   geom_point(aes(color = temp_c), size = 0.3, alpha = 0.7) +
#   # geom_line() +
#   scale_x_datetime(date_breaks = "1 month",
#                    date_labels = "%b'%y",
#                    expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(x = "", y = "Depth in m", color = "Temperature in °C") #+ #title = paste0("tag ", tag_serial_number_short), 
# tag321_raw_depthtemp

## daily summaries ####

p_308_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293308")
p_308_sum_stats

p_321_sum_stats <- plot_summary_stats(data_depth = long_dst_date,
                                      tag_serial_num = "1293321")
p_321_sum_stats

## wavelet ####

# dates
dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

## tag 308
wt_308_depthmedian_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                              filter(tag_serial_number == "1293308") %>%
                                              dplyr::select(depth_median_sgolay),
                                            dt = 1,
                                            factor_smallest_scale = 2)
wt_df_308_depthmedian_sgolay <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthmedian_sgolay)

p_wt_308_depthmedian_sgolay <- plot_wavelet(wt_df = wt_df_308_depthmedian_sgolay,
                                                 type = "power_log") 
p_wt_308_depthmedian_sgolay

## tag 321
wt_321_depthmedian_sgolay <- compute_wavelet(parameter = long_dst_date %>% 
                                               filter(tag_serial_number == "1293321") %>%
                                               dplyr::select(depth_median_sgolay),
                                             dt = 1,
                                             factor_smallest_scale = 2)
wt_df_321_depthmedian_sgolay <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthmedian_sgolay)

p_wt_321_depthmedian_sgolay <- plot_wavelet(wt_df = wt_df_321_depthmedian_sgolay,
                                            type = "power_log") 

p_wt_321_depthmedian_sgolay


# acoustic detections ####
## summaries ####
detections_sum_sex <- detections_tempdepth_daynight %>% group_by(sex) %>% summarise(n_detect = n(), 
                                                              n_detect_perc = 100 * (n_detect / detections_tempdepth_daynight %>% nrow()))

detections_sum_area <- detections_tempdepth_daynight %>% group_by(area) %>% summarise(n_detect = n(), 
                                                              n_detect_perc = 100 * (n_detect / detections_tempdepth_daynight %>% nrow()))
detections_sum_area

## abacus plot ####

p_abacus <- ggplot() + # %>% mutate(tag_serial_number = reorder(tag_serial_number, masterias_info$release_date_time))
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
  geom_point(data = detections_tempdepth_daynight,
             aes(x = date_time, y = tag_serial_number, colour = area, pch = sex), size = 3) +
  geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
             aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
  scale_x_datetime(date_breaks = "1 month",
                   # date_minor_breaks = "1 month",
                   date_labels = "%b'%y") +
  labs(x = "", y = "Tag serial nr.", colour = "Area", pch = "Sex") 
# +theme(axis.text.x = element_text(angle = 45, hjust = 0.5))
p_abacus

## heatmap ####

detections_sum_station <- detections_tempdepth_daynight %>% 
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                station_name = gsub("bpns-", "", station_name)) %>%
  mutate(month_year = as.POSIXct(paste0(lubridate::year(date_time), '-', lubridate::month(date_time), '-16')),
         month_year_chr = paste0(lubridate::year(date_time), '-', date_time %>% format("%b"))) %>%
  group_by(station_name, month_year, sex) %>%
  # group_by(area, month_year, sex) %>%
  summarise(n_detect = n(),
            area = area %>% unique(),
            month_year_chr = month_year_chr %>% unique(), 
            n_ind = tag_serial_number %>% unique() %>% length())

p_detections_heatmap <- ggplot(data = detections_sum_station %>% filter(!area == "BPNS", sex == "f"),
                               aes(x = month_year, y = station_name, fill = n_detect, colour = n_detect)) + #, color = group
  geom_tile(linewidth = 0.75) +
  geom_text(aes(x = month_year, y = station_name, label = paste0(n_ind)), colour = "grey", angle = 0, family = "sans", fontface = "bold", size = 5) + #, colour = "grey"
  # facet_grid(vars(sex), scales="free_y") +
  scale_fill_viridis_c(expand = c(0,0)) +
  scale_colour_viridis_c(expand = c(0,0)) +
  # scale_colour_manual(name = "", values = c("# individuals" = "grey")) +
  # scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
  #                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
  
  scale_x_datetime(date_breaks = "1 month",
                   date_minor_breaks = "1 month",
                   date_labels = "%b'%y") +
  # scale_y_discrete(labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), expand = c(0,0)) +
  # theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(x = "", y = "Area", fill = "# detections", colour = "# detections") #, colour = "# detections"

p_detections_heatmap #%>% ggplotly()


p_detections_heatmap <- ggplot(data = summary_wide2 %>% dplyr::mutate(station_name = gsub("ws-", "", station_name)),
                                   aes(x = station_name, y = depth_range, fill = value)) + #, color = group
  geom_tile(linewidth = 0.5) +
  facet_grid(vars(group), scales="free_y") +
  # theme_minimal(base_size = 12) +
  scale_fill_viridis_c(expand = c(0,0)) +
  scale_y_discrete(labels = c("40-50 m", "30-40 m", "20-30 m", "10-20 m", "0-10 m"), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) +
  labs(x = "Receiver Station", y = "Depth bin", fill = "Percentage")

p_detections_heatmap #%>% ggplotly()

# maps ####

# bathy_belgium <- bathy_belgium %>% filter(dplyr::between(latitude, 51.343, 51.485) & dplyr::between(longitude, 3.455, 3.77))

labels_latlng <- tibble(name = c("North Sea", "Hurd deep", "English Channel", "France", "Belgium"),
                        lat = c(51.92, Hurd_deep$latitude, 50.5, 49.8, 50.86),
                        lng = c(3.75, Hurd_deep$longitude, -1.6, 2.7, 3.84))

receiver_stations <- deployments %>% 
  group_by(station_name) %>% 
  summarise(deploy_latitude = mean(deploy_latitude),
            deploy_longitude = mean(deploy_longitude)) %>%
  dplyr::mutate(station_name = gsub("ws-", "", station_name),
                station_name = gsub("bpns-", "", station_name)) %>%
  filter(deploy_latitude %>% between(50, 52),
         deploy_longitude %>% between(1.5, 4.5))

ws_stations <- receiver_stations %>% 
  filter(deploy_latitude %>% between(51.3, 51.5),
         deploy_longitude %>% between(3.4, 4.05)) %>%
  mutate(area = ifelse(deploy_latitude < 3.6, "WS1", 
                       ifelse(deploy_latitude < 3.9, "WS2", "WS3")))

# outlines <- rbind(Belgium, Netherlands, North_sea, English_channel)
outlines <- rbind(Belgium, Netherlands)

col_scale_areas <- c("grey","#E67D1F", "#EFC000")
# for emodnet layer
emodnet_tiles <-"https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"
cite_emodnet <- "<a href='https://emodnet.ec.europa.eu'>EMODnet</a>"
attr(cite_emodnet, "class") <- c("html", "character")

## westerschelde ####
map1_overview <- leaflet::leaflet() %>%
  # options = leafletOptions(zoomControl = FALSE,
  #                          minZoom = 7, maxZoom = 7,
  #                          dragging = FALSE
  # )
# ) %>%
  # ADD BASELAYERS #
  # addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 0.6), group = "satellite") %>%
  leaflet::addTiles(urlTemplate = emodnet_tiles,
                    # options = leaflet::tileOptions(tms = FALSE),
                    attribution = cite_emodnet,
                    group = "EMODnet bathymetry") %>%
  # BPNS #
  addPolygons(data = BPNS, color = "grey",
              weight = 2,
              opacity = 0,
              fillOpacity = 0) %>%
  ## BPNS
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
addCircleMarkers(data = receiver_stations,
                 lat = ~deploy_latitude,
                 lng = ~deploy_longitude,
                 fillColor = "black",
                 # color = "white",
                 # weight = ifelse(close_stations$Array == "offshore", 1.5, 0),
                 radius = 3,
                 opacity = 1,
                 fillOpacity = 0.8,
                 labelOptions = labelOptions(noHide = T, textOnly = T, sticky = F),
                 label = ~station_name,
                 # highlightOptions = highlightOptions(bringToFront = TRUE),
                 group = "stations") %>%
# GRATICULE #
addSimpleGraticule(interval = 0.5) %>%
  # OUTLINES BOUNDARIES #
  addPolygons(data = outlines, color = "black",
              weight = 1,
              opacity = 1.0,
              # label = "North Sea",
              fillOpacity = 0,
              labelOptions = labelOptions(noHide = T, textOnly = F, offset = c(-170, 500), permanent = T)) %>% #
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
  leafem::addMouseCoordinates() %>%
  setView(1.35, 50.75, zoom = 6.5) %>%
    addFullscreenControl() %>%
  # addLayersControl(position = "topright",
  #                  baseGroups = c("EMODnet bathymetry", "satellite", "bathymetry", "OpenStreetMap"),
  #                  overlayGroups = c("<span style=color:grey>released tags</span>", "stations", "wrecks, OWFs, cables"),
  #                  options = layersControlOptions(collapsed = FALSE)) #%>%
  # addLegend(position = "bottomright",
  #           opacity = 1,
  #           colors = c("darkgrey", "yellow", "red", "blue", "green"),
  #           labels = c("BPNS", "Scheldt estuary", "Windfarms", "Submarine cables", "Shipwrecks"),
  #           # title = "Legend",
  #           labFormat = labelFormat(textOnly = T)) %>%
  # SCALEBAR #
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 150, imperial = F))

map1_overview

# addPolygons(data = Schelde_boundaries, color = "yellow",
#             weight = 2,
#             opacity = 1.0,
#             fillOpacity = 0) %>%
# addMiniMap(position = "bottomright",
#            width = 100,
#            height = 100,
#            zoomLevelOffset = -3,
#            zoomLevelFixed = T,
#            tiles = "https://tiles.emodnet-bathymetry.eu/2020/baselayer/web_mercator/{z}/{x}/{y}.png"#providers$Esri.WorldStreetMap)
# ) %>%
# addRasterImage(bathy_belgium_raster, opacity = 1, colors = "Spectral", group = "bathymetry") %>%
# addPolygons(data = coastline_BE_poly, opacity = 1, fillColor = "grey", weight = 0, fillOpacity = 0.7, group = "bathymetry") %>% #"#ECE4BF"
# addTiles(group = "OpenStreetMap") %>%
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
# addPolylines(data = cables,
#              color = "blue",
#              weight = 1,
#              opacity = 0.6,
#              label = ~name,
#              group = "wrecks, OWFs, cables") %>%
# shipwrecks #
# addCircleMarkers(data = wrecks,
#                  fillColor = "green",
#                  opacity = 0,
#                  fillOpacity = 1,
#                  radius = 0.9,
#                  label = ~paste0("Object: ", obj_type, ", sink year: ", sink_yr),
#                  group = "wrecks, OWFs, cables") %>%
# addCircleMarkers(data = wrecks_BE %>% filter(Staat != "Geborgen"),
#                  lng = ~longitude,
#                  lat = ~latitude,
#                  fillColor = "green",
#                  opacity = 0,
#                  fillOpacity = 1,
#                  radius = 0.9,
#                  label = ~paste0("Object: ", Type, ", sink date: ", sink_yr, ", status: ", Staat, ", material: ", Materiaal, ", name: ", Naam),
#                  group = "wrecks, OWFs, cables") %>%
# more bathy #
# addCircleMarkers(data = bathy_belgium, lat = ~latitude, lng = ~longitude,
#                  opacity = 0, fillOpacity = 0,
#                  label = ~paste0(depth_m %>% round(digits = 2), " m")) %>%
# windfarms #
# addPolygons(data = windfarms_polygons %>% filter(!status %in% c("Approved", "Planned")),
#             color = "red",
#             weight = 1,
#             opacity = 1,
#             fillOpacity = 0.3,
#             label = ~paste0("status: ", status, ", country: ", country, ", year: ", year),
#             group = "wrecks, OWFs, cables") %>%




leaflet() %>% addTiles() %>%
  addCircleMarkers(data = test,
                   lat = ~deploy_latitude,
                   lng = ~deploy_longitude,
                   label = ~station_name,
                   labelOptions = labelOptions(noHide = T, textOnly = T, sticky = F))
