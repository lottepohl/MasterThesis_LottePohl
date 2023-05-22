# script with old functions from thesis_manuscrupt_figures.R

# summary stats ####

plot_summary_stats <- function(data_depth, data_DVM, tag_serial_num){
  
  # date_breaks_minor <- tibble(dates = c("2019-08-15", "2019-09-15", "2019-10-15", "2019-11-15"))
  # test <- date_breaks_minor + lubridate::days(30)
  
  ggplot(data = data_depth %>% ungroup() %>% 
           filter(tag_serial_number == tag_serial_num) %>% 
           mutate(t_days = t_days %>% as.numeric())) +
    # geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
    #          aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "DVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
    # geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
    #          aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "rDVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
    # geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
    #          aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "nVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
    # OLD
    # geom_rect(data = data_DVM %>% filter(tag_serial_number == tag_serial_num),
    #             aes(x = date_24hcycle,
    #                 ymin = -70,
    #                 ymax = 0,
  #                 fill = vertical_movement)) +
  geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "depth range"), alpha = 0.55) +
    geom_line(aes(x = date, y = -depth_median_sgolay, colour = "median")) +
    # geom_line(aes(x = date, y = -depth_median_change_sgolay, colour = "median change")) + # %>% abs()
    # scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d", expand = c(0,0)) + #, %y
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + #angle = 30
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Date", y = "Depth in m") +  
    scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
                                              "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
    scale_fill_manual(name = "", values = c("depth range" = "lightgrey", "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #"range" = "grey", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
    
    theme(legend.position = "none", # "bottom",
          legend.box = "horizontal") +
    scale_x_datetime(
      # date_minor_breaks = "2 weeks", # make vector with 15.1., 15.2. to take as minor breaks
      date_breaks = "1 month",
      # date_minor_breaks = "1 month",
      # date_breaks = "2 months",
      date_labels = "%b %Y"
      # ,expand = c(0,0)
    )
  
  # ggplot(data = data_depth %>% ungroup() %>% 
  #          filter(tag_serial_number == tag_serial_num) %>% 
  #          mutate(t_days = t_days %>% as.numeric())) +
  #   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
  #            aes(x = date_24hcycle, y = 3, fill = "DVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
  #   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
  #            aes(x = date_24hcycle, y = 3, fill = "rDVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
  #   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
  #            aes(x = date_24hcycle, y = 3, fill = "nVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
  #   geom_line(aes(x = date, y = -depth_median_roll3, colour = "median")) +
  #   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "range"), alpha = 0.2) +
  #   geom_line(aes(x = date, y = -depth_median_change_roll3, colour = "median change")) + # %>% abs()
  #   scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d") + #, %y
  #   theme(axis.text.x = element_text(angle = 30, hjust = 0.5)) +
  #   scale_y_continuous(expand = c(0,0)) +
  #   labs(x = "date", y = "depth (change) in m") +  
  #   scale_colour_manual(name = "", values = c("median" = "black", "range" = "transparent", "change of range" = "black", "median change" = "purple",
  #                                             "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #
  #   scale_fill_manual(name = "", values = c("range" = "transparent", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
  #                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal")
  
}

# ggplot(data = long_dst_date %>% ungroup() %>% 
#          filter(tag_serial_number == "1293308") %>% 
#          mutate(t_days = t_days %>% as.numeric())) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "DVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "rDVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_bar(data = masterias_DVM_sum_day %>% filter(tag_serial_number == "1293308", vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = 5, fill = "nVM"), stat = "identity", alpha = 0.5, position = position_nudge(y = 10), width = NULL) +
#   geom_line(aes(x = date, y = -depth_median_roll3, colour = "daily median")) +
#   geom_ribbon(aes(x = date, ymin = -depth_max_roll3, ymax = -depth_min_roll3, colour = "daily range"), alpha = 0.2) +
#   geom_line(aes(x = date, y = -depth_median_change_roll3, colour = "daily median change")) + # %>% abs()
#   scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d") + #, %y
#   theme(axis.text.x = element_text(angle = 30, hjust = 0.5)) +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(x = "date", y = "depth (change) in m") + #title = 'Tag 308 (female)', 
#   scale_colour_manual(name = "", values = c("daily median" = "black", "daily range" = "transparent", "change of daily range" = "black", "daily median change" = "purple",
#                                             "DVM" = "red", "rDVM" = "blue", "nVM" = "green", "change of daily median change" = "lightblue", "daily median change raw" = "orange",
#                                             "change of daily median change abs" = "darkgreen", "change of daily range abs" = "purple")) + #
#   scale_fill_manual(name = "", values = c("daily range" = "transparent", #"daily median" = "black", "change of daily range" = "black", "daily median change" = "darkblue",
#                                           "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) #+
# # theme(legend.position = "bottom",
# #       legend.box = "horizontal")
# 
# p_t_308_depth_median_range_change_ribbon #%>% ggplotly()

# p <- ggplot(data = data_depth %>% ungroup() %>% 
#          filter(tag_serial_number == tag_serial_num) %>% 
#          mutate(t_days = t_days %>% as.numeric())) +
#   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "DVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "DVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
#   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "rDVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "rDVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
#   geom_bar(data = data_DVM %>% filter(tag_serial_number == tag_serial_num, vertical_movement == "nVM"), #, t_days %>% between(tag_308_migration1_start, tag_308_migration1_end)
#            aes(x = date_24hcycle, y = (data_depth$depth_max_sgolay %>% max()) * (-1), fill = "nVM"), stat = "identity", alpha = 1, position = 'dodge') + #, width = NULL
#   # geom_rect(data = data_DVM %>% filter(tag_serial_number == tag_serial_num),
#   #             aes(x = date_24hcycle,
#   #                 ymin = -70,
#   #                 ymax = 0,
#   #                 fill = vertical_movement)) +
#   geom_ribbon(aes(x = date, ymin = -depth_max_sgolay, ymax = -depth_min_sgolay, fill = "depth range"), alpha = 1) +
#   geom_line(aes(x = date, y = -depth_median_sgolay, colour = "median")) +
#   # geom_line(aes(x = date, y = -depth_median_change_sgolay, colour = "median change")) + # %>% abs()
#   scale_x_datetime(date_breaks = "6 weeks", date_labels = "%b %d") + #, %y
#   theme(axis.text.x = element_text(angle = 30, hjust = 0.5)) +
#   scale_y_continuous(expand = c(0,0)) +
#   labs(x = "date", y = "depth in m") +  
#   scale_colour_manual(name = "", values = c("median" = "black", "depth range" = "lightgrey", "change of range" = "black", "median change" = "purple",
#                                             "DVM" = "red", "rDVM" = "blue", "nVM" = "green"))  +
#   scale_fill_manual(name = "", values = c("depth range" = "lightgrey", "DVM" = "red", "rDVM" = "blue", "nVM" = "green")) + #"range" = "grey", #"median" = "black", "change of range" = "black", "median change" = "darkblue",
# 
#   theme(legend.position = "bottom",
#         legend.box = "horizontal")
# 
#  p #%>% ggplotly()

# lm to see if between jan and apr 2019 the moon illuminated fraction correlates with min depth 
# start_date <- "2018-12-12" %>% as.POSIXct()
# end_date <- "2019-03-05" %>% as.POSIXct()
# 
# # prepare data
# 
# data_lm_308 <- long_dst_date %>% 
#   filter(tag_serial_number == "1293308",
#                                  date %>% between(start_date, end_date)) %>%
#   # dplyr::select(tag_serial_number, date, depth_min_sgolay) %>%
#   mutate(moonfraq = oce::moonAngle(t = data_lm_308$date, longitude = 2.45, latitude = 51)$illuminatedFraction)
# 
# data_lm_308_sum <- data_lm_308 %>% group_by(moonfraq) %>% #make summary per moonfraq
#   summarise(depth_min_median = median(depth_min_sgolay),
#             depth_min_mean = mean(depth_min_sgolay))
# 
# ## min depth
# 
# lm_depthmin_moonfraq <- stats::lm(data = data_lm_308, formula = depth_min_sgolay ~ moonfraq)
# lm_depthmin_moonfraq %>% summary()
# 
# resid_depthmin_moonfraq <- stats::resid(lm_depthmin_moonfraq) %>% as.data.frame() %>% `colnames<-`("residuals")
# 
# # plot lm
# ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_min_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily minimum depth in m (Savitzky-Golay filter)", title = 'daily min depth over moon fraq')
# 
# # plot residuals
# ggplot(lm_depthmin_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(resid_depthmin_moonfraq, aes(sample=residuals)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_308, aes(sample=-log(depth_min_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# # plot(density(resid_depthmin_moonfraq))
# 
# ggplot(resid_depthmin_moonfraq, aes(x=residuals))+
#   geom_density(linewidth = 1) 
#   
# 
# ## median depth
# 
# 
# lm_depthmedian_moonfraq <- stats::lm(data = data_lm_308, formula = depth_median_sgolay ~ moonfraq)
# lm_depthmedian_moonfraq %>% summary()
# 
# resid_depthmedian_moonfraq <- stats::resid(lm_depthmedian_moonfraq) %>% as.data.frame() %>% `colnames<-`("residuals")
# 
# # plot lm
# ggplot(data = data_lm_308, aes(x = moonfraq, y = -log(depth_median_sgolay))) +
#   geom_smooth(method = "lm", colour = "red", fill = "grey", alpha = 0.5) +
#   geom_point() +
#   labs(x = "fraction of the moon illuminated", y = "daily median depth in m (Savitzky-Golay filter)", title = 'daily median depth over moon fraq')
# 
# # plot residuals
# ggplot(lm_depthmedian_moonfraq, aes(x = .fitted, y = .resid)) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linewidth = 0.75) +
#   labs(x = "fitted", y = "residuals")
# 
# # plot qq of residuals to asses normality of residuals (aka did we get 'everything' out of the data)
# ggplot(resid_depthmedian_moonfraq, aes(sample=residuals)) +
#   stat_qq(size=2.5) + 
#   stat_qq_line() +
#   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# # ## log transformed -> does not look better
# # ggplot(data_lm_308, aes(sample=-log(depth_median_sgolay))) +
# #   stat_qq(size=2.5) + 
# #   stat_qq_line() +
# #   labs(x = "Theoretical quantiles", y = "Sample Quantiles")
# 
# 
# # plot density (to assess normal distribution of residuals)
# # plot(density(resid_depthmedian_moonfraq))
# 
# ggplot(resid_depthmedian_moonfraq, aes(x=residuals))+
#   geom_density(linewidth = 1) 

# autocorrelation ####

# library(ggpmisc)
# library(splus2R)
# 
# acf_plot308 <- ggplot(data = acf_308_df, mapping = aes(x = lag, y = acf %>% abs())) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_segment(mapping = aes(xend = lag,
#                              yend = 0)) +
#   labs(y = "Autocorrelation", x = "Lag date") 
# acf_plot308 %>% ggplotly()
# 
# acf_plot321 <- ggplot(data = acf_321_df, mapping = aes(x = lag, y = acf)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_segment(mapping = aes(xend = lag,
#                              yend = 0)) +
#   labs(y = "Autocorrelation", x = "Lag date") 
# acf_plot321 %>% ggplotly()

# acf_plot <- ggplot(data = acf_df, mapping = aes(x = xvals, y = acf)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_segment(mapping = aes(xend = xvals, yend = 0)) +
#   labs(y = "Autocorrelation", x = "Lag date") +
#   # scale_x_datetime(date_minor_breaks = "1 month",
#   #                  date_breaks = "2 months",
#   #                  date_labels = "%b %Y",
#   #                  expand = c(0,0)) +
#   theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank()) +
#   coord_cartesian(clip = "off") +
#   annotate(geom = "text",
#            x = 1:nrow(acf_df),
#            y = 3,
#            label = xvals,
#            vjust = 3.5) +
#   annotate(geom = "text",
#            x = seq(from = 0, to = nrow(acf_308_df), by = 50),
#            y = 0,
#            label = seq(from = 0, to = nrow(acf_308_df), by = 50),
#            vjust = 5)
# 
# acf_plot
# 
# test_acf <- plot_dst_autocorrelation(acf_df = acf_308_df,
#                                      tagging_date = tagged_animal_info %>% dplyr::filter(tag_serial_number == "1293308") %>% dplyr::select(release_date_time))
# test_acf


# abacus ####
# 
# p_abacus <- ggplot() + # %>% mutate(tag_serial_number = reorder(tag_serial_number, masterias_info$release_date_time))
#   geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
#              aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
#   geom_point(data = detections_tempdepth_daynight,
#              aes(x = date_time, y = tag_serial_number, colour = area, pch = sex), size = 3) +
#   geom_point(data = masterias_info %>% dplyr::filter(n_detect > 1) %>% mutate(tag_serial_number = reorder(tag_serial_number, release_date_time, decreasing = T)),
#              aes(x = release_date_time, y = tag_serial_number), stroke = 1, colour = "black", size = 3, pch = 4) +
#   scale_x_datetime(date_minor_breaks = "1 month",
#                    date_breaks = "2 months",
#                    date_labels = "%b %Y") +
#   labs(x = "Date", y = "Tag serial nr.", colour = "Area", pch = "Sex") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.5))
