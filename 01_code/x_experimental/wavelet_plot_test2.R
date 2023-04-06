# script to test out wavelet analysis result plots 
library(scales)
library(biwavelet)
rm(list = ls())

source(paste0(getwd(), "/01_code/06_functions/functions.R"))
source(paste0(getwd(), "/01_code/02_load_data/load_wavelet_results.R"))
source(paste0(getwd(), "/01_code/06_functions/compute_wavelettransform.R"))

# test data ####
t1 <- cbind(1:100, rnorm(100)) %>% as.data.frame()
wt_output1 <- t1 %>% as.matrix() %>% wt()
dates1 <- t1 %>% select(V1) %>% `colnames<-`("t") %>%
  mutate(t = sprintf("%03d", t %>% as.numeric()))

# real data test dataset ####
t2 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(depth_median) %>%
  mutate(t = seq(from = 1, to = n(), by = 1)) %>%
  relocate(t, depth_median) %>%
   head(n = 100)
wt_output2 <- t2 %>% as.matrix() %>% wt()
dates2 <- t2 %>% select(t) %>% `colnames<-`("t") %>%
  mutate(t = sprintf("%03d", t %>% as.numeric()))

# plot.biwavelet ####
plot.biwavelet(x = wt_output1, type = "power.corr", plot.cb = T, tol = 1)

plot.biwavelet(x = wt_output2, type = "power.corr", plot.cb = T)

# transformation for y axis ####
my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)


# try as ggplot ####
wt_df1 <- wavelet_output_compare(dates = dates1, wt_output = wt_output1)

wt_df2 <- wavelet_output_compare(dates = dates2, wt_output = wt_output2)


plot1 <- ggplot(data = wt_df1 #%>% filter(date %>% between(dates1$t[1], dates1$t[nrow(dates1) - 1]))
                ) +
  geom_tile(aes(x = t, y = period, fill = power, height = height), #, colour = sig
            position = "identity"
  ) +
  # geom_tile(data = wt_df1 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
  #           fill = "black",
  #           width = 1.8,
  #           # height = ,
  #           position = "identity") +
  # geom_tile(data = wt_df1 %>% filter(sig == 1), aes(x = t, y = power, fill = power, height = height),
  #           position = "identity") +
  geom_tile(data = wt_df1 %>% filter(sig == 1), aes(x = t, y = period, height = height),
            fill = "white",#, colour = sig
  position = "identity") +
  scale_y_continuous(trans = my_trans, expand = c(0,0), 
                     breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
  # scale_y_reverse(expand = c(0,0), trans = my_trans) +
  scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "100")) +
  scale_fill_viridis_c(direction = 1, option = "viridis") +
  labs(x = "Date", y = "Period", fill = "Power") + #
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot1 #%>% plotly::ggplotly()


plot2 <- ggplot(data = wt_df2 #%>% filter(date %>% between(dates2$t[2], dates2$t[nrow(dates2) - 2]))
) +
  geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
            position = "identity"
  ) +
  geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
            fill = "black",
            width = 2.8,
            # height = ,
            position = "identity") +
  geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
            position = "identity") +

  # geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, height = height),
  #           fill = "white",#, colour = sig
  # position = "identity") +
  scale_y_continuous(trans = my_trans, expand = c(0,0), 
                     breaks = c(2, 4, 8, 26, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "26", "32")
  # scale_y_reverse(expand = c(0,0), trans = my_trans) +
  scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "200")) +
  scale_fill_viridis_c(direction = 1, option = "turbo") +
  labs(x = "Date", y = "Period", fill = "log2(Power)") + #, fill = "Power"
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 2))

plot2 #%>% plotly::ggplotly()

# noch ein test, diesmal komplette real data ####

# real data test dataset ####
t3 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(depth_median) %>%
  mutate(t = seq(from = 1, to = n(), by = 1)) %>%
  relocate(t, depth_median) #%>% head(n = 100) #diesmal alles
wt_output3 <- t3 %>% as.matrix() %>% wt()
dates3 <- t3 %>% select(t) %>% `colnames<-`("t") %>%
  mutate(t = sprintf("%03d", t %>% as.numeric()))

# test data ####
t4 <- cbind(1:nrow(t3), rnorm(nrow(t3))) %>% as.data.frame()
wt_output4 <- t4 %>% as.matrix() %>% wt()
dates4 <- t4 %>% select(V1) %>% `colnames<-`("t") %>%
  mutate(t = sprintf("%03d", t %>% as.numeric()))


# plot.biwavelet ####
plot.biwavelet(x = wt_output3, type = "power.corr", plot.cb = T, tol = 1)

plot.biwavelet(x = wt_output4, type = "power.corr", plot.cb = T, tol = 1)

# transformation for y axis ####
my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)


# try as ggplot ####
wt_df3 <- wavelet_output_compare(dates = dates3, wt_output = wt_output3)

wt_df4 <- wavelet_output_compare(dates = dates4, wt_output = wt_output4)


plot3 <- ggplot(data = wt_df3 #%>% filter(date %>% between(dates1$t[1], dates1$t[nrow(dates1) - 1]))
) +
  geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
            position = "identity"
  ) +
  geom_tile(data = wt_df3 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
            fill = "black",
            width = 3,
            # height = ,
            position = "identity") +
  geom_tile(data = wt_df3 %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
            position = "identity") +
  # geom_tile(data = wt_df3 %>% filter(sig == 1), aes(x = t, y = period, height = height),
  #           fill = "white",#, colour = sig
  # position = "identity") +
  scale_y_continuous(trans = my_trans,
                     # breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
                     breaks = c(4, 8, 16, 32, 64, 128), 
                     expand = c(0,0)) +
  # scale_y_reverse(expand = c(0,0)) +
  # scale_y_continuous(trans = "log2", expand = c(0,0)) +
  scale_x_discrete(breaks = c("000", "100", "200", "300", "400"),
                   expand = c(0,0)) +
  scale_fill_viridis_c(direction = 1, option = "viridis") +
  labs(x = "Date", y = "log2(Period)", fill = "log2(Power)") + #
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot3 #%>% plotly::ggplotly()

plot5 <- ggplot(data = wt_df3 #%>% filter(date %>% between(dates1$t[1], dates1$t[nrow(dates1) - 1]))
) +
  geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
            position = "identity"
  ) +
  # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
  #           fill = "black",
  #           width = 3,
  #           # height = ,
  #           position = "identity") +
  # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
  #           position = "identity") +

  # geom_tile(data = wt_df4 %>% filter(significance >= 1), aes(x = t, y = period, height = height),
  #           fill = "white",
  #           alpha = 0.3) +
  # geom_tile(aes(x = t, y = period, height = height, fill = sig),
  #           alpha = 0) +
  scale_y_continuous(trans = my_trans,
                     # breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
                     breaks = c(4, 8, 16, 32, 64, 128), 
                     expand = c(0,0)) +
  # scale_y_reverse(expand = c(0,0)) +
  # scale_y_continuous(trans = "log2", expand = c(0,0)) +
  scale_x_discrete(breaks = c("000", "100", "200", "300", "400"),
                   expand = c(0,0)) +
  scale_fill_viridis_c(direction = 1, option = "turbo") +
  labs(x = "Date", y = "log2(Period)", fill = "log2(Power)") + #
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot5 #%>% plotly::ggplotly()

# plot wavelet gg ####

plot_wavelet_gg <- function(wt_df, type = c("power", "significance", "power_log"),
                            y_breaks = c(4, 8, 16, 32, 64, 128),
                            x_breaks = c("000", "100", "200", "300", "400")){
 
  ifelse(type == "power_log",
         plot <- ggplot(data = wt_df) +
           geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
                     position = "identity"
           ) +
           # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
           #           fill = "black",
           #           width = 3,
           #           # height = ,
           #           position = "identity") +
           # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
           #           position = "identity") +
           
           # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = t, y = period, height = height),
           #           fill = "white",
           #           alpha = 0.3) +
         # geom_tile(aes(x = t, y = period, height = height, fill = sig),
         #           alpha = 0) +
         scale_y_continuous(trans = my_trans,
                            breaks = y_breaks, 
                            expand = c(0,0)) +
           # scale_y_reverse(expand = c(0,0)) +
           # scale_y_continuous(trans = "log2", expand = c(0,0)) +
           scale_x_discrete(breaks = x_breaks,
                            expand = c(0,0)) +
           scale_fill_viridis_c(direction = 1, option = "turbo") +
           labs(x = "Date", y = "log2(Period)", fill = "log2(Power)") + #
           # theme_bw() +
           theme(axis.text.x = element_text(angle = 60, hjust = 1))
         , 
         ifelse(type == "significance",
                  plot <- ggplot(data = wt_df) +
                    geom_tile(aes(x = t, y = period, fill = significance, height = height), #, colour = sig
                              position = "identity"
                    ) +
                    # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
                    #           fill = "black",
                    #           width = 3,
                    #           # height = ,
                    #           position = "identity") +
                    # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
                    #           position = "identity") +
                    
                    # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = t, y = period, height = height),
                    #           fill = "white",
                    #           alpha = 0.3) +
                  # geom_tile(aes(x = t, y = period, height = height, fill = sig),
                  #           alpha = 0) +
                  scale_y_continuous(trans = my_trans,
                                     breaks = y_breaks, 
                                     expand = c(0,0)) +
                    # scale_y_reverse(expand = c(0,0)) +
                    # scale_y_continuous(trans = "log2", expand = c(0,0)) +
                    scale_x_discrete(breaks = x_breaks,
                                     expand = c(0,0)) +
                    scale_fill_viridis_c(direction = 1, option = "turbo") +
                    labs(x = "Date", y = "log2(Period)", fill = "Significance Level") + #
                    # theme_bw() +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
                ,
                plot <- ggplot(data = wt_df) +
                  geom_tile(aes(x = t, y = period, fill = power, height = height), #, colour = sig
                            position = "identity"
                  ) +
                  # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
                  #           fill = "black",
                  #           width = 3,
                  #           # height = ,
                  #           position = "identity") +
                  # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
                  #           position = "identity") +
                  
                  # geom_tile(data = wt_df %>% filter(significance >= 1), aes(x = t, y = period, height = height),
                  #           fill = "white",
                  #           alpha = 0.3) +
                # geom_tile(aes(x = t, y = period, height = height, fill = sig),
                #           alpha = 0) +
                scale_y_continuous(trans = my_trans,
                                   breaks = y_breaks, 
                                   expand = c(0,0)) +
                  # scale_y_reverse(expand = c(0,0)) +
                  # scale_y_continuous(trans = "log2", expand = c(0,0)) +
                  scale_x_discrete(breaks = x_breaks,
                                   expand = c(0,0)) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "Date", y = "log2(Period)", fill = "Power") + #
                  # theme_bw() +
                  theme(axis.text.x = element_text(angle = 60, hjust = 1))
                )
         )
  
  # plot <- ggplot(data = wt_df #%>% filter(date %>% between(dates1$t[1], dates1$t[nrow(dates1) - 1]))
  # ) +
  #   geom_tile(aes(x = t, y = period, fill = ifelse(type == "power_log", power_log,
  #                                                  ifelse(type == "significance", significance,
  #                                                         power)), 
  #                 height = height), #, colour = sig
  #             position = "identity"
  #   ) +
  #   # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.15), #
  #   #           fill = "black",
  #   #           width = 3,
  #   #           # height = ,
  #   #           position = "identity") +
  #   # geom_tile(data = wt_df4 %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
  #   #           position = "identity") +
  #   
  #   # geom_tile(data = wt_df4 %>% filter(significance >= 1), aes(x = t, y = period, height = height),
  #   #           fill = "white",
  #   #           alpha = 0.3) +
  # # geom_tile(aes(x = t, y = period, height = height, fill = sig),
  # #           alpha = 0) +
  # scale_y_continuous(trans = my_trans,
  #                    breaks = y_breaks, 
  #                    expand = c(0,0)) +
  #   # scale_y_reverse(expand = c(0,0)) +
  #   # scale_y_continuous(trans = "log2", expand = c(0,0)) +
  #   scale_x_discrete(breaks = x_breaks,
  #                    expand = c(0,0)) +
  #   scale_fill_viridis_c(direction = 1, option = "turbo") +
  #   labs(x = "Date", y = "log2(Period)", fill = ifelse(type == "power_log", "log2(Power)", type)) + #
  #   # theme_bw() +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  return(plot) #%>% plotly::ggplotly()
}

# # example ####
# plot_wavelet_gg(wt_df = wt_df4, type = "significance")

# old ####
# plot2 <- ggplot(data = wt_df2 #%>% filter(date %>% between(dates2$t[2], dates2$t[nrow(dates2) - 2]))
# ) +
#   geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
#             position = "identity"
#   ) +
#   geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
#             fill = "black",
#             width = 2.8,
#             # height = ,
#             position = "identity") +
#   geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, fill = power_log, height = height),
#             position = "identity") +
#   
#   # geom_tile(data = wt_df2 %>% filter(sig == 1), aes(x = t, y = period, height = height),
#   #           fill = "white",#, colour = sig
#   # position = "identity") +
#   scale_y_continuous(trans = my_trans, expand = c(0,0), 
#                      breaks = c(2, 4, 8, 26, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "26", "32")
#   # scale_y_reverse(expand = c(0,0), trans = my_trans) +
#   scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "200")) +
#   scale_fill_viridis_c(direction = 1, option = "turbo") +
#   labs(x = "Date", y = "Period", fill = "log2(Power)") + #, fill = "Power"
#   # theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 2))
# 
# plot2 #%>% plotly::ggplotly()
# 
# 
# 
# x <- wt_output$t 
# y <- wt_output$period %>% log2
# z <- wt_output$power %>% log2
# z2 <- wt_output$signif
# tol <- 1
# zlim <- range( c(-1, 1) * max(z) )
# zvals <- z
# # zvals[zvals < zlim[1]] <- zlim[1]
# 
# image(x = x, y = y, z = t(zvals), ylim = rev(range(y)), col = rainbow(5000))
# contour(x = x, y = y, z = t(z2), level = tol, col = "black",
#         lwd = 3, add = TRUE, drawlabels = FALSE)
# 
# # wt_output$signif %>% as.data.frame() %>% View()
# par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
# # plot(wt_res, 
#      # type = "power.corr.norm", main = "test", plot.cb = F, plot.phase = F)
# biwavelet::plot.biwavelet(wt_output, type = "power.corr", plot.cb = T, tol = 1)
# 
# wt_df <- wavelet_output_compare(dates = dates, wt_output = wt_output) 
# 
# ## real data ####
# dates_real_data <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)
# wt_df_real_data <- wavelet_output_compare(dates = dates_real_data, wt_output = wt_321_mediandepth) 
# 
# my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
# 
# test_plot_real_data <- ggplot(data = wt_df_real_data) +
#   geom_tile(aes(x = t, y = period, fill = power_log, height = height), #, colour = sig
#             position = "identity"
#   ) +
#   # geom_tile(data = wt_df_real_data %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
#   #           fill = "black",
#   #           width = 1.8,
#   #           # height = ,
#   #           position = "identity") +
#   # geom_tile(data = wt_df_real_data %>% filter(sig == 1), aes(x = t, y = period, fill = power, height = height),
#   #           position = "identity") +
#   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height),
#   #           fill = "white",#, colour = sig
#   # position = "identity") +
#   # scale_y_continuous(expand = c(0,0), trans = my_trans) + #,
#                      # breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
#   scale_y_reverse() +
#   # scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "100")) +
#   # scale_x_discrete(n.breaks = 10) +
#   scale_fill_viridis_c(direction = 1, option = "viridis") +
#   labs(x = "Date", y = "Period", fill = "Power") + #
#   # theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# test_plot_real_data #%>% plotly::ggplotly()
# 
# 
# test_plot <- ggplot(data = wt_df) +
#   geom_tile(aes(x = t, y = period, fill = power, height = height), #, colour = sig
#             position = "identity"
#             ) +
#   geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
#             fill = "black",
#             width = 1.8,
#             # height = ,
#             position = "identity") +
#   geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power, height = height),
#             position = "identity") +
#   # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height),
#   #           fill = "white",#, colour = sig
#             # position = "identity") +
#   scale_y_continuous(expand = c(0,0), trans = my_trans,
#                      breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
#   # scale_y_reverse(trans = my_trans) +
#   scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "100")) +
#   scale_fill_viridis_c(direction = 1, option = "viridis") +
#   labs(x = "Date", y = "Period", fill = "Power") + #
#   # theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# 
# test_plot #%>% plotly::ggplotly()
# 
# dates <- long_dst_date %>% ungroup() %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)
# 
# x <- wt_321_mediandepth$t 
# y <- wt_321_mediandepth$period %>% log2
# z <- wt_321_mediandepth$power %>% log2 #%>% scale()
# z2 <- wt_321_mediandepth$signif
# tol <- 1
# 
# wt_df <- wavelet_output_compare(dates = dates, wt_output = wt_321_mediandepth)
# 
# ggplot(wt_df, aes(x = t, y = freq_norm)) + geom_point()
# 
# image(x = x, y = y, z = t(z), ylim = rev(range(y)), col = rainbow(5000))
# contour(x = x, y = y, z = t(z), level = , col = "black",
#         lwd = 3, add = TRUE, drawlabels = FALSE)
# 
# image(x = x, y = y, z = t(z2), ylim = rev(range(y)), col = rainbow(5000))
# contour(x = x, y = y, z = t(z2), level = 01, col = "black",
#         lwd = 3, add = TRUE, drawlabels = FALSE)
# 
# wt_321_mediandepth$signif %>% as.data.frame() %>% View()
# 
# plot.biwavelet(wt_321_mediandepth)
# 
# g <- ggplot(data = wt_ggplot, aes(x = t, y = period, z = significance)) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# g + geom_contour(bins = 3)
# 
# v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
# v + 
#   geom_tile(aes(waiting, eruptions, fill = density)) +
#   geom_contour(breaks = 0.03) #that's it!!!
# ggplot(faithful, aes(waiting, eruptions)) +
#   stat_density_2d()
# v + geom_contour_filled(bins = 10)
# v + geom_contour(bins = 2)
# v + geom_contour(binwidth = 0.01)
