# Script to generate figures for the thesis manuscript

# Workspace ####

rm(list = ls())

## libraries ####

library(ggplot2)
library(dplyr)
library(scale)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

## load data ####

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()

## function to plot wavelet results ####
plot_wavelet_gg2 <- function(wt_df, type = c("power", "significance", "power_log"),
                             # y_breaks = c(4, 8, 16, 32, 64, 128),
                             # x_breaks = c("000", "100", "200", "300", "400", "500"),
                             date = TRUE){
  # transformation function for the y axis
  my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)
  
  n_data <- wt_df$t %>% unique %>% length()
  
  # y axis labels
  y_breaks <- 2^floor(log2(wt_df$period)) %>% unique()
  
  # x axis labels
  ifelse(date %>% isTRUE(),
         x_breaks <- c(wt_df$date[1], wt_df$date[(1/5) * n_data], wt_df$date[(2/5) * n_data], wt_df$date[(3/5) * n_data],
                       wt_df$date[(4/5) * n_data], wt_df$date[(5/5) * n_data])
         ,
         x_breaks <- sprintf("%03d", seq(from = 0, to = n_data, by = 100)))
  
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
                  scale_x_discrete(breaks = x_breaks) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "dates", y = "period in hours", fill = "log2(power)") #+
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
                         scale_x_discrete(breaks = x_breaks) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "dates", y = "period in hours", fill = "significance") #+
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
                         scale_x_discrete(breaks = x_breaks) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "dates", y = "period in hours", fill = "power") #+
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
                  scale_x_discrete(breaks = x_breaks) +
                  scale_fill_viridis_c(direction = 1, option = "turbo") +
                  labs(x = "time in days", y = "period in hours", fill = "log2(power)")# +
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
                         scale_x_discrete(breaks = x_breaks) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "time in days", y = "period in hours", fill = "significance")# +
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
                         scale_x_discrete(breaks = x_breaks) +
                         scale_fill_viridis_c(direction = 1, option = "turbo") +
                         labs(x = "time in days", y = "period in hours", fill = "power") #+
                       # theme(axis.text.x = element_text(angle = 60, hjust = 0.5))
                )
         )
  )
  
  return(plot)
}




# set plot theme ####

thesis_theme <- theme(
  plot.title = element_text(family = "serif", size = 12, face = "bold"),
  plot.subtitle = element_text(family = "serif", size = 12),
  axis.title = element_text(family = "serif", size = 12),
  axis.text = element_text(family = "serif", size = 10),
  legend.title = element_text(family = "serif", size = 12),
  legend.text = element_text(family = "serif", size = 10),
  # plot.background = element_blank()#,
  panel.background = element_blank(),
  # panel.background = element_rect(fill = "transparent"),
  panel.grid.major = element_line(color = "gray70", linetype = "solid"),
  panel.grid.minor = element_line(color = "gray90", linetype = "dashed"),
)

# Set the theme as the default for all plots
theme_set(thesis_theme)

# plots ####

## Autocorrelation ####

p_acf_308 <- ggplot(data = acf_308_df, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  # theme_minimal() +
  labs(y = "autocorrelation", x = "lag in days") #, title = "tag 308 (female), daily median depth roll3"
# p_acf_308

p_acf_321 <- ggplot(data = acf_321_df, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  # theme_minimal() +
  labs(y = "autocorrelation", x = "lag in days") #, title = "tag 321 (male), daily median depth roll3"
# p_acf_321

# # # first plot attempt ####
# # ggplot(data = long_dst_date,
# #        mapping = aes(x = date, y = depth_median)) +
# #   geom_line() +
# #   labs(x = "Date", y = "Depth in m")
# # 
# par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
# # the general plots you cannot save, I would have to call the function in the thesis manuscript
# biwavelet::plot.biwavelet(wt_308_depthrange,
#      type = "power.corr.norm", main = "", xlab = "Time in days", ylab = "Period", plot.cb = T)

#

# save plots ####

save_data(data = p_acf_308, folder = plot_path)
save_data(data = p_acf_321, folder = plot_path)