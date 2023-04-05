# Script to generate figures for the thesis manuscript

# Workspace ####

## libraries ####

library(ggplot2)
library(dplyr)

## plot path ####

plot_path <- paste0(getwd(), "/01_code/00_thesis_manuscript/figures/")

## load data ####

paste0(getwd(), "/01_code/06_functions/functions.R") %>% base::source()
paste0(getwd(), "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(getwd(), "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()

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


# # first plot attempt ####
# ggplot(data = long_dst_date,
#        mapping = aes(x = date, y = depth_median)) +
#   geom_line() +
#   labs(x = "Date", y = "Depth in m")
# 
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
# the general plots you cannot save, I would have to call the function in the thesis manuscript
biwavelet::plot.biwavelet(wt_308_depthrange,
     type = "power.corr.norm", main = "", xlab = "Time in days", ylab = "Period", plot.cb = T)

# save plots ####

