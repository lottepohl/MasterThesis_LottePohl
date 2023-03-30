# WORKSPACE ####
library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(psdr)

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/02_results/spectral_analysis/fft/")

source(paste0(dir_path, "/01_code/04_analyses/FFT/calculate_fft_psd.R"))

plot_periodogram <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05, path = plot_path){
  # todo: get local rule or set values for period upper and lower lim
  periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + geom_line(aes(x = period, y = spec), colour = "black") + theme_bw() +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 12) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  
  ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
  ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".png"), plot = periodogram, width = 18, height = 12, units = "cm")
  return(periodogram)
}

pgram_295 <- plot_periodogram(fft_result = fft_295, 
                              tag_serial_number_short = "295")

pgram_304 <- plot_periodogram(fft_result = fft_304, 
                              tag_serial_number_short = "304")

pgram_308 <- plot_periodogram(fft_result = fft_308, 
                              tag_serial_number_short = "308")

pgram_310 <- plot_periodogram(fft_result = fft_310, 
                              tag_serial_number_short = "310")

pgram_312 <- plot_periodogram(fft_result = fft_312, 
                              tag_serial_number_short = "312")

pgram_319 <- plot_periodogram(fft_result = fft_319, 
                              tag_serial_number_short = "319")

pgram_321 <- plot_periodogram(fft_result = fft_321, 
                              tag_serial_number_short = "321")

pgram_295 <- plot_periodogram(fft_result = fft_295, 
                              tag_serial_number_short = "295")

pgram_322 <- plot_periodogram(fft_result = fft_322, 
                              tag_serial_number_short = "322")