# Script to perform RuLSIF change point detection analysis

# install.packages("devtools")
library(ggplot2)
# library(plotly)
library(dplyr)
library(rulsif.ts)
library(tidyverse)
# library(gridExtra)

# rm(list = ls())

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()

## function to compute rulsif ####
compute_rulsif <- function(all_data, tag_serial_num_short, vars, time_vector = "date", thresh = 0.9, alpha = 0.05, step = 15, window_size = 5){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  df_rulsif <- all_data %>% 
    dplyr::select(vars %>% all_of()) %>% 
    as.matrix(nrow = vars %>% length()) %>% 
    t()
  
  result <- rulsif.ts::ts_detect(df_rulsif, thresh = thresh, alpha = alpha, step = step, window_size = window_size, make_plot = F)
  
  return(result)
}

# var_list_change <- c("depth_median","depth_median_change", "depth_min_change", "depth_max_change", "depth_range_change")

var_list <- c("depth_mean","depth_median_change", "depth_min_change", "depth_max_change", "depth_range_change","depth_median_change", "depth_var", "depth_median", "depth_min", "depth_max", "depth_range", "vertical_speed_max")
var_list <- c("depth_median_change", "depth_min_change", "depth_max_change", "depth_range_change","depth_median_change", "depth_median", "depth_min", "depth_max", "depth_range")
var_list <- c("depth_median_change", "depth_range_change","depth_median_change", "depth_median", "depth_range")

rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.9,
                                 window_size = 5,
                                 step = 10,
                                 alpha = 0.1)

p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
# p_308_scores_rulsif

p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308")
# p_308_data_rulsif %>% ggplotly()

grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1) %>% ggplotly()





p_308 <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293308"),
                aes(x = date, y = depth_median_change)) +
  geom_line() +
  theme_minimal()

p_308 %>% ggplotly()
