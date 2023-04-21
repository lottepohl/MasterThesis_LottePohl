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

## plot rulsif data ####

plot_rulsif_data <- function(rulsif_result = rulsif_308_res, var = "depth_median", tag_serial_num_short = "308", all_data = long_dst_date, time_vector = "date"){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  var_df <- all_data %>% dplyr::select(var %>% all_of()) %>%
    `colnames<-`("var_name")#%>%
  # mutate(var_name = ifelse(var == "depth_median", -var_name, var_name)) # 
  
  # if var contains median, min, max or mean, then inverse it to have depths plotted negatively
  if( ( grep("(median|mean|max|min)", var) %>% length() ) > 0){
    var_df <- var_df %>% 
      mutate(var_name = -var_name)
  }
  
  # change_points
  c_points <- rulsif_result$change_points %>% 
    as.data.frame() %>% 
    `colnames<-`("r_num") %>%
    mutate(c_point = TRUE)
  
  df_c_points <- dates %>% 
    mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
    left_join(c_points, by = "r_num") %>%
    dplyr::filter(c_point == TRUE) %>%
    dplyr::select(date) %>%
    dplyr::mutate(week = date %>% lubridate::week(),
                  year = date %>% lubridate::year(),
                  CP_period = 1) %>%
    mutate(week_diff = (week - dplyr::lag(week, default = week[1])) %>% abs())
  
  for(i in 2:nrow(df_c_points)){
    if(df_c_points$week_diff[i] <= 1){
      df_c_points$CP_period[i] <- df_c_points$CP_period[i-1]
    }else{df_c_points$CP_period[i] <- df_c_points$CP_period[i-1] + 1}
  }
  
  
  df_c_points_week <- 


  # plots 
  p_data <- ggplot() +
    geom_line(aes(x = dates$date, y = var_df$var_name)) +
    geom_vline(data = df_c_points, aes(xintercept = date), colour = "red", alpha = 0.6) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "depth in m")
  
  # p_data
  return(p_data)
  
}


# all_data and var list prepare ####

long_dst_date <- long_dst_date %>%
  dplyr::mutate(depth_median_change_sgolay = depth_median_change %>% signal::sgolayfilt(p = 7, n = 9),
                depth_range_change_sgolay = depth_range_change %>% signal::sgolayfilt(p = 7, n = 9),
                depth_median_sgolay = depth_median %>% signal::sgolayfilt(p = 1, n = 5),
                depth_range_sgolay = depth_range %>% signal::sgolayfilt(p = 5, n = 7),
                depth_max_sgolay = depth_max %>% signal::sgolayfilt(p = 1, n = 5),
                depth_min_sgolay = depth_min %>% signal::sgolayfilt(p = 1, n = 5))

var_list <- c("depth_median_sgolay", "depth_max_sgolay", "depth_min_sgolay")


## tag 308 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 20,
                                 alpha = 0.05)

p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
# p_308_scores_rulsif

p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308")
# p_308_data_rulsif %>% ggplotly()

grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)




## tag 321 ####

rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 23,
                                 alpha = 0.01)

p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
# p_321_scores_rulsif

p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      var = "depth_median_sgolay",
                                      tag_serial_num_short = "321")

p_321_data2_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      var = "depth_max_sgolay",
                                      tag_serial_num_short = "321")

p_321_data3_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                       all_data = long_dst_date,
                                       var = "depth_min_sgolay",
                                       tag_serial_num_short = "321")
# p_321_data_rulsif %>% ggplotly()

grid.arrange(p_321_data_rulsif, p_321_data2_rulsif, p_321_data3_rulsif, p_321_scores_rulsif, ncol = 1)

## tag 308 test with new hyperparams ####

rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 25,
                                 alpha = 0.01)

p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
# p_308_scores_rulsif

p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      var = "depth_median_sgolay",
                                      tag_serial_num_short = "308")

p_308_data2_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                       all_data = long_dst_date,
                                       var = "depth_max_sgolay",
                                       tag_serial_num_short = "308")

p_308_data3_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                       all_data = long_dst_date,
                                       var = "depth_min_sgolay",
                                       tag_serial_num_short = "308")
# p_308_data_rulsif %>% ggplotly()

grid.arrange(p_308_data_rulsif, p_308_data2_rulsif, p_308_data3_rulsif, p_308_scores_rulsif, ncol = 1)



# other plots ####

p_321 <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"),
                aes(x = date)) +
  geom_line(aes(y = -depth_min), colour = "grey") +
  # geom_line(aes(y = -depth_min %>% signal::sgolayfilt(p = 5, n = 7)), colour = "blue") +
  geom_line(aes(y = -depth_min %>% signal::sgolayfilt(p = 1, n = 5)), colour = "red")
# theme_minimal()

p_321 %>% ggplotly()

p_308 <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293308"),
                aes(x = date)) +
  geom_line(aes(y = depth_median_change), colour = "grey") +
  geom_line(aes(y = depth_median_change %>% signal::sgolayfilt(p = 7, n = 9)), colour = "blue") +
  geom_line(aes(y = depth_median_change %>% signal::sgolayfilt(p = 1, n = 3)), colour = "red") 
  # theme_minimal()

p_308 %>% ggplotly()

p_308 <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293308"),
                aes(x = date)) +
  geom_line(aes(y = depth_range_change), colour = "grey") +
# geom_line(aes(y = depth_range_change %>% signal::sgolayfilt(p = 7, n = 9)), colour = "blue") +
  geom_line(aes(y = depth_range_change %>% signal::sgolayfilt(p = 1, n = 3)), colour = "red")
# # theme_minimal()

p_308 %>% ggplotly()


# old ####

# var_list <- c("depth_range_change_sgolay","depth_median_change_sgolay", "depth_median", "depth_range")
# var_list <- c("depth_range_change","depth_median_change", "depth_median", "depth_range")
# # var_list <- c("depth_median", "depth_range")
# var_list <- "depth_median_sgolay"
# var_list <- c("depth_range_change_sgolay","depth_median_change_sgolay", "depth_median_sgolay", "depth_range_sgolay")
