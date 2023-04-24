# Script to perform RuLSIF change point detection analysis

# install.packages("devtools")
library(ggplot2)
# library(plotly)
library(dplyr)
library(rulsif.ts)
library(tidyverse)
# library(gridExtra)

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## plot path ####
rulsif_data_path <- paste0(dir_path, "/02_results/dst_changepointdetection/")

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()



## function to compute rulsif ####
compute_rulsif <- function(all_data = long_dst_date, tag_serial_num_short = "321", vars = var_list, time_vector = "date", thresh = 0.95, alpha = 0.05, step_percent = 5, window_size = 7){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  # define step length before data are padded
  step <- ((all_data %>% nrow()) * (step_percent / 100)) %>% base::round()
  
  # add step length days of data from the last day
  pad_data_end <- tibble(date = seq(from = all_data$date %>% max() + lubridate::days(1), to = (all_data$date %>% max()) + lubridate::days(step), by = "day")) %>%
    mutate(depth_median_sgolay = (mean(all_data$depth_median_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5),
           depth_max_sgolay = (mean(all_data$depth_max_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5),
           depth_min_sgolay = (mean(all_data$depth_min_sgolay[(nrow(all_data) - 10) :nrow(all_data)]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5))
  
  pad_data_start <- tibble(date = seq(from = all_data$date %>% min() - lubridate::days(step), to = (all_data$date %>% min()) - lubridate::days(1), by = "day")) %>%
    mutate(depth_median_sgolay = (mean(all_data$depth_median_sgolay[1:10]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5),
           depth_max_sgolay = (mean(all_data$depth_max_sgolay[1:10]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5),
           depth_min_sgolay = (mean(all_data$depth_min_sgolay[1:10]) + rnorm(n = step)) %>% signal::sgolayfilt(p = 1, n = 5))

  # pad data
  all_data <- all_data %>%
    full_join(pad_data_start, by = join_by(date, depth_median_sgolay, depth_max_sgolay, depth_min_sgolay), multiple = "all") %>%
    full_join(pad_data_end, by = join_by(date, depth_median_sgolay, depth_max_sgolay, depth_min_sgolay), multiple = "all") %>% 
    arrange(date)

  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  df_rulsif <- all_data %>% 
    dplyr::select(vars %>% all_of()) #%>%
  df_rulsif <- -df_rulsif
  df_rulsif <- df_rulsif %>%
    as.matrix(nrow = vars %>% length()) %>% 
    t()
  
  .Random.seed <- NULL
  result <- rulsif.ts::ts_detect(df_rulsif, thresh = thresh, alpha = alpha, step = step, window_size = window_size, make_plot = F)
  .Random.seed <- NULL
  return(result)
}

# # test ####
# p <- ggplot(data = all_data) +
#   geom_line(aes(x = date, y = -depth_median_sgolay))

p %>% ggplotly()

# functions `plot_rulsif_scores()` and `plot_rulsif_data()` in "./01_code/05_plots_maps/thesis_manuscript_figures.R"

## get change point periods ####
# not necessarily needed for the analysis but potentially helpful for the manuscript (to e.g. generate a table)
get_change_point_periods <- function(rulsif_result, tag_serial_num_short, all_data, time_vector = "date"){
  
  all_data <- all_data %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short))
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
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
  
  df_c_points_week <- df_c_points %>% 
    # dplyr::ungroup() %>%
    dplyr::group_by(CP_period) %>%
    dplyr::mutate(start_date = min(date, na.rm = T),
                  end_date = max(date, na.rm = T)) %>%
    dplyr::select(CP_period, start_date, end_date) %>%
    # mutate(CP_period = CP_period %>% as.factor()) %>%
    distinct() %>%
    dplyr::mutate(step = rulsif_result$step)
  
  return(df_c_points_week)
}

# var list prepare ####

var_list <- c("depth_median_sgolay", "depth_max_sgolay", "depth_min_sgolay")


## tag 308 ####

### step=2.5%  ####
#### result 
rulsif_308_res_2_5percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "308",
                                           vars = var_list,
                                           step_percent = 2.5)
#### plots 
p_308_scores_rulsif_2_5percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_2_5percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "308",
                                                    thresh = 0.95)
p_308_ribbon_rulsif_2_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_2_5percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "308") 

# p_308_data1_rulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_2_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_datarulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_2_5percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_data3_rulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_2_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "308")
# grid.arrange(p_308_data1_rulsif_2_5percent, p_308_datarulsif_2_5percent, p_308_data3_rulsif_2_5percent, p_308_scores_rulsif_2_5percent, ncol = 1)

grid.arrange(p_308_ribbon_rulsif_2_5percent, p_308_scores_rulsif_2_5percent, ncol = 1)

#### table
rulsif_308_table_2_5percent <- get_change_point_periods(rulsif_result = rulsif_308_res_2_5percent, 
                                                       tag_serial_num_short = "308",
                                                       all_data = long_dst_date)

### step=5%  ####
#### result 
rulsif_308_res_5percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "308",
                                           vars = var_list,
                                           step_percent = 10)
#### plots 
p_308_scores_rulsif_5percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_5percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "308",
                                                    thresh = 0.95)
p_308_ribbon_rulsif_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_5percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "308") 

# p_308_data1_rulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_datarulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_5percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_data3_rulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "308")
# grid.arrange(p_308_data1_rulsif_5percent, p_308_datarulsif_5percent, p_308_data3_rulsif_5percent, p_308_scores_rulsif_5percent, ncol = 1)

grid.arrange(p_308_ribbon_rulsif_5percent, p_308_scores_rulsif_5percent, ncol = 1)

#### table
rulsif_308_table_5percent <- get_change_point_periods(rulsif_result = rulsif_308_res_5percent, 
                                                       tag_serial_num_short = "308",
                                                       all_data = long_dst_date)

### step=10%  ####
#### result 
rulsif_308_res_10percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "308",
                                           vars = var_list,
                                           step_percent = 10)
#### plots 
p_308_scores_rulsif_10percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_10percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "308",
                                                    thresh = 0.95)
p_308_ribbon_rulsif_10percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_10percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "308") 

# p_308_data1_rulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_10percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_datarulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_10percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_data3_rulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_10percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "308")
# grid.arrange(p_308_data1_rulsif_10percent, p_308_datarulsif_10percent, p_308_data3_rulsif_10percent, p_308_scores_rulsif_10percent, ncol = 1)

grid.arrange(p_308_ribbon_rulsif_10percent, p_308_scores_rulsif_10percent, ncol = 1)

#### table
rulsif_308_table_10percent <- get_change_point_periods(rulsif_result = rulsif_308_res_10percent, 
                                                       tag_serial_num_short = "308",
                                                       all_data = long_dst_date)

### step=15%  ####
#### result 
rulsif_308_res_15percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "308",
                                           vars = var_list,
                                           step_percent = 15)
#### plots 
p_308_scores_rulsif_15percent <- plot_rulsif_scores(rulsif_result = rulsif_308_res_15percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "308",
                                                    thresh = 0.95)
p_308_ribbon_rulsif_15percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_308_res_15percent,
                                                 all_data = long_dst_date,
                                                 var = var_list,
                                                 tag_serial_num_short = "308") 

# p_308_data1_rulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_15percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_datarulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_15percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "308") +
#   theme(legend.position = "none")
# p_308_data3_rulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_308_res_15percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "308")
# grid.arrange(p_308_data1_rulsif_15percent, p_308_datarulsif_15percent, p_308_data3_rulsif_15percent, p_308_scores_rulsif_15percent, ncol = 1)

grid.arrange(p_308_ribbon_rulsif_15percent, p_308_scores_rulsif_15percent, ncol = 1)

#### table
rulsif_308_table_15percent <- get_change_point_periods(rulsif_result = rulsif_308_res_15percent, 
                                                       tag_serial_num_short = "308",
                                                       all_data = long_dst_date)

## tag 321 ####

### step=2.5%  ####
#### result 
rulsif_321_res_2_5percent <- compute_rulsif(all_data = long_dst_date,
                                            tag_serial_num_short = "321",
                                            vars = var_list,
                                            step_percent = 2.5)
#### plots 
p_321_scores_rulsif_2_5percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_2_5percent,
                                                     all_data = long_dst_date,
                                                     tag_serial_num_short = "321",
                                                     thresh = 0.95)
p_321_ribbon_rulsif_2_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_2_5percent,
                                                          all_data = long_dst_date,
                                                          var = var_list,
                                                          tag_serial_num_short = "321") 

# p_321_data1_rulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_2_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_datarulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_2_5percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_data3_rulsif_2_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_2_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "321")
# grid.arrange(p_321_data1_rulsif_2_5percent, p_321_datarulsif_2_5percent, p_321_data3_rulsif_2_5percent, p_321_scores_rulsif_2_5percent, ncol = 1)

grid.arrange(p_321_ribbon_rulsif_2_5percent, p_321_scores_rulsif_2_5percent, ncol = 1)

#### table
rulsif_321_table_2_5percent <- get_change_point_periods(rulsif_result = rulsif_321_res_2_5percent, 
                                                        tag_serial_num_short = "321",
                                                        all_data = long_dst_date)

### step=5%  ####
#### result 
rulsif_321_res_5percent <- compute_rulsif(all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          vars = var_list,
                                          step_percent = 10)
#### plots 
p_321_scores_rulsif_5percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_5percent,
                                                   all_data = long_dst_date,
                                                   tag_serial_num_short = "321",
                                                   thresh = 0.95)
p_321_ribbon_rulsif_5percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_5percent,
                                                        all_data = long_dst_date,
                                                        var = var_list,
                                                        tag_serial_num_short = "321") 

# p_321_data1_rulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_datarulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_5percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_data3_rulsif_5percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_5percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "321")
# grid.arrange(p_321_data1_rulsif_5percent, p_321_datarulsif_5percent, p_321_data3_rulsif_5percent, p_321_scores_rulsif_5percent, ncol = 1)

grid.arrange(p_321_ribbon_rulsif_5percent, p_321_scores_rulsif_5percent, ncol = 1)

#### table
rulsif_321_table_5percent <- get_change_point_periods(rulsif_result = rulsif_321_res_5percent, 
                                                      tag_serial_num_short = "321",
                                                      all_data = long_dst_date)

### step=10%  ####
#### result 
rulsif_321_res_10percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "321",
                                           vars = var_list,
                                           step_percent = 10)
#### plots 
p_321_scores_rulsif_10percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_10percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "321",
                                                    thresh = 0.95)
p_321_ribbon_rulsif_10percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_10percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "321") 

# p_321_data1_rulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_10percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_datarulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_10percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_data3_rulsif_10percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_10percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "321")
# grid.arrange(p_321_data1_rulsif_10percent, p_321_datarulsif_10percent, p_321_data3_rulsif_10percent, p_321_scores_rulsif_10percent, ncol = 1)

grid.arrange(p_321_ribbon_rulsif_10percent, p_321_scores_rulsif_10percent, ncol = 1)

#### table
rulsif_321_table_10percent <- get_change_point_periods(rulsif_result = rulsif_321_res_10percent, 
                                                       tag_serial_num_short = "321",
                                                       all_data = long_dst_date)

### step=15%  ####
#### result 
rulsif_321_res_15percent <- compute_rulsif(all_data = long_dst_date,
                                           tag_serial_num_short = "321",
                                           vars = var_list,
                                           step_percent = 15)
#### plots 
p_321_scores_rulsif_15percent <- plot_rulsif_scores(rulsif_result = rulsif_321_res_15percent,
                                                    all_data = long_dst_date,
                                                    tag_serial_num_short = "321",
                                                    thresh = 0.95)
p_321_ribbon_rulsif_15percent <- plot_rulsif_data_ribbon(rulsif_result = rulsif_321_res_15percent,
                                                         all_data = long_dst_date,
                                                         var = var_list,
                                                         tag_serial_num_short = "321") 

# p_321_data1_rulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_15percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_median_sgolay",
#                                                  tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_datarulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_15percent,
#                                                all_data = long_dst_date,
#                                                var = "depth_max_sgolay",
#                                                tag_serial_num_short = "321") +
#   theme(legend.position = "none")
# p_321_data3_rulsif_15percent <- plot_rulsif_data(rulsif_result = rulsif_321_res_15percent,
#                                                  all_data = long_dst_date,
#                                                  var = "depth_min_sgolay",
#                                                  tag_serial_num_short = "321")
# grid.arrange(p_321_data1_rulsif_15percent, p_321_datarulsif_15percent, p_321_data3_rulsif_15percent, p_321_scores_rulsif_15percent, ncol = 1)

grid.arrange(p_321_ribbon_rulsif_15percent, p_321_scores_rulsif_15percent, ncol = 1)

#### table
rulsif_321_table_15percent <- get_change_point_periods(rulsif_result = rulsif_321_res_15percent, 
                                                       tag_serial_num_short = "321",
                                                       all_data = long_dst_date)

# save data ####
save_data(data = rulsif_308_res_2_5percent, folder = rulsif_data_path)
save_data(data = rulsif_308_table_2_5percent, folder = rulsif_data_path)
save_data(data = rulsif_308_res_5percent, folder = rulsif_data_path)
save_data(data = rulsif_308_table_5percent, folder = rulsif_data_path)
save_data(data = rulsif_308_res_10percent, folder = rulsif_data_path)
save_data(data = rulsif_308_table_10percent, folder = rulsif_data_path)
save_data(data = rulsif_308_res_15percent, folder = rulsif_data_path)
save_data(data = rulsif_308_table_15percent, folder = rulsif_data_path)

save_data(data = rulsif_321_res_5percent, folder = rulsif_data_path)

# other plots 

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

## tag 308 ####
### step = 20 ####
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
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 21 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 21,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 22 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 22,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 23 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 23,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 24 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 24,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 25 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 25,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 17 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 17,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

### step = 26 ####
rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "308",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 26,
                                 alpha = 0.05)
p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "308",
                                          thresh = 0.95)
p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "308")
grid.arrange(p_308_data_rulsif, p_308_scores_rulsif, ncol = 1)

## tag 321 ####

### step = 24 ####
rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 24,
                                 alpha = 0.05)
p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "321")
grid.arrange(p_321_data_rulsif, p_321_scores_rulsif, ncol = 1)

### step = 25 ####
rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 25,
                                 alpha = 0.05)
p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "321")
grid.arrange(p_321_data_rulsif, p_321_scores_rulsif, ncol = 1)

### step = 26 ####
rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 26,
                                 alpha = 0.05)
p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "321")
grid.arrange(p_321_data_rulsif, p_321_scores_rulsif, ncol = 1)

### step = 27 ####
rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 27,
                                 alpha = 0.05)
p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "321")
grid.arrange(p_321_data_rulsif, p_321_scores_rulsif, ncol = 1)

### step = 28 ####
rulsif_321_res <- compute_rulsif(all_data = long_dst_date,
                                 tag_serial_num_short = "321",
                                 vars = var_list,
                                 thresh = 0.95,
                                 window_size = 7,
                                 step = 28,
                                 alpha = 0.05)
p_321_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_321_res,
                                          all_data = long_dst_date,
                                          tag_serial_num_short = "321",
                                          thresh = 0.95)
p_321_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_321_res,
                                      all_data = long_dst_date,
                                      tag_serial_num_short = "321")
grid.arrange(p_321_data_rulsif, p_321_scores_rulsif, ncol = 1)


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


# ## tag 308 test with new hyperparams ####
# 
# rulsif_308_res <- compute_rulsif(all_data = long_dst_date,
#                                  tag_serial_num_short = "308",
#                                  vars = var_list,
#                                  thresh = 0.95,
#                                  window_size = 7,
#                                  step = 25,
#                                  alpha = 0.01)
# 
# p_308_scores_rulsif <- plot_rulsif_scores(rulsif_result = rulsif_308_res,
#                                           all_data = long_dst_date,
#                                           tag_serial_num_short = "308",
#                                           thresh = 0.95)
# # p_308_scores_rulsif
# 
# p_308_data_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
#                                       all_data = long_dst_date,
#                                       var = "depth_median_sgolay",
#                                       tag_serial_num_short = "308")
# 
# p_308_data2_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
#                                        all_data = long_dst_date,
#                                        var = "depth_max_sgolay",
#                                        tag_serial_num_short = "308")
# 
# p_308_data3_rulsif <- plot_rulsif_data(rulsif_result = rulsif_308_res,
#                                        all_data = long_dst_date,
#                                        var = "depth_min_sgolay",
#                                        tag_serial_num_short = "308")
# # p_308_data_rulsif %>% ggplotly()
# 
# grid.arrange(p_308_data_rulsif, p_308_data2_rulsif, p_308_data3_rulsif, p_308_scores_rulsif, ncol = 1)
# 




# var_list <- c("depth_range_change_sgolay","depth_median_change_sgolay", "depth_median", "depth_range")
# var_list <- c("depth_range_change","depth_median_change", "depth_median", "depth_range")
# # var_list <- c("depth_median", "depth_range")
# var_list <- "depth_median_sgolay"
# var_list <- c("depth_range_change_sgolay","depth_median_change_sgolay", "depth_median_sgolay", "depth_range_sgolay")
