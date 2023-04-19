# install.packages("devtools")
library(ggplot2)
library(plotly)
library(dplyr)
library(rulsif.ts)
library(tidyverse)
library(gridExtra)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()


# tests ts_detect() #####

## test1 ####
series <- c(
  rnorm(50, mean = 0, sd = 0.3),
  rnorm(25, mean = 8, sd = 1),
  rnorm(75, mean = 3, sd = 0.6),
  rnorm(25, mean = 1, sd = 0.8),
  rnorm(100, mean = -5, sd = 1.5),
  rnorm(100, mean = -5, sd = 0.2),
  rnorm(50, mean = -2.5, sd = 0.4),
  rnorm(50, mean = 2, sd = 1.2)
)

ts_detect(series, window_size = 3, step = 10, make_plot = TRUE)

## test2 ####

s <- c(rnorm(150, mean = 0), rnorm(150, mean = 5), rnorm(150, mean = 1))
s <- matrix(s, nrow = 1)
ts_detect(s, step = 10 ,window_size = 5, make_plot = TRUE)


## test 3: tag 321 ####

df_321 <- long_dst_date %>% dplyr::filter(tag_serial_number == "1293321") %>% 
  dplyr::select(depth_median) %>% mutate(depth_median = -depth_median) %>% pull() %>%
  matrix(nrow = 1)

rulsif_321_res <- ts_detect(df_321, thresh = 0.9, alpha = 0.01, step = 5, window_size = 5, make_plot = T)

p_depth <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"),
                  aes(x = date, y = -depth_median)) +
  geom_line() +
  theme_minimal()
p_depth %>% ggplotly()

## test 4: tag 321, Dim > 1 ####

df_321 <- long_dst_date %>% dplyr::filter(tag_serial_number == "1293321") %>% 
  dplyr::select(depth_median, depth_range) %>% mutate(depth_median = -depth_median) %>% #pull(depth_median, depth_range) %>% View()
  as.matrix(nrow = 2) %>% t()

rulsif_321_res <- ts_detect(df_321, thresh = 0.9, alpha = 0.01, step = 5, window_size = 5, make_plot = T)

p_depth <- ggplot(data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"),
                  aes(x = date, y = -depth_median)) +
  geom_line() +
  theme_minimal()
p_depth %>% ggplotly()


# test gridextra ####
p1 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()
p2 <- ggplot(data = mtcars, aes(x = wt, y = qsec)) + geom_point()
grid.arrange(p1, p2, ncol = 1)


# function to plot results of rulsif ####

# vars <- c("depth_range", "depth_median")
# all_data <- long_dst_date %>% dplyr::filter(tag_serial_number == "1293308")
# time_vector <- "date"

compute_rulsif <- function(all_data, vars, time_vector = "date", thresh = 0.9, alpha = 0.05, step = 15, window_size = 5){
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  df_rulsif <- all_data %>% 
    dplyr::select(vars %>% all_of()) %>% 
    as.matrix(nrow = vars %>% length()) %>% 
    t()
  
  result <- rulsif.ts::ts_detect(df_rulsif, thresh = thresh, alpha = alpha, step = step, window_size = window_size, make_plot = F)
  
  return(result)
}

plot_rulsif <- function(rulsif_result, all_data, time_vector = "date"){
  
  dates <- all_data %>% dplyr::select(time_vector %>% all_of())
  
  row_diff <- ((nrow(dates) - length(result$scores)) / 2) %>% floor()
  
  # scores 
  scores <- result$scores %>% 
    as.data.frame() %>% 
    `colnames<-`("score") %>%
    mutate(r_num = seq(from = row_diff,
                       to = row_diff + length(result$scores) - 1,
                       by = 1))
  
  df_scores <- dates %>% 
    mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
    left_join(scores, by = "r_num")
  
  # change_points
  c_points <- result$change_points %>% 
    as.data.frame() %>% 
    `colnames<-`("r_num") %>%
    mutate(c_point = TRUE)
  
  df_c_points <- dates %>% 
    mutate(r_num = seq(from = 1, to = nrow(dates))) %>%
    left_join(c_points, by = "r_num") %>%
    dplyr::filter(c_point == TRUE) %>%
    dplyr::select(date)
  
  
  # plots 
  p_scores <- ggplot(data = df_scores, aes(x = date, y = score)) +
    geom_line(colour = "darkgrey") + 
    labs(x = "", y = "rPE score") #+
  # theme_minimal()
  
  # p_scores
  
  p_data <- ggplot() +
    geom_line(data = all_data, aes(x = date, y = -depth_median)) +
    geom_vline(data = df_c_points, aes(xintercept = date), colour = "red") +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "", y = "depth in m")
  
  # p_data
  plots_result <- c(p_data, p_scores)
  return(plots_result)
  # grid.arrange(p_data, p_scores, ncol = 1) 
}

# test functions ####

rulsif_321_res <- compute_rulsif(all_data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"),
                                 vars = c("depth_median", "depth_range", "depth_max", "depth_range_change"))

p_321_rulsif <- plot_rulsif(rulsif_result = result, all_data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"))

rulsif_321_res %>% plot_rulsif(all_data = long_dst_date %>% dplyr::filter(tag_serial_number == "1293321"))

