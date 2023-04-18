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
vars <- c("depth_range", "depth_median")
all_data <- long_dst_date
time_vector <- "date"
df <- long_dst_date %>% dplyr::select(vars %>% all_of())
dates <- all_data %>% dplyr::select(time_vector %>% all_of())

df_rulsif <- data %>% as.matrix(nrow = data %>% ncol()) %>% t()
result <- rulsif.ts::ts_detect(df_rulsif, thresh = 0.9, alpha = 0.01, step = 5, window_size = 5, make_plot = F)

# p_scores <- ggplot(data = ) +
  


