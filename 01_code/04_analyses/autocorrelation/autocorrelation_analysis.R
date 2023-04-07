# Script to carry out autocorrelation analysis on longterm dst plots


# library(biwavelet)
library(dplyr)
library(plotly)
library(tibble)
# library(zoo)

rm(list = ls())
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
source(paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R"))

plot_path <- paste0(getwd(), "/02_results/dst_autocorrelation/")

# long term dsts ####

## tag 321 ####

acf_321 <- acf(data_321_summary_wt_all$depth_median_roll3, lag.max = 400, type = "correlation", plot = F) 

acf_321_df <- data.frame(acf = acf_321$acf,
                        lag = acf_321$lag)

p_acf_321 <- ggplot(data = acf_321_df, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  theme_minimal() +
  labs(y = "autocorrelation", x = "lag in days", title = "tag 321 (male), daily median depth roll3")
p_acf_321 %>% ggplotly()

## tag 308 ####

acf_308 <- acf(data_308_summary_wt_all$depth_median_roll3, lag.max = 400, type = "correlation", plot = F) 

acf_308_df <- data.frame(acf = acf_308$acf,
                         lag = acf_308$lag)

p_acf_308 <- ggplot(data = acf_308_df, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  theme_minimal() +
  labs(y = "autocorrelation", x = "lag in days", title = "tag 308 (female), daily median depth roll 3")
p_acf_308 %>% ggplotly()

# save data ####
save_data(data = acf_321_df, folder = plot_path)
save_data(data = acf_308_df, folder = plot_path)
save_data(data = p_acf_321, folder = plot_path)
save_data(data = p_acf_308, folder = plot_path)

# load_data(filestring = "p_acf_308", folder = plot_path)

# short term dsts ####

# ## tag 295 ####
# 
# acf_295 <- acf(masterias_depth_date %>% 
#                  ungroup() %>%
#                  filter(tag_serial_number == "1293295") %>%
#                  dplyr::select(depth_median), lag.max = 400, type = "correlation", plot = F) 
# 
# acf_295_df <- data.frame(acf = acf_295$acf,
#                          lag = acf_295$lag)
# 
# p_acf_295 <- ggplot(data = acf_295_df, mapping = aes(x = lag, y = acf)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_segment(mapping = aes(xend = lag, yend = 0)) +
#   theme_minimal() +
#   labs(y = "autocorrelation", x = "lag in days", title = "tag 295 (male)")
# p_acf_295 %>% ggplotly()
# 
# ## tag 319 ####
# 
# acf_319 <- acf(masterias_depth_date %>% 
#                  ungroup() %>%
#                  filter(tag_serial_number == "1293319") %>%
#                  dplyr::select(depth_median), lag.max = 400, type = "correlation", plot = F) 
# 
# acf_319_df <- data.frame(acf = acf_319$acf,
#                          lag = acf_319$lag)
# 
# p_acf_319 <- ggplot(data = acf_319_df, mapping = aes(x = lag, y = acf)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_segment(mapping = aes(xend = lag, yend = 0)) +
#   theme_minimal() +
#   labs(y = "autocorrelation", x = "lag in days", title = "tag 319 (male)")
# p_acf_319 %>% ggplotly()
