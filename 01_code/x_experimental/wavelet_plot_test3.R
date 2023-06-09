
library(scales)
library(biwavelet)
library(ggplot2)
library(dplyr)

# rm(list = ls())

dir_path_new <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
source(paste0(dir_path_new, "/01_code/06_functions/functions.R"))
source(paste0(dir_path_new, "/01_code/02_load_data/load_wavelet_results.R"))
source(paste0(dir_path_new, "/01_code/06_functions/compute_wavelettransform.R"))
paste0(dir_path_new, "/01_code/05_plots_maps/plots_dst_summary.R") %>% base::source()
paste0(dir_path_new, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()

dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)

wt_df_308_depthrange <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange) #wt_308_depthrange$xaxis %>% as.data.frame()
wt_df_308_mediandepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth) #wt_308_depthrange$xaxis %>% as.data.frame()

p_wave_308_depthchange <- plot_wavelet_gg2(wt_df = wt_df_308_depthrange, type = "power_log", date = F)
p_wave_308_depthchange #%>% ggplotly()

p_wave_308_mediandepth <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth, type = "power_log", date = F)
p_wave_308_mediandepth #%>% ggplotly()


ggplot(data = wt_df) +
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
  # scale_fill_distiller(palette ="RdYlGn") +
  labs(x = "dates", y = "period in hours", fill = "log2(power)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5))


my_function <- function(dataframe, x_value) {
  ggplot(data = dataframe, aes(x = {{x_value}}, y = y)) +
    geom_point()
}


test_df <- data.frame(t = (1:100),
                      y = rnorm(1:100))

test_plot <- my_function(dataframe = test_df, 
                         x_value = t)
test_plot


my_function <- function(dataframe, x_value) {
  ggplot(data = dataframe, aes(x = .data[[x_value]], y = y)) +
    geom_point()
}

# create a test dataframe with a column of values
test_df <- data.frame(values = c(0, 64, 128, 192, 256))

# find the closest power of 2 to the maximum value in the values column
closest_power_of_2 <- 2^floor(log2(wt_df_308_depthrange$period)) %>% unique()
# closest_power_of_2 <- 2^floor(log2(max(test_df$values)))
closest_power_of_2 %>% unique()

# create some test values
test_values <- c(141, 156, 176, 188)

# round each value to the nearest 50
rounded_values <- round(test_values / 50) * 50

rounded_values
