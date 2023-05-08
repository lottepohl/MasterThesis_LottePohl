# script to calculate fft from putative migration periods


# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/02_results/spectral_analysis/fft/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
source(paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R"))


# Function to calc fft and psd ####
calc_fft <- function(depth_log, sample_freq){
  # `depth_log` contains the columns `depth_m` and `date_time`.
  
  # prepare/format data
  depth_log <- depth_log %>% 
    # make time vector
    mutate(t = difftime(date_time, date_time[1], units = "hours") %>% as.numeric()) %>% 
    #filter out NAs
    filter(!is.na(depth_m))
  
  # define tmax and n
  tmax <- depth_log$t %>% max()
  n <- depth_log$t %>% length()
  
  # mit nullen auffuellen bis naechste n^2
  next_power <- 2^ceiling(log2(n))
  depth_log_padded <- c(depth_log$depth_m, rep(0, next_power - n))
  
  # compute the frequency spectrum
  spec <- abs(fft(depth_log_padded))^2
  
  # compute the frequency vector
  # freq <- seq(0, length(spec) - 1) * (1/sample_freq) / length(spec)
  freq <- seq(0, length(spec) - 1) * (sample_freq) / length(spec)
  
  # make result dataframe
  result_fft <- cbind(spec, freq) %>% as.data.frame() %>% 
    # calculate period
    mutate(period = 1 / freq)
  
  return(result_fft)
}

# plot periodogram ####
plot_periodogram <- function(fft_result, tag_serial_number_short, period_upperlim = 40, period_lowerlim = 0.05, path = plot_path){
  # todo: get local rule or set values for period upper and lower lim
  periodogram <- ggplot(data = fft_result %>% filter(period < period_upperlim & period > period_lowerlim)) + geom_line(aes(x = period, y = spec), colour = "black") + theme_bw() +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw(base_size = 12) +
    scale_x_continuous(expand = c(0,0), breaks = seq(2, period_upperlim, by = 2)) +
    labs(y = "spectral density", x = "period in hours") #, title = paste0("tag ", tag_serial_number_short)
  
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".pdf"), plot = periodogram, width = 18, height = 12, units = "cm")
  # ggplot2::ggsave(filename = paste0(plot_path, "periodogram_", tag_serial_number_short, ".png"), plot = periodogram, width = 18, height = 12, units = "cm")
  return(periodogram)
}

# calc and plot subset periodograms ####

fft_calc_plot <- function(depth_log, tag_serial_num_short, start_date, end_date, sample_frequency){
  fft_res <- calc_fft(depth_log = depth_log %>% filter(tag_serial_number == paste0("1293", tag_serial_num_short),
                                                                      lubridate::date(date_time) %>% 
                                                                        between(start_date, end_date)),
                          sample_freq = sample_frequency)
  
  pgram <- plot_periodogram(fft_result = fft_res, 
                                    tag_serial_number_short = tag_serial_num_short)
  pgram %>% return()
}

# try with second interesting migration period

pgram_308_CP2 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                               start_date = rulsif_308_table_10percent$start_date[3],
                               end_date = rulsif_308_table_10percent$end_date[3] + lubridate::days(10), # add days bc otherwise very short
                               sample_frequency = 30)
pgram_308_CP2

# look at potential pupping period with weird alternating DVMs
pgram_308_potentialpupping <- fft_calc_plot(depth_log = masterias_depth_temp, 
                                            tag_serial_num_short = "308",
                               start_date = as.POSIXct("2019-06-10"),
                               end_date = as.POSIXct("2019-07-15"),
                               sample_frequency = 30)
pgram_308_potentialpupping

# raw depthlog of potential pupping time

p <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
  lubridate::date(date_time) %>% between(as.POSIXct("2019-06-10"), as.POSIXct("2019-07-25")),
                                                   row_number() %% 10 == 0), aes(x = date_time, y = -depth_m)) +
  geom_point()

p #%>% ggplotly()

p2 <- ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
  lubridate::date(date_time) %>% between(as.POSIXct("2019-05-11"), as.POSIXct("2019-08-15"))
                                                    # ,row_number() %% 5 == 0
  ), aes(x = date_time, y = -depth_m, colour = lubridate::hour(date_time))) + #, colour = lubridate::hours(date_time)
  geom_point() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female, tag 308")

p2 #%>% ggplotly()

data_test <- masterias_depth_temp %>% 
  filter(lubridate::date(date_time) %>% 
                                  between(as.POSIXct("2019-06-16"), as.POSIXct("2019-06-18")),
                                row_number() %% 5 == 0) %>% 
  mutate(hour = lubridate::hour(date_time)) # %>% as.factor())

p3 <- ggplot(data = data_test, aes(x = date_time, y = -depth_m, colour = hour)) + #, colour = lubridate::hours(date_time)
  geom_point() +
  labs(y = "depth in m", x = "date", colour = "hour of the day")

p3 #%>% ggplotly()

# plot female and male behaviour in summer
p4 <- ggplot(data = masterias_depth_temp %>% filter(lubridate::date(date_time) %>% between(as.POSIXct("2019-05-11"), as.POSIXct("2019-08-15"))
                                                    ,tag_serial_number %in% c("1293308", "1293321")
), aes(x = date_time, y = -depth_m, colour = tag_serial_number)) + #lubridate::hour(date_time)
  geom_line() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female, tag 308")

p4 %>% ggplotly()

# make fft subsets ####

rulsif_308_table_10percent %>% View()
sample_frequency <- 30 # 1 sample per 2 min == 30 samples per 60 min == 30 [1/hour]


fft_308_CP1 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
                                                                    lubridate::date(date_time) %>% 
                                                                      between(rulsif_308_table_10percent$start_date[1], rulsif_308_table_10percent$end_date[1])),
                        sample_freq = sample_frequency)


# # check that data subsetting worked
# ggplot(data = masterias_depth_temp %>% filter(tag_serial_number == "1293308",
#                                               date_time %>% 
#                                                 between(rulsif_308_table_10percent$start_date[1], 
#                                                         rulsif_308_table_10percent$end_date[1]))) +
#   geom_line(aes(x = date_time, y = -depth_m))

## plot periodograms ####

pgram_308_CP1 <- plot_periodogram(fft_result = fft_308_CP1, 
                              tag_serial_number_short = "308")
pgram_308_CP1
