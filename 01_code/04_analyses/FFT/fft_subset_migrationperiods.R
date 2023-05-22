# script to calculate fft from putative migration periods

library(dplyr)
library(lubridate)
library(plotly)
library(pracma)
library(psdr)
library(ggplot2)

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
    dplyr::filter(!is.na(depth_m))
  
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
  periodogram <- ggplot(data = fft_result %>% dplyr::filter(period < period_upperlim & period > period_lowerlim)) + geom_line(aes(x = period, y = spec), colour = "black") + theme_bw() +
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
  fft_res <- calc_fft(depth_log = depth_log %>% dplyr::filter(tag_serial_number == paste0("1293", tag_serial_num_short),
                                                                      lubridate::date(date_time) %>% 
                                                                        between(start_date, end_date)),
                          sample_freq = sample_frequency)
  
  pgram <- plot_periodogram(fft_result = fft_res, 
                                    tag_serial_number_short = tag_serial_num_short)
  pgram %>% return()
}


# make fft subsets ####

## tag 308 ####

### potential winter mig ####

p_308_wintermig <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-09-20"), as.POSIXct("2018-10-15")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-09-21"), as.POSIXct("2018-09-25")),
                                                                 row_number() %% 1 == 0), aes(x = date_time, y = -depth_m)) +
  # geom_point(colour = lubridate::hour(date_time)) +
  geom_line() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential winter migration")

p_308_wintermig %>% ggplotly()

pgram_308_wintermig1 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                   start_date = as.POSIXct("2018-09-25"),
                                   # end_date = as.POSIXct("2018-09-30"), 
                                   end_date = as.POSIXct("2018-10-11"), 
                                   sample_frequency = 30)
pgram_308_wintermig1

pgram_308_wintermigpause <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                      start_date = as.POSIXct("2018-10-01"),
                                      end_date = as.POSIXct("2018-10-11"),
                                      sample_frequency = 30)
pgram_308_wintermigpause

p_308_wintermigpause <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-09-01"), as.POSIXct("2018-09-25")), #to see if the shark rests the whole time
                                                                 lubridate::date(date_time) %>% between(as.POSIXct("2018-09-29"), as.POSIXct("2018-10-07")),
                                                                 row_number() %% 1 == 0), aes(x = date_time, y = -depth_m, colour = lubridate::hour(date_time))) +
  geom_point() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential winter migration (pause?)")

p_308_wintermigpause # %>% ggplotly()

pgram_308_wintermig2 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                      start_date = as.POSIXct("2018-10-07"),
                                      end_date = as.POSIXct("2018-10-11"),
                                      sample_frequency = 30)
pgram_308_wintermig2

pgram_308_wintermigtotal <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                      start_date = as.POSIXct("2018-09-25"),
                                      end_date = as.POSIXct("2018-10-11"), 
                                      sample_frequency = 30)
pgram_308_wintermigtotal

pgram_308_beforewintermig <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                          start_date = as.POSIXct("2018-09-01"),
                                          end_date = as.POSIXct("2018-09-25"), 
                                          sample_frequency = 30)
pgram_308_beforewintermig


### winter residency ####

p_308_winterres <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                      # lubridate::date(date_time) %>% between(as.POSIXct("2018-11-01"), as.POSIXct("2019-04-15")), #to see if the shark rests the whole time
                                                                      # lubridate::date(date_time) %>% between(as.POSIXct("2018-12-05"), as.POSIXct("2018-12-20")),
                                                                      row_number() %% 1 == 0), aes(x = date_time, y = -depth_m)) +
  # geom_point(aes(colour = lubridate::hour(date_time))) +
  geom_line() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential winter residency")

p_308_winterres %>% ggplotly()

pgram_308_winterres_resting <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                           start_date = as.POSIXct("2018-10-11"),
                                           end_date = as.POSIXct("2018-12-05"),
                                           # end_date = as.POSIXct("2018-12-15"),
                                           # end_date = as.POSIXct("2019-03-10"), 
                                           sample_frequency = 30)
pgram_308_winterres_resting

pgram_308_winterres_resting_test_short <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                             # start_date = as.POSIXct("2019-03-13"),
                                             # end_date = as.POSIXct("2019-03-17"),
                                             start_date = as.POSIXct("2018-12-04"),
                                             end_date = as.POSIXct("2018-12-13"), 
                                             sample_frequency = 30)
pgram_308_winterres_resting_test_short

pgram_308_winterres_feeding <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                             # start_date = as.POSIXct("2018-10-11"),
                                             start_date = as.POSIXct("2018-12-01"),
                                             # end_date = as.POSIXct("2018-12-10"),
                                             end_date = as.POSIXct("2019-03-10"),
                                             sample_frequency = 30)
pgram_308_winterres_feeding

pgram_308_winterres_feeding_detail <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                                    start_date = as.POSIXct("2018-12-05"),
                                                    end_date = as.POSIXct("2018-12-20"),
                                             sample_frequency = 30)
pgram_308_winterres_feeding_detail

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

### potential pupping time ####

p_summerpupping <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                 lubridate::date(date_time) %>% between(as.POSIXct("2019-07-11"), as.POSIXct("2019-07-17")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2019-05-10"), as.POSIXct("2019-08-03")),
                                                                 row_number() %% 1 == 0), aes(x = date_time, y = -depth_m, colour = lubridate::hour(date_time))) +
  geom_point() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential pupping period")


p_summerpupping #%>% ggplotly()

pgram_308_pupping <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                     start_date = as.POSIXct("2019-05-10"),
                                     end_date = as.POSIXct("2019-08-03"), # add days bc otherwise very short
                                     sample_frequency = 30)
pgram_308_pupping

### summer migration 308 ####

p_summermig19_308 <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                   lubridate::date(date_time) %>% between(as.POSIXct("2019-03-18"), as.POSIXct("2019-05-10"))
                                                                   # lubridate::date(date_time) %>% between(as.POSIXct("2019-03-10"), as.POSIXct("2019-03-30"))
                                                                   # ,row_number() %% 5 == 0
), aes(x = date_time, y = -depth_m, colour = lubridate::hour(date_time))) + #, colour = lubridate::hours(date_time)
  geom_point() +
  # scale_fill_brewer() +
  # guides(colour = guide_legend(reverse = T)) +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential summer migration")

p_summermig19_308 #%>% ggplotly()

pgram_308_summermig <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                      start_date = as.POSIXct("2019-03-17"),
                                      end_date = as.POSIXct("2019-03-27"), # add days bc otherwise very short
                                      sample_frequency = 30)
pgram_308_summermig

#### 2nd potential summer migration ####

p_summermig19_308_2 <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293308",
                                                                     lubridate::date(date_time) %>% between(as.POSIXct("2019-04-10"), as.POSIXct("2019-05-14"))
                                                                     # ,row_number() %% 5 == 0
), aes(x = date_time, y = -depth_m, colour = lubridate::hour(date_time))) + #, colour = lubridate::hours(date_time)
  geom_point() +
  # scale_fill_brewer() +
  # guides(colour = guide_legend(reverse = T)) +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female (tag 308), potential summer migration")

p_summermig19_308_2 %>% ggplotly()

##### fft of 2nd potential summer mig ####

pgram_308_summermig2_1 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                               start_date = as.POSIXct("2019-03-18"),
                               end_date = as.POSIXct("2019-05-10"),
                               sample_frequency = 30)
pgram_308_summermig2_1

pgram_308_summermig2_2 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "308",
                                        start_date = as.POSIXct("2019-05-05"),
                                        end_date = as.POSIXct("2019-05-11"),
                                        sample_frequency = 30)
pgram_308_summermig2_2

# data_test <- masterias_depth_temp %>% 
#   dplyr::filter(lubridate::date(date_time) %>% 
#                                   between(as.POSIXct("2019-06-16"), as.POSIXct("2019-06-18")),
#                                 row_number() %% 5 == 0) %>% 
#   mutate(hour = lubridate::hour(date_time)) # %>% as.factor())
# 
# p3 <- ggplot(data = data_test, aes(x = date_time, y = -depth_m, colour = hour)) + #, colour = lubridate::hours(date_time)
#   geom_point() +
#   labs(y = "depth in m", x = "date", colour = "hour of the day")
# 
# p3 #%>% ggplotly()

# plot female and male behaviour in summer
p4 <- ggplot(data = masterias_depth_temp %>% dplyr::filter(lubridate::date(date_time) %>% between(as.POSIXct("2019-05-11"), as.POSIXct("2019-08-15"))
                                                    ,tag_serial_number %in% c("1293308", "1293321"), row_number() %% 5 == 0
), aes(x = date_time, y = -depth_m, colour = tag_serial_number)) + #lubridate::hour(date_time)
  geom_point() +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "female, tag 308")

p4 %>% ggplotly()

## tag 321 ####

p_321_winterres <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293321",
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2019-02-12"), as.POSIXct("2019-02-15")), #to see if the shark rests the whole time
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-09-14"), as.POSIXct("2018-09-19")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-10-21"), as.POSIXct("2018-11-05")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-11-11"), as.POSIXct("2018-11-27")),
                                                                 row_number() %% 1 == 0), aes(x = date_time, y = -depth_m)) +
  # geom_point(aes(colour = lubridate::hour(date_time))) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week") +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "male (tag 321), potential winter migration")

p_321_winterres %>% ggplotly()

pgram_321_summerres_2018 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                        start_date = as.POSIXct("2018-07-29"),
                                        end_date = as.POSIXct("2018-10-23"),
                                        sample_frequency = 30)
pgram_321_summerres_2018

pgram_321_winterrres_2018_1 <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                          start_date = as.POSIXct("2018-11-01"),
                                          end_date = as.POSIXct("2018-12-07"),
                                          sample_frequency = 30)
## winter res ####

pgram_321_winterrres_2018_1

pgram_321_winterrres_2018_1_detail <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                             start_date = as.POSIXct("2018-11-12"),
                                             end_date = as.POSIXct("2018-11-26"),
                                             sample_frequency = 30)
pgram_321_winterrres_2018_1_detail

pgram_321_winterrres_2018_2_detail <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                                    # start_date = as.POSIXct("2018-10-29"),
                                                    # end_date = as.POSIXct("2018-11-08"),
                                                    start_date = as.POSIXct("2018-10-23"),
                                                    end_date = as.POSIXct("2018-11-11"),
                                                    sample_frequency = 30)
pgram_321_winterrres_2018_2_detail

## summer mig ####

p_321_winterres <- ggplot(data = masterias_depth_temp %>% dplyr::filter(tag_serial_number == "1293321",
                                                                 lubridate::date(date_time) %>% between(as.POSIXct("2019-06-01"), as.POSIXct("2019-09-20")), #to see if the shark rests the whole time
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-09-14"), as.POSIXct("2018-09-19")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-10-21"), as.POSIXct("2018-11-05")),
                                                                 # lubridate::date(date_time) %>% between(as.POSIXct("2018-11-11"), as.POSIXct("2018-11-27")),
                                                                 row_number() %% 1 == 0), aes(x = date_time, y = -depth_m)) +
  geom_point(aes(colour = lubridate::hour(date_time))) +
  # geom_line() +
  scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week") +
  labs(y = "depth in m", x = "date", colour = "hour of the day", title = "male (tag 321), potential summer migration")

p_321_winterres %>% ggplotly()


pgram_321_summermig <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                                    # start_date = as.POSIXct("2018-10-29"),
                                                    # end_date = as.POSIXct("2018-11-08"),
                                                    start_date = as.POSIXct("2019-05-03"),
                                                    end_date = as.POSIXct("2019-05-25"),
                                                    sample_frequency = 30)
pgram_321_summermig

## summerres ####
pgram_321_summerres <- fft_calc_plot(depth_log = masterias_depth_temp, tag_serial_num_short = "321",
                                     # start_date = as.POSIXct("2018-10-29"),
                                     # end_date = as.POSIXct("2018-11-08"),
                                     start_date = as.POSIXct("2019-06-01"),
                                     end_date = as.POSIXct("2019-09-25"),
                                     sample_frequency = 30)
pgram_321_summerres


# test with downsampled depthlog ####
pgram_308_downsampled <- fft_calc_plot(depth_log = masterias_depth_temp %>% dplyr::filter(row_number() %% 15 == 0), tag_serial_num_short = "308",
                                           start_date = as.POSIXct("2018-09-01"),
                                           end_date = as.POSIXct("2018-09-25"), 
                                           sample_frequency = 2)
pgram_308_downsampled
