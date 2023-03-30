# Script to calculate the fft and PSD

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path <- paste0(dir_path, "/04_analysis_results/spectral_analysis/fft/")

source(paste0(dir_path, "/02_scripts/02_load_data/load_depth_temp_logs.R"))

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

# Calculate fft and psd for all dst tags

sample_frequency <- 30 # 1 sample per 2 min == 30 samples per 60 min == 30 [1/hour]

fft_295 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293295"),
                    sample_freq = sample_frequency)

fft_304 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293304" & t < 300),
                    sample_freq = sample_frequency)

fft_308 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293308"),
                    sample_freq = sample_frequency)

fft_310 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293310"),
                    sample_freq = sample_frequency)

fft_312 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293312"),
                    sample_freq = sample_frequency)

fft_319 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293319"),
                    sample_freq = sample_frequency)

fft_321 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293321"),
                    sample_freq = sample_frequency)

fft_322 <- calc_fft(depth_log = masterias_depth_temp %>% filter(tag_serial_number == "1293322"),
                    sample_freq = sample_frequency)


# save data as .rds ####