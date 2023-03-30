# Load the required packages
library(dplyr)  # For data manipulation
library(lubridate)  # For working with date-time data

# Read in the data
data <- read.csv("your_file_path.csv")

# Convert the timestamp to a date-time format
data$timestamp <- ymd_hms(data$timestamp)

# Group the data by tag serial number and receiver station name
grouped_data <- data %>% group_by(tag_serial_number, receiver_station_name)

# Compute the time difference between consecutive detections for each group
grouped_data <- grouped_data %>% mutate(time_diff = difftime(timestamp, lag(timestamp), units = "hours"))

# Compute the instantaneous frequency for each detection
grouped_data <- grouped_data %>% mutate(inst_freq = 1 / as.numeric(time_diff))

# Compute the FFT of the instantaneous frequency for each group
fft_data <- grouped_data %>% group_by(tag_serial_number, receiver_station_name) %>% summarize(fft_inst_freq = abs(fft(inst_freq)))

# Plot the frequency spectrum
plot(fft_data$fft_inst_freq, type = "l", xlab = "Frequency", ylab = "FFT amplitude")

# Identify the frequencies corresponding to diurnal patterns
peak_freqs <- findInterval(24, seq(0, 1/(2*median(diff(data$timestamp))), length.out = length(fft_data$fft_inst_freq)))

# Highlight the peak frequencies in the plot
abline(v = peak_freqs, col = "red")
