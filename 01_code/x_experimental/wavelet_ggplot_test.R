library(biwavelet)
library(ggplot2)
library(purrr)

# Conduct wavelet analysis on time series x
x <- data.frame(t = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                vals = rnorm(9))

wt_res <- biwavelet::wt(d = x %>% as.matrix())

par(mfcol = c(1,1))
# par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(wt_res, 
     type = "power.corr.norm", main = "test", plot.cb = F, plot.phase = F)
plot(x = x$t, y = x$vals, type = "l")

# format data into df ####
test_scale <- wt_res$scale %>% as.data.frame() %>% `colnames<-`("scale") 

test_period <- wt_res$period %>% as.data.frame() %>% `colnames<-`("period")

test_xaxis <- wt_res$xaxis %>% as.data.frame() %>% `colnames<-`("time")

# wt_data <- cbind(test_scale, test_period, test_xaxis) # nicht richtig

# get the height for the last tile (which gets NA when using `dplyr::lead()`): subtracting the smallest fromt the second smallest period val
# tile_height <- ( slice(test_power, nrow(test_power) - 1) %>% select(period) %>% pull() ) - ( slice(test_power, nrow(test_power)) %>% select(period) %>% pull())
# test_power$period[nrow(test_power) - 1]
# slice(test_power, nrow(test_power) - 1) %>% 
#   select(period) %>% 
#   pull()

test_power <- wt_res$power.corr %>% as.data.frame() %>%
  purrr::set_names(as.character(test_xaxis$time)) %>%
  # `colnames <-` as.character(test_xaxis$time)
  cbind(test_period) %>%
  arrange(desc(period)) %>%
  mutate(height = (period - dplyr::lead(period, 
                                        default = test_power$period[nrow(test_power) - 1])) %>% 
           abs(),
         height = height + 0.15) %>%
  pivot_longer(cols = -c(last_col(offset = 1), last_col(offset = 0)), names_to = "t") %>% #don't pivot the two last columns
  rename(frequency = value)
  
  # replace_na(tile_height)

# test_power <- wt_res$power.corr %>% as.data.frame() %>%
#   purrr::set_names(as.character(test_xaxis$time)) %>%
#   # `colnames <-` as.character(test_xaxis$time)
#   cbind(test_period) %>%
#   arrange(desc(period)) %>%
#   # mutate(period2 = arrange(desc(period))) %>%
#   # dplyr::select(!period)
#   pivot_longer(cols = -last_col(), names_to = "t") %>%
#   rename(frequency = value)
# # colnames(test_power)

# tile_height <- ( test_power$period %>% max() - test_power$period %>% min() ) / test_power$period %>% unique() %>% length()

test_plot <- ggplot(data = test_power) +
  geom_tile(aes(x = t, y = period, fill = frequency, height = height), #, colour = frequency
            position = "identity") +
  scale_y_reverse(expand =c(0,0)) +
  scale_fill_viridis_c() +
  labs(x = "Time in days", y = "Period", fill = "Frequency")

test_plot #%>% ggplotly()
