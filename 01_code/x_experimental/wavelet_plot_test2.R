# script to test out wavelet analysis result plots 
library(scales)
rm(list = ls())

source(paste0(getwd(), "/01_code/06_functions/functions.R"))
# source(paste0(getwd(), "/01_code/02_load_data/load_wavelet_results.R"))
source(paste0(getwd(), "/01_code/06_functions/compute_wavelettransform.R"))

# test data ####
t1 <- cbind(1:100, rnorm(100))
wt_output <- t1 %>% wt()
dates <- t1[,1] %>% as.data.frame() %>% `colnames<-`("t") %>%
  mutate(t = sprintf("%03d", t %>% as.numeric()))

x <- wt_output$t 
y <- wt_output$period %>% log2
z <- wt_output$power %>% log2
z2 <- wt_output$signif
tol <- 1
zlim <- range( c(-1, 1) * max(z) )
zvals <- z
# zvals[zvals < zlim[1]] <- zlim[1]

image(x = x, y = y, z = t(zvals), ylim = rev(range(y)), col = rainbow(5000))
contour(x = x, y = y, z = t(z2), level = tol, col = "black",
        lwd = 3, add = TRUE, drawlabels = FALSE)

# wt_output$signif %>% as.data.frame() %>% View()
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
# plot(wt_res, 
     # type = "power.corr.norm", main = "test", plot.cb = F, plot.phase = F)
plot.biwavelet(wt_output, type = "power.corr", plot.cb = T)

wt_df <- wavelet_output_compare(dates = dates, wt_output = wt_output) %>%
  mutate(sig = ifelse(significance >= 1, 1, 0))

# wt_ggplot <- make_wavelet_result_ggplot_obj(wt_output = wt_output) %>%
#   mutate(power_raw = power,
#          power = power %>% log2() %>% scale(),
#          period_log = period %>% log2(),
#          t = sprintf("%03d", t %>% as.numeric())) %>%
#   left_join(wt_df %>% dplyr::select(date, significance), by = join_by(t == date), multiple = "all")
my_trans <- scales::trans_new("log2_reverse", function(x) -log2(x), function(x) 2^-x)

test_plot <- ggplot(data = wt_df) +
  geom_tile(aes(x = t, y = period, fill = power, height = height), #, colour = sig
            position = "identity"
            ) +
  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height + 0.08), #
            fill = "black",
            width = 1.8,
            # height = ,
            position = "identity") +
  geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, fill = power, height = height),
            position = "identity") +
  
  # geom_tile(data = wt_df %>% filter(sig == 1), aes(x = t, y = period, height = height),
  #           fill = "white",#, colour = sig
  #           position = "identity") +
  # geom_polygon(data = wt_df %>% filter(sig == 1), aes(x = t, y = period), fill = 'wh') +
  # stat_contour(aes(x = t, y = period, z = significance), binwidth = 0.0001) +
  scale_y_continuous(expand = c(0,0), trans = my_trans,
                     breaks = c(2, 4, 8, 16, 32)) + # Todo: change y axis scale, n.breaks = 6 breaks = c("2", "4", "8", "16", "32")
  # scale_y_reverse(trans = my_trans) +
  scale_x_discrete(breaks = c('000', "020", "040", "060", "080", "100")) +
  scale_fill_viridis_c(direction = 1, option = "turbo") +
  labs(x = "Date", y = "Period") + #, fill = "Power"
  # theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

test_plot #%>% plotly::ggplotly()



dates <- long_dst_date %>% ungroup() %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

x <- wt_321_mediandepth$t 
y <- wt_321_mediandepth$period %>% log2
z <- wt_321_mediandepth$power %>% log2 #%>% scale()
z2 <- wt_321_mediandepth$signif
tol <- 1

wt_df <- wavelet_output_compare(dates = dates, wt_output = wt_321_mediandepth)

ggplot(wt_df, aes(x = t, y = freq_norm)) + geom_point()

image(x = x, y = y, z = t(z), ylim = rev(range(y)), col = rainbow(5000))
contour(x = x, y = y, z = t(z), level = , col = "black",
        lwd = 3, add = TRUE, drawlabels = FALSE)

image(x = x, y = y, z = t(z2), ylim = rev(range(y)), col = rainbow(5000))
contour(x = x, y = y, z = t(z2), level = 01, col = "black",
        lwd = 3, add = TRUE, drawlabels = FALSE)

wt_321_mediandepth$signif %>% as.data.frame() %>% View()

plot.biwavelet(wt_321_mediandepth)

g <- ggplot(data = wt_ggplot, aes(x = t, y = period, z = significance)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
g + geom_contour(bins = 3)

v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + 
  geom_tile(aes(waiting, eruptions, fill = density)) +
  geom_contour(breaks = 0.03) #that's it!!!
ggplot(faithful, aes(waiting, eruptions)) +
  stat_density_2d()
v + geom_contour_filled(bins = 10)
v + geom_contour(bins = 2)
v + geom_contour(binwidth = 0.01)
