# Script to carry out autocorrelation analysis on longterm dst plots

# todo: header richtig konfigurieren

# x <- arima.sim(n = 200, model = list(ar = 0.6))

bacf <- acf(x, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

acf_321 <- acf(data_321_summary_wt_all$depth_median, lag.max = 400, type = "covariance", plot = F) %>% as.data.frame(lag,acf)

q <- ggplot(data = acf_321, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))
q

