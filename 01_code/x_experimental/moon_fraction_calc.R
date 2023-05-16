# Script to calculate the fraction of moon illuminated by the sun

# library(astrolibR) # not available for recent R versions
library(oce)
library(leaflet)
library(leafem)
library(tibble)
library(ggplot2)

# X11()

t_start <- as.POSIXct("2017-02-23", tz='UTC')
t_end   <- t_start + 75 * 24 * 3600  # 35 days

tm <- seq(from = t_start,
          to  = t_end,
          by  = 10000
)

moon     <- moonAngle(t=tm, longitude=-8, latitude=47)
fraction <- moon$illuminatedFraction
phase    <- moon$phase - floor(moon$phase)

plot (tm, fraction, type="l", col='blue')
lines(tm, phase   , type="l", col='red' )

# grid()

leaflet() %>% addTiles() %>% addMouseCoordinates()

dates <- seq(from = "2018-08-03" %>% as.POSIXct(tz = "utc"), to = "2019-05-10" %>% as.POSIXct(tz = "utc"), by = "day")

moonfraq_capdelahague <- oce::moonAngle(t = dates, longitude = -2.1, latitude = 49.6)$illuminatedFraction
moonfraq_norwhich <- oce::moonAngle(t = dates, longitude = 1.66, latitude = 52.9)$illuminatedFraction

moonfraq <- tibble::tibble(dates = dates,
                           capdelahague = moonfraq_capdelahague,
                           norwhich = moonfraq_norwhich)

ggplot(data = moonfraq, aes(x = dates)) +
  geom_line(aes(y = capdelahague + 0.1), colour = "red") +
  geom_line(aes(y = norwhich), colour = "blue") 
