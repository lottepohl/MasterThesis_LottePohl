# Script to access Scheldemonitor Data

library(readr)
library(lubridate)
library(leafem)

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/functions.R"))

# WFS
SM_salinity_dataurl <- "http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aabiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%28998%29+AND+%28gID+IN+%2810%5C%2C9%29%29+AND+%28%28datetime_search+BETWEEN+%272018-01-01%27+AND+%272020-12-31%27+AND+seasonID+IN+%282%5C%2C3%5C%2C4%29%29%29%3Bcontext%3A0001&propertyName=stationname%2Clongitude%2Clatitude%2Cdatetime%2Cdepth%2Cparametername%2Cvaluesign%2Cvalue%2Cdataprovider%2Cdatasettitle%2Clod%2Cloq%2Ccategory%2Cseason%2Cclassunit%2Cclass%2Caphiaid%2Cscientificname%2Cdateprecision%2Cdatafichetitle%2Cdataficheid%2Cstandardparameterid%2Cparameterunit&outputFormat=csv"

SM_salinity <- read_csv(SM_salinity_dataurl)
# 
# SM_sal_Apr_2018 <- SM_salinity %>% filter(lubridate::month(datetime) == 4 & lubridate::year(datetime) == 2018)

SM_salinity_summary <- SM_salinity %>% filter(longitude > 3.65) %>% mutate(month = base::month.name[lubridate::month(datetime)], year = lubridate::year(datetime)) %>% 
  group_by(year, month) %>%
  summarise(month_mean = mean(value)) %>% mutate(date = paste0(month, " ", year)) %>% 
  mutate(year = year %>% factor(levels = c(2018, 2019, 2020)),
         month = month %>% factor(levels = base::month.name)) %>%
  filter(!month == "March")

SM_salinity_summary_month <- SM_salinity %>% filter(longitude > 3.65) %>% mutate(month = base::month.name[lubridate::month(datetime)], year = lubridate::year(datetime)) %>% 
  group_by(month) %>%
  summarise(month_mean = mean(value), sd = sd(value)) %>%
  mutate(month = month %>% factor(levels = base::month.name)) %>%
  filter(!month == "March")

save_data(data = SM_salinity, folder = path_envdata)
save_data(data = SM_salinity_summary, folder = path_envdata)
save_data(data = SM_salinity_summary_month, folder = path_envdata)

