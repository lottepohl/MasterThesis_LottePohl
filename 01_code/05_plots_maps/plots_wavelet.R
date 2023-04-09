# Script to plot the wavelet analysis results

library(scales)
library(biwavelet)
library(ggplot2)
library(dplyr)

# rm(list = ls())

dir_path_new <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
source(paste0(dir_path_new, "/01_code/06_functions/functions.R"))
source(paste0(dir_path_new, "/01_code/02_load_data/load_wavelet_results.R"))
source(paste0(dir_path_new, "/01_code/06_functions/compute_wavelettransform.R"))
data_path <- paste0(dir_path_new, "/02_results/dst_wavelet/")
# paste0(dir_path_new, "/01_code/05_plots_maps/plots_dst_summary.R") %>% base::source()
# paste0(dir_path_new, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()

# dates ####
dates_308 <- long_dst_date %>% filter(tag_serial_number == "1293308") %>% dplyr::select(date)
dates_321 <- long_dst_date %>% filter(tag_serial_number == "1293321") %>% dplyr::select(date)

# tag 308 ####
## depthrange ####
wt_df_308_depthrange <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange) #wt_308_depthrange$xaxis %>% as.data.frame()
p_wave_308_depthrange <- plot_wavelet_gg2(wt_df = wt_df_308_depthrange, type = "power_log", date = T)
p_wave_308_depthrange %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm") #%>% ggplotly()

## depthrange_roll3 ####
wt_df_308_depthrange_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange_roll3) #wt_308_depthrange_roll3$xaxis %>% as.data.frame()
p_wave_308_depthrange_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_depthrange_roll3, type = "power_log", date = T)
p_wave_308_depthrange_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## depthrange_change_roll3 ####
wt_df_308_depthrange_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthrange_change_roll3) #wt_308_depthrange_change_roll3$xaxis %>% as.data.frame()
p_wave_308_depthrange_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_depthrange_change_roll3, type = "power_log", date = T)
p_wave_308_depthrange_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## depthsd ####
wt_df_308_depthsd <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_depthsd) #wt_308_depthsd$xaxis %>% as.data.frame()
p_wave_308_depthsd <- plot_wavelet_gg2(wt_df = wt_df_308_depthsd, type = "power_log", date = T)
p_wave_308_depthsd %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth ####
wt_df_308_maxdepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth) #wt_308_maxdepth$xaxis %>% as.data.frame()
p_wave_308_maxdepth <- plot_wavelet_gg2(wt_df = wt_df_308_maxdepth, type = "power_log", date = T)
p_wave_308_maxdepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth_change_roll3 ####
wt_df_308_maxdepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth_change_roll3) #wt_308_maxdepth_change_roll3$xaxis %>% as.data.frame()
p_wave_308_maxdepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_maxdepth_change_roll3, type = "power_log", date = T)
p_wave_308_maxdepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth_roll3 ####
wt_df_308_maxdepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_maxdepth_roll3) #wt_308_maxdepth_roll3$xaxis %>% as.data.frame()
p_wave_308_maxdepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_maxdepth_roll3, type = "power_log", date = T)
p_wave_308_maxdepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth ####
wt_df_308_mindepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth) #wt_308_mindepth$xaxis %>% as.data.frame()
p_wave_308_mindepth <- plot_wavelet_gg2(wt_df = wt_df_308_mindepth, type = "power_log", date = T)
p_wave_308_mindepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth_change_roll3 ####
wt_df_308_mindepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth_change_roll3) #wt_308_mindepth_change_roll3$xaxis %>% as.data.frame()
p_wave_308_mindepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_mindepth_change_roll3, type = "power_log", date = T)
p_wave_308_mindepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth_roll3 ####
wt_df_308_mindepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mindepth_roll3) #wt_308_mindepth_roll3$xaxis %>% as.data.frame()
p_wave_308_mindepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_mindepth_roll3, type = "power_log", date = T)
p_wave_308_mindepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## meandepth ####
wt_df_308_meandepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_meandepth) #wt_308_meandepth$xaxis %>% as.data.frame()
p_wave_308_meandepth <- plot_wavelet_gg2(wt_df = wt_df_308_meandepth, type = "power_log", date = T)
p_wave_308_meandepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth ####
wt_df_308_mediandepth <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth) #wt_308_mediandepth$xaxis %>% as.data.frame()
p_wave_308_mediandepth <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth, type = "power_log", date = T)
p_wave_308_mediandepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change ####
wt_df_308_mediandepth_change <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change) #wt_308_mediandepth_change$xaxis %>% as.data.frame()
p_wave_308_mediandepth_change <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth_change, type = "power_log", date = T)
p_wave_308_mediandepth_change %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change_roll3 ####
wt_df_308_mediandepth_change_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change_roll3) #wt_308_mediandepth_change_roll3$xaxis %>% as.data.frame()
p_wave_308_mediandepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth_change_roll3, type = "power_log", date = T)
p_wave_308_mediandepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change2 ####
wt_df_308_mediandepth_change2 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change2) #wt_308_mediandepth_change2$xaxis %>% as.data.frame()
p_wave_308_mediandepth_change2 <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth_change2, type = "power_log", date = T)
p_wave_308_mediandepth_change2 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change2_roll3 ####
wt_df_308_mediandepth_change2_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_change2_roll3) #wt_308_mediandepth_change2_roll3$xaxis %>% as.data.frame()
p_wave_308_mediandepth_change2_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth_change2_roll3, type = "power_log", date = T)
p_wave_308_mediandepth_change2_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_roll3 ####
wt_df_308_mediandepth_roll3 <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_mediandepth_roll3) #wt_308_mediandepth_roll3$xaxis %>% as.data.frame()
p_wave_308_mediandepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_308_mediandepth_roll3, type = "power_log", date = T)
p_wave_308_mediandepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## vertspeedmax ####
wt_df_308_vertspeedmax <- wavelet_output_compare(dates = dates_308, wt_output = wt_308_vertspeedmax) #wt_308_vertspeedmax$xaxis %>% as.data.frame()
p_wave_308_vertspeedmax <- plot_wavelet_gg2(wt_df = wt_df_308_vertspeedmax, type = "power_log", date = T)
p_wave_308_vertspeedmax %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()


# tag 321 ####
## depthrange ####
wt_df_321_depthrange <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange) #wt_321_depthrange$xaxis %>% as.data.frame()
p_wave_321_depthrange <- plot_wavelet_gg2(wt_df = wt_df_321_depthrange, type = "power_log", date = T)
p_wave_321_depthrange %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm") #%>% ggplotly()

## depthrange_roll3 ####
wt_df_321_depthrange_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange_roll3) #wt_321_depthrange_roll3$xaxis %>% as.data.frame()
p_wave_321_depthrange_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_depthrange_roll3, type = "power_log", date = T)
p_wave_321_depthrange_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## depthrange_change_roll3 ####
wt_df_321_depthrange_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthrange_change_roll3) #wt_321_depthrange_change_roll3$xaxis %>% as.data.frame()
p_wave_321_depthrange_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_depthrange_change_roll3, type = "power_log", date = T)
p_wave_321_depthrange_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## depthsd ####
wt_df_321_depthsd <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthsd) #wt_321_depthsd$xaxis %>% as.data.frame()
p_wave_321_depthsd <- plot_wavelet_gg2(wt_df = wt_df_321_depthsd, type = "power_log", date = T)
p_wave_321_depthsd %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## depthvar ####
wt_df_321_depthvar <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_depthvar) #wt_321_depthvar$xaxis %>% as.data.frame()
p_wave_321_depthvar <- plot_wavelet_gg2(wt_df = wt_df_321_depthvar, type = "power_log", date = T)
p_wave_321_depthvar %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth ####
wt_df_321_maxdepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth) #wt_321_maxdepth$xaxis %>% as.data.frame()
p_wave_321_maxdepth <- plot_wavelet_gg2(wt_df = wt_df_321_maxdepth, type = "power_log", date = T)
p_wave_321_maxdepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth_change_roll3 ####
wt_df_321_maxdepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth_change_roll3) #wt_321_maxdepth_change_roll3$xaxis %>% as.data.frame()
p_wave_321_maxdepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_maxdepth_change_roll3, type = "power_log", date = T)
p_wave_321_maxdepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## maxdepth_roll3 ####
wt_df_321_maxdepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_maxdepth_roll3) #wt_321_maxdepth_roll3$xaxis %>% as.data.frame()
p_wave_321_maxdepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_maxdepth_roll3, type = "power_log", date = T)
p_wave_321_maxdepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth ####
wt_df_321_mindepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth) #wt_321_mindepth$xaxis %>% as.data.frame()
p_wave_321_mindepth <- plot_wavelet_gg2(wt_df = wt_df_321_mindepth, type = "power_log", date = T)
p_wave_321_mindepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth_change_roll3 ####
wt_df_321_mindepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth_change_roll3) #wt_321_mindepth_change_roll3$xaxis %>% as.data.frame()
p_wave_321_mindepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_mindepth_change_roll3, type = "power_log", date = T)
p_wave_321_mindepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mindepth_roll3 ####
wt_df_321_mindepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mindepth_roll3) #wt_321_mindepth_roll3$xaxis %>% as.data.frame()
p_wave_321_mindepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_mindepth_roll3, type = "power_log", date = T)
p_wave_321_mindepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## meandepth ####
wt_df_321_meandepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_meandepth) #wt_321_meandepth$xaxis %>% as.data.frame()
p_wave_321_meandepth <- plot_wavelet_gg2(wt_df = wt_df_321_meandepth, type = "power_log", date = T)
p_wave_321_meandepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth ####
wt_df_321_mediandepth <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth) #wt_321_mediandepth$xaxis %>% as.data.frame()
p_wave_321_mediandepth <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth, type = "power_log", date = T)
p_wave_321_mediandepth %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()
p_wave_321_mediandepth_sig <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth, type = "significance", date = T)
p_wave_321_mediandepth_sig

## mediandepth_change ####
wt_df_321_mediandepth_change <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change) #wt_321_mediandepth_change$xaxis %>% as.data.frame()
p_wave_321_mediandepth_change <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth_change, type = "power_log", date = T)
p_wave_321_mediandepth_change %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change_roll3 ####
wt_df_321_mediandepth_change_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change_roll3) #wt_321_mediandepth_change_roll3$xaxis %>% as.data.frame()
p_wave_321_mediandepth_change_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth_change_roll3, type = "power_log", date = T)
p_wave_321_mediandepth_change_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change2 ####
wt_df_321_mediandepth_change2 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change2) #wt_321_mediandepth_change2$xaxis %>% as.data.frame()
p_wave_321_mediandepth_change2 <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth_change2, type = "power_log", date = T)
p_wave_321_mediandepth_change2 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_change2_roll3 ####
wt_df_321_mediandepth_change2_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_change2_roll3) #wt_321_mediandepth_change2_roll3$xaxis %>% as.data.frame()
p_wave_321_mediandepth_change2_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth_change2_roll3, type = "power_log", date = T)
p_wave_321_mediandepth_change2_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## mediandepth_roll3 ####
wt_df_321_mediandepth_roll3 <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_mediandepth_roll3) #wt_321_mediandepth_roll3$xaxis %>% as.data.frame()
p_wave_321_mediandepth_roll3 <- plot_wavelet_gg2(wt_df = wt_df_321_mediandepth_roll3, type = "power_log", date = T)
p_wave_321_mediandepth_roll3 %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()

## vertspeedmax ####
wt_df_321_vertspeedmax <- wavelet_output_compare(dates = dates_321, wt_output = wt_321_vertspeedmax) #wt_321_vertspeedmax$xaxis %>% as.data.frame()
p_wave_321_vertspeedmax <- plot_wavelet_gg2(wt_df = wt_df_321_vertspeedmax, type = "power_log", date = T)
p_wave_321_vertspeedmax %>% ggsave(filename = paste0(data_path,deparse(substitute(.)), ".png"), height = 12, width = 16, units = "cm")#%>% ggplotly()
