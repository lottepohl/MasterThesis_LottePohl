# script to load the results of the wavelet analyses

# source(paste0(getwd(), "/01_code/04_analyses/wavelet/wavelet_analysis.R")) # load analysis script
data_path <- paste0(getwd(), "/02_results/dst_wavelet/")


# load data ####

## tag 321 ####
wt_321_mediandepth_roll3 <- load_data(filestring = "wt_321_mediandepth_roll3", folder = data_path)
wt_321_mediandepth <- load_data(filestring = "wt_321_mediandepth", folder = data_path)
wt_321_mediandepth_change_roll3 <- load_data(filestring = "wt_321_mediandepth_change_roll3", folder = data_path)
wt_321_depthrange <- load_data(filestring = "wt_321_depthrange", folder = data_path)
wt_321_depthrange_roll3 <- load_data(filestring = "wt_321_depthrange_roll3", folder = data_path)

## tag 308 ####
wt_308_mediandepth_roll3 <- load_data(filestring = "wt_308_mediandepth_roll3", folder = data_path)
wt_308_mediandepth <- load_data(filestring = "wt_308_mediandepth", folder = data_path)
wt_308_mediandepth_change_roll3 <- load_data(filestring = "wt_308_mediandepth_change_roll3", folder = data_path)
wt_308_depthrange <- load_data(filestring = "wt_308_depthrange", folder = data_path)
wt_308_depthrange_roll3 <- load_data(filestring = "wt_308_depthrange_roll3", folder = data_path)

## dates ####
long_dst_date <- load_data(filestring = "long_dst_date", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()
long_dst_daynight <- load_data(filestring = "long_dst_daynight", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()
