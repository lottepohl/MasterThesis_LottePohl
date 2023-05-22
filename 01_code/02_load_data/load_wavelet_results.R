# script to load the results of the wavelet analyses

# rm(list = ls())

# source(paste0(getwd(), "/01_code/04_analyses/wavelet/wavelet_analysis.R")) # load analysis script
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
data_path <- paste0(dir_path, "/02_results/dst_wavelet/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()


# load data ####
# 
# ## tag 321
# wt_321_mediandepth_roll3 <- load_data(filestring = "wt_321_mediandepth_roll3", folder = data_path)
# wt_321_mediandepth <- load_data(filestring = "wt_321_mediandepth", folder = data_path)
# wt_321_mediandepth_change_roll3 <- load_data(filestring = "wt_321_mediandepth_change_roll3", folder = data_path)
# wt_321_depthrange <- load_data(filestring = "wt_321_depthrange", folder = data_path)
# wt_321_depthrange_roll3 <- load_data(filestring = "wt_321_depthrange_roll3", folder = data_path)
# 
# ## tag 308
# wt_308_mediandepth_roll3 <- load_data(filestring = "wt_308_mediandepth_roll3", folder = data_path)
# wt_308_mediandepth <- load_data(filestring = "wt_308_mediandepth", folder = data_path)
# wt_308_mediandepth_change_roll3 <- load_data(filestring = "wt_308_mediandepth_change_roll3", folder = data_path)
# wt_308_depthrange <- load_data(filestring = "wt_308_depthrange", folder = data_path)
# wt_308_depthrange_roll3 <- load_data(filestring = "wt_308_depthrange_roll3", folder = data_path)

## dates ####
long_dst_date <- load_data(filestring = "long_dst_date", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()
long_dst_daynight <- load_data(filestring = "long_dst_daynight", folder = paste0(dir_path, "/02_results/dst_summary/")) %>% ungroup()

# dfs for gg plotting ####
## tag 321 ####
# wt_df_321_mediandepth_roll3 <- load_data(filestring = "wt_df_321_mediandepth_roll3", folder = data_path)
# wt_df_321_mediandepth <- load_data(filestring = "wt_df_321_mediandepth", folder = data_path)
# wt_df_321_mediandepth_change_roll3 <- load_data(filestring = "wt_df_321_mediandepth_change_roll3", folder = data_path)
# wt_df_321_depthrange <- load_data(filestring = "wt_df_321_depthrange", folder = data_path)
# wt_df_321_depthrange_roll3 <- load_data(filestring = "wt_df_321_depthrange_roll3", folder = data_path)
# wt_df_321_maxdepth_change_roll3 <- load_data(filestring = "wt_df_321_maxdepth_change_roll3", folder = data_path)
# wt_df_321_mindepth_change_roll3 <- load_data(filestring = "wt_df_321_mindepth_change_roll3", folder = data_path)
# wt_df_321_depthrange_change_roll3 <- load_data(filestring = "wt_df_321_depthrange_change_roll3", folder = data_path)
# wt_df_321_mediandepth_change2_roll3 <- load_data(filestring = "wt_df_321_mediandepth_change2_roll3", folder = data_path)
# wt_df_321_mediandepth_change2 <- load_data(filestring = "wt_df_321_mediandepth_change2", folder = data_path)
# wt_df_321_mediandepth_change <- load_data(filestring = "wt_df_321_mediandepth_change", folder = data_path)
# wt_df_321_vertspeedmax <- load_data(filestring = "wt_df_321_vertspeedmax", folder = data_path)
# wt_df_321_depthsd <- load_data(filestring = "wt_df_321_depthsd", folder = data_path)
# wt_df_321_maxdepth_roll3 <- load_data(filestring = "wt_df_321_maxdepth_roll3", folder = data_path)
# wt_df_321_maxdepth <- load_data(filestring = "wt_df_321_maxdepth", folder = data_path)
# wt_df_321_mindepth_roll3 <- load_data(filestring = "wt_df_321_mindepth_roll3", folder = data_path)
# wt_df_321_mindepth <- load_data(filestring = "wt_df_321_mindepth", folder = data_path)
# wt_df_321_meandepth <- load_data(filestring = "wt_df_321_meandepth", folder = data_path)
wt_df_321_mediandepth_sgolay <- load_data(filestring = "wt_df_321_mediandepth_sgolay", folder = data_path)
wt_df_321_depthrange_sgolay <- load_data(filestring = "wt_df_321_depthrange_sgolay", folder = data_path)
wt_df_321_mindepth_sgolay <- load_data(filestring = "wt_df_321_mindepth_sgolay", folder = data_path)
wt_df_321_maxdepth_sgolay <- load_data(filestring = "wt_df_321_maxdepth_sgolay", folder = data_path)
## tag 308 ####
# wt_df_308_mediandepth_roll3 <- load_data(filestring = "wt_df_308_mediandepth_roll3", folder = data_path)
# wt_df_308_mediandepth <- load_data(filestring = "wt_df_308_mediandepth", folder = data_path)
# wt_df_308_mediandepth_change_roll3 <- load_data(filestring = "wt_df_308_mediandepth_change_roll3", folder = data_path)
# wt_df_308_depthrange <- load_data(filestring = "wt_df_308_depthrange", folder = data_path)
# wt_df_308_depthrange_roll3 <- load_data(filestring = "wt_df_308_depthrange_roll3", folder = data_path)
# wt_df_308_maxdepth_change_roll3 <- load_data(filestring = "wt_df_308_maxdepth_change_roll3", folder = data_path)
# wt_df_308_mindepth_change_roll3 <- load_data(filestring = "wt_df_308_mindepth_change_roll3", folder = data_path)
# wt_df_308_depthrange_change_roll3 <- load_data(filestring = "wt_df_308_depthrange_change_roll3", folder = data_path)
# wt_df_308_mediandepth_change2_roll3 <- load_data(filestring = "wt_df_308_mediandepth_change2_roll3", folder = data_path)
# wt_df_308_mediandepth_change2 <- load_data(filestring = "wt_df_308_mediandepth_change2", folder = data_path)
# wt_df_308_mediandepth_change <- load_data(filestring = "wt_df_308_mediandepth_change", folder = data_path)
# wt_df_308_vertspeedmax <- load_data(filestring = "wt_df_308_vertspeedmax", folder = data_path)
# wt_df_308_depthsd <- load_data(filestring = "wt_df_308_depthsd", folder = data_path)
# wt_df_308_maxdepth_roll3 <- load_data(filestring = "wt_df_308_maxdepth_roll3", folder = data_path)
# wt_df_308_maxdepth <- load_data(filestring = "wt_df_308_maxdepth", folder = data_path)
# wt_df_308_mindepth_roll3 <- load_data(filestring = "wt_df_308_mindepth_roll3", folder = data_path)
# wt_df_308_mindepth <- load_data(filestring = "wt_df_308_mindepth", folder = data_path)
# wt_df_308_meandepth <- load_data(filestring = "wt_df_308_meandepth", folder = data_path)
wt_df_308_mediandepth_sgolay <- load_data(filestring = "wt_df_308_mediandepth_sgolay", folder = data_path)
wt_df_308_depthrange_sgolay <- load_data(filestring = "wt_df_308_depthrange_sgolay", folder = data_path)
wt_df_308_mindepth_sgolay <- load_data(filestring = "wt_df_308_mindepth_sgolay", folder = data_path)
wt_df_308_maxdepth_sgolay <- load_data(filestring = "wt_df_308_maxdepth_sgolay", folder = data_path)

# raw wavelet results ####
## tag 321 ####
# wt_321_mediandepth_roll3 <- load_data(filestring = "wt_321_mediandepth_roll3", folder = data_path)
# wt_321_mediandepth <- load_data(filestring = "wt_321_mediandepth", folder = data_path)
# wt_321_mediandepth_change_roll3 <- load_data(filestring = "wt_321_mediandepth_change_roll3", folder = data_path)
# wt_321_depthrange <- load_data(filestring = "wt_321_depthrange", folder = data_path)
# wt_321_depthrange_roll3 <- load_data(filestring = "wt_321_depthrange_roll3", folder = data_path)
# wt_321_maxdepth_change_roll3 <- load_data(filestring = "wt_321_maxdepth_change_roll3", folder = data_path)
# wt_321_mindepth_change_roll3 <- load_data(filestring = "wt_321_mindepth_change_roll3", folder = data_path)
# wt_321_depthrange_change_roll3 <- load_data(filestring = "wt_321_depthrange_change_roll3", folder = data_path)
# wt_321_mediandepth_change2_roll3 <- load_data(filestring = "wt_321_mediandepth_change2_roll3", folder = data_path)
# wt_321_mediandepth_change2 <- load_data(filestring = "wt_321_mediandepth_change2", folder = data_path)
# wt_321_mediandepth_change <- load_data(filestring = "wt_321_mediandepth_change", folder = data_path)
# wt_321_vertspeedmax <- load_data(filestring = "wt_321_vertspeedmax", folder = data_path)
# wt_321_depthsd <- load_data(filestring = "wt_321_depthsd", folder = data_path)
# wt_321_maxdepth_roll3 <- load_data(filestring = "wt_321_maxdepth_roll3", folder = data_path)
# wt_321_maxdepth <- load_data(filestring = "wt_321_maxdepth", folder = data_path)
# wt_321_mindepth_roll3 <- load_data(filestring = "wt_321_mindepth_roll3", folder = data_path)
# wt_321_mindepth <- load_data(filestring = "wt_321_mindepth", folder = data_path)
# wt_321_meandepth <- load_data(filestring = "wt_321_meandepth", folder = data_path)
# 
 ## tag 308 ####
# wt_308_mediandepth_roll3 <- load_data(filestring = "wt_308_mediandepth_roll3", folder = data_path)
# wt_308_mediandepth <- load_data(filestring = "wt_308_mediandepth", folder = data_path)
# wt_308_mediandepth_change_roll3 <- load_data(filestring = "wt_308_mediandepth_change_roll3", folder = data_path)
# wt_308_depthrange <- load_data(filestring = "wt_308_depthrange", folder = data_path)
# wt_308_depthrange_roll3 <- load_data(filestring = "wt_308_depthrange_roll3", folder = data_path)
# wt_308_maxdepth_change_roll3 <- load_data(filestring = "wt_308_maxdepth_change_roll3", folder = data_path)
# wt_308_mindepth_change_roll3 <- load_data(filestring = "wt_308_mindepth_change_roll3", folder = data_path)
# wt_308_depthrange_change_roll3 <- load_data(filestring = "wt_308_depthrange_change_roll3", folder = data_path)
# wt_308_mediandepth_change2_roll3 <- load_data(filestring = "wt_308_mediandepth_change2_roll3", folder = data_path)
# wt_308_mediandepth_change2 <- load_data(filestring = "wt_308_mediandepth_change2", folder = data_path)
# wt_308_mediandepth_change <- load_data(filestring = "wt_308_mediandepth_change", folder = data_path)
# wt_308_vertspeedmax <- load_data(filestring = "wt_308_vertspeedmax", folder = data_path)
# wt_308_depthsd <- load_data(filestring = "wt_308_depthsd", folder = data_path)
# wt_308_maxdepth_roll3 <- load_data(filestring = "wt_308_maxdepth_roll3", folder = data_path)
# wt_308_maxdepth <- load_data(filestring = "wt_308_maxdepth", folder = data_path)
# wt_308_mindepth_roll3 <- load_data(filestring = "wt_308_mindepth_roll3", folder = data_path)
# wt_308_mindepth <- load_data(filestring = "wt_308_mindepth", folder = data_path)
# wt_308_meandepth <- load_data(filestring = "wt_308_meandepth", folder = data_path)
# 

# hourly wavelet results ####
wt_df_308_depth_hr <- load_data(filestring = "wt_df_308_depth_hr", folder = data_path)
wt_df_321_depth_hr <- load_data(filestring = "wt_df_321_depth_hr", folder = data_path)