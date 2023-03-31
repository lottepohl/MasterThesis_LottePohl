# Script to load the dst overview plots

# rm(list = ls())

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
plot_path_dst <- paste0(dir_path, "/02_results/dst_overview/depth/")

source(paste0(getwd(), "/01_code/06_functions/functions.R"))


# source(paste0(dir_path, "/functions/functions_DST_presstemplogs.R"))
# source(paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R"))

plot_dst_295 <- load_data(filestring = "plot_dst_295", folder = plot_path_dst)
plot_dst_304 <- load_data(filestring = "plot_dst_304", folder = plot_path_dst)
plot_dst_308 <- load_data(filestring = "plot_dst_308", folder = plot_path_dst)
plot_dst_310 <- load_data(filestring = "plot_dst_310", folder = plot_path_dst)
plot_dst_312 <- load_data(filestring = "plot_dst_312", folder = plot_path_dst)
plot_dst_319 <- load_data(filestring = "plot_dst_319", folder = plot_path_dst)
plot_dst_321 <- load_data(filestring = "plot_dst_321", folder = plot_path_dst)
plot_dst_322 <- load_data(filestring = "plot_dst_322", folder = plot_path_dst)
