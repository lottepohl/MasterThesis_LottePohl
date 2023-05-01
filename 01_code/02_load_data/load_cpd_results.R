# Script to load results of the RuLSIF change point detection, 
# generated in "./01_code/04_analyses/change_point_detection_rulsif.R"

# rm(list = ls())

# source(paste0(getwd(), "/01_code/04_analyses/change_point_detection_rulsif.R")) # load analysis script
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
data_path <- paste0(dir_path, "/02_results/dst_changepointdetection/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

# save_data(data = rulsif_308_res_2_5percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_table_2_5percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_res_5percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_table_5percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_res_10percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_table_10percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_res_15percent, folder = rulsif_data_path)
# save_data(data = rulsif_308_table_15percent, folder = rulsif_data_path)

rulsif_308_res_2_5percent <- load_data(filestring = "rulsif_308_res_2_5percent", folder = data_path)
# rulsif_308_table_2_5percent <- load_data(filestring = "rulsif_308_table_2_5percent", folder = data_path)
rulsif_308_res_5percent <- load_data(filestring = "rulsif_308_res_5percent", folder = data_path)
# rulsif_308_table_5percent <- load_data(filestring = "rulsif_308_table_5percent", folder = data_path)
rulsif_308_res_10percent <- load_data(filestring = "rulsif_308_res_10percent", folder = data_path)
# rulsif_308_table_10percent <- load_data(filestring = "rulsif_308_table_10percent", folder = data_path)
# rulsif_308_res_15percent <- load_data(filestring = "rulsif_308_res_15percent", folder = data_path)
# rulsif_308_table_15percent <- load_data(filestring = "rulsif_308_table_15percent", folder = data_path)

rulsif_321_res_2_5percent <- load_data(filestring = "rulsif_321_res_2_5percent", folder = data_path)
# rulsif_321_table_2_5percent <- load_data(filestring = "rulsif_321_table_2_5percent", folder = data_path)
rulsif_321_res_5percent <- load_data(filestring = "rulsif_321_res_5percent", folder = data_path)
# rulsif_321_table_5percent <- load_data(filestring = "rulsif_321_table_5percent", folder = data_path)
rulsif_321_res_10percent <- load_data(filestring = "rulsif_321_res_10percent", folder = data_path)
# rulsif_321_table_10percent <- load_data(filestring = "rulsif_321_table_10percent", folder = data_path)
# rulsif_321_res_15percent <- load_data(filestring = "rulsif_321_res_15percent", folder = data_path)
# rulsif_321_table_15percent <- load_data(filestring = "rulsif_321_table_15percent", folder = data_path)

