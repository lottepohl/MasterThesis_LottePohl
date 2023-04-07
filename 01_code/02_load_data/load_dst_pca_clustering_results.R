# script to load the pca and k means clustering results

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl" # "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
data_path <- paste0(dir_path, "/02_results/dst_pca_kmeans/")
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()
# source(paste0(dir_path, "/02_scripts/04_analyses/dst_summarystatistics/dst_pca_clustering.R"))

tag_308_kmeans <- load_data(filestring = "tag_308_kmeans", folder = data_path)
tag_308_depthsum <- load_data(filestring = "tag_308_depthsum", folder = data_path)
tag_308_pca <- load_data(filestring = "tag_308_pca", folder = data_path)

tag_321_kmeans <- load_data(filestring = "tag_321_kmeans", folder = data_path)
tag_321_depthsum <- load_data(filestring = "tag_321_depthsum", folder = data_path)
tag_321_pca <- load_data(filestring = "tag_321_pca", folder = data_path)
