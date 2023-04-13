# script to load plots for the thesis manuscript

# Workspace ####

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

# plots ####

## autocorrelation #####

p_acf_308 <- load_data(filestring = "p_acf_308", folder = plot_path)
p_acf_321 <- load_data(filestring = "p_acf_321", folder = plot_path)
