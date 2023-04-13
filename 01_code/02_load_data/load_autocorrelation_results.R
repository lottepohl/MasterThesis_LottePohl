# Script to load autocorrelation results 

plot_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl/02_results/dst_autocorrelation/"

acf_308_df <- load_data(filestring = "acf_308_df", folder = plot_path)
acf_321_df <- load_data(filestring = "acf_321_df", folder = plot_path)

# p_acf_308 <- load_data(filestring = "p_acf_308", folder = plot_path)
# p_acf_321 <- load_data(filestring = "p_acf_321", folder = plot_path)

rm(plot_path)