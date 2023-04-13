# Script to load the fft results

# rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
plot_path <- paste0(dir_path, "/02_results/spectral_analysis/fft/")

paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

fft_295 <- load_data(filestring = "fft_295", folder = plot_path)
fft_304 <- load_data(filestring = "fft_304", folder = plot_path)
fft_308 <- load_data(filestring = "fft_308", folder = plot_path)
fft_310 <- load_data(filestring = "fft_310", folder = plot_path)
fft_312 <- load_data(filestring = "fft_312", folder = plot_path)
fft_319 <- load_data(filestring = "fft_319", folder = plot_path)
fft_321 <- load_data(filestring = "fft_321", folder = plot_path)
fft_322 <- load_data(filestring = "fft_322", folder = plot_path)
