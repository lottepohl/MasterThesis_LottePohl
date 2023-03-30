# Script to save the actel files

dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"

source(paste0(dir_path, "/01_code/03_wrangle_data/format_acoustic_data_actel.R"))

folder <- paste0(dir_path, "/00_data/actel_files/")

save_data(Biometrics, folder)
write_csv(Biometrics, paste0(folder, "Biometrics.csv"))
save_data(Spatial, folder)
write_csv(Spatial, paste0(folder, "Spatial.csv"))
save_data(Deployments, folder)
write_csv(Deployments, paste0(folder, "Deployments.csv"))
save_data(Detections, folder)
write_csv(Detections, paste0(folder, "Detections.csv"))
save_data(dot, folder)
# write_csv(dot, paste0(folder, "dot.csv"))

writeLines(dot, con = paste0(folder, "dot.txt"))

# rm(list = ls())

# dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"