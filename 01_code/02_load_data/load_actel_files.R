
dir_path <- getwd() #"C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source(paste0(dir_path, "/functions.R"))
paste0(getwd(), "/01_code/06_functions/functions.R") %>% source()

Biometrics <- load_data(filestring = "Biometrics", folder = path_actel)
Spatial <- load_data(filestring = "Spatial", folder = path_actel)
Deployments <- load_data(filestring = "Deployments", folder = path_actel)
Detections <- load_data(filestring = "Detections", folder = path_actel)
dot <- load_data(filestring = "dot", folder = path_actel)
# spatial <- my_data <- read_delim("spatial.txt") #, delim = "\t")
