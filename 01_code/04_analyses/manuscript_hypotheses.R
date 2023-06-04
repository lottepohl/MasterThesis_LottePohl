# Script to make statistical tests for hypotheses

# Workspace ####

# rm(list = ls())

## libraries ####

library(ggplot2)
library(plotly)
library(dplyr)
library(car)
library(MASS)
library(scales)
library(gridExtra)
library(pracma)
library(oce)
library(ggpubr)

## plot path ####
dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/MasterThesis_LottePohl"
path_maps <- paste0(dir_path, "/01_code/00_thesis_manuscript/maps/")
plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")
models_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/models/")
paste0(dir_path, "/01_code/06_functions/functions.R") %>% base::source()

## load data ####
paste0(dir_path, "/01_code/02_load_data/load_dst_summarystatistics.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_acoustic_detections.R") %>% base::source()
# to do: choose df's to load to reduce workspace size
# paste0(dir_path, "/01_code/02_load_data/load_wavelet_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_autocorrelation_results.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/load_depth_temp_logs.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_fft_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_cpd_results.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/load_vertical_space_use_analysis.R") %>% base::source()
paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_tables.R") %>% base::source()
# paste0(dir_path, "/01_code/02_load_data/manuscript_figures/load_models.R") %>% base::source()

## set path were all figures are saved ####
# plot_path <- paste0(dir_path, "/01_code/00_thesis_manuscript/figures/")


# 1.a Females show higher residency in the area of the PBARN receivers than males. ####

# exploration

plot_exploration <- ggplot(data = tagged_animal_info %>% group_by(tag_serial_number), aes(x = sex, y = residency_index)) +
  geom_point()

plot_exploration %>% ggplotly()

sample1 <- tagged_animal_info %>% dplyr::filter(sex == "m") %>% dplyr::select(residency_index) %>% drop_na() %>% dplyr::pull()
sample2 <- tagged_animal_info %>% dplyr::filter(sex == "f") %>% dplyr::select(residency_index) %>% drop_na() %>% dplyr::pull()

median(sample1, na.rm = T)
median(sample2, na.rm = T)

# check for normality of the data

# if p > 0.05: samples are normally distributed
shapiro.test(sample1)
shapiro.test(sample2)
# both samples are normally distributed!

# check for equaloty of variances 
# if p > 0.05: variances are equal
levene_hyp1 <- car::leveneTest(residency_index ~ sex, data = tagged_animal_info %>% drop_na(residency_index))
levene_hyp1$`Pr(>F)`[1]

# if p < 0.05: median of samples is different
hypothesis1_result <- wilcox.test(sample1, sample2)
hypothesis1_result$p.value

hypothesis1a_result <- t.test(sample2, sample1, alternative = "greater", var.equal = F)
hypothesis1a_result$p.value

save_data(data = hypothesis1_result, folder = models_path)
# 
# # 1.b the Females are detected more by the receivers of the PBARN than males. ####
# 
# # data exploration
# 
# # data_hypothesis1b <- detections_tempdepth_daynight %>% group_by(tag_serial_number) %>% summarise(n_detect = )
# 
# ggplot(data = tagged_animal_info, aes(x = sex, y = n_detect)) +
#   # geom_point() 
#   geom_boxplot()
# 
# sample1 <- tagged_animal_info %>% dplyr::filter(sex == "m") %>% dplyr::select(n_detect) %>% drop_na() %>% dplyr::pull() 
# sample2 <- tagged_animal_info %>% dplyr::filter(sex == "f") %>% dplyr::select(n_detect) %>% drop_na() %>% dplyr::pull() 
# 
# median(sample1, na.rm = T)
# median(sample2, na.rm = T)
# 
# # if p > 0.05: samples are normally distributed
# shapiro.test(sample1)
# shapiro.test(sample2)
# # not both samples are normally distributed, thus: wilcoxon test
# 
# # if p < 0.05: median of samples is different
# hypothesis1b_result <- wilcox.test(sample1, sample2)
# hypothesis1b_result$p.value
# 
# save_data(data = hypothesis1b_result, folder = models_path)
# 
# 
# # 1.c the females are more in the WS area than the males ####
# 
# # there is only one male detected in the WS area....doesn't make sense to compare with n = 1
# data_hyp1c <- detections_tempdepth_daynight %>% dplyr::filter(!area == "BPNS") %>% group_by(tag_serial_number) %>% summarise(n_detect = n(), sex = sex %>% unique())
# 
# ggplot(data = data_hyp1c, aes(x = sex, y = n_detect)) +
#   # geom_point() 
#   geom_boxplot()

# 1.d Females show higher residency in the area of the PBARN receivers than males. ####

## with days at liberty > 28 (== 4 weeks)

# there is only one male detected in the WS area....doesn't make sense to compare with n = 1
data_hyp1d <- tagged_animal_info %>% dplyr::filter(days_at_liberty > 0) %>% dplyr::select(residency_index, tag_serial_number, sex) %>% drop_na() #%>% dplyr::pull()

ggplot(data = data_hyp1d, aes(x = sex, y = residency_index)) +
  # geom_point() 
  geom_boxplot()

males <- data_hyp1d %>% dplyr::filter(sex == "m") %>% dplyr::select(residency_index) %>% dplyr::pull() 
females <- data_hyp1d %>% dplyr::filter(sex == "f") %>% dplyr::select(residency_index) %>% dplyr::pull() 

median(males)
median(females)

# if p > 0.05: samples are normally distributed
shapiro_males_hyp1 <- shapiro.test(males)
shapiro_males_hyp1$p.value
shapiro_females_hyp1 <- shapiro.test(females)
shapiro_females_hyp1$p.value
# both samples are normally distributed, thus: wilcoxon test

# check equality of variances
# if p > 0.05: variances are NOT different, i.e. equal
bartlett_hyp1 <- bartlett.test(residency_index ~ sex, data = data_hyp1d) # if samples = normally distributed: bartlett test
bartlett_hyp1$p.value
# p < 0.05: variances are not equal

# check for equality of variances 
levene_hyp1 <- car::leveneTest(residency_index ~ sex, data = tagged_animal_info %>% drop_na(residency_index))
levene_hyp1 <- car::leveneTest(residency_index ~ sex, data = data_hyp1d)
levene_hyp1$`Pr(>F)`[1]
# if p < 0.05: median of samples is different
hypothesis1_result <- wilcox.test(sample1, sample2)
hypothesis1_result$p.value

hypothesis1_wilcox_greater <- wilcox.test(sample2, sample1, alternative = "greater")
hypothesis1_wilcox_greater$p.value

# if p < 0.05: median of samples is different
hypothesis1d_result <- t.test(females, males, alternative = "greater", var.equal = F)
hypothesis1d_result$p.value

# if p < 0.05: median of samples is different
hypothesis1d_result_varequal <- t.test(females, males, alternative = "greater", var.equal = T) #acc to levene test vars are equal
hypothesis1d_result_varequal$p.value

# hypothesis1d_nonparam_result <- wilcox.test(sample1, sample2) # with non parametric alternative: not significant
# hypothesis1d_nonparam_result$p.value

hyp1_results <- tibble(test_aim = c("normality_m", "normality_f", "equality_of_variances", "equality_of_variances", "equality_of_means", "equality_of_medians"),
                       stat_test = c("shapiro", "shapiro", "bartlett", "levene", "t.test (var.equal = F, alternative = greater)", "wilcoxon rank sum"),
                       p_val = c(shapiro_males_hyp1$p.value, shapiro_females_hyp1$p.value, bartlett_hyp1$p.value, levene_hyp1$`Pr(>F)`[1], hypothesis1d_result$p.value, hypothesis1_result$p.value),
                       interpretation = c("if p>0.05: normal dist", "if p>0.05: normal dist", "if p>0.05: vars are equal", "if p>0.05: vars are equal", "if p<0.05: means differ", "if p<0.05: medians differ"))

save_data(data = hyp1_results, folder = models_path)


# hypothesis 2 ####

seasons_startdates <- tibble(season = c("spring", "summer", "autumn", "winter", "spring", "summer", "autumn", "winter"),
                             phenomenon = c("equinox", "solstice", "equinox", "solstice", "equinox", "solstice", "equinox", "solstice"),
                             start_date = c("2018-03-20", "2018-06-21", "2018-09-23", "2018-12-21", "2019-03-20", "2019-06-21", "2019-09-23", "2019-12-22")) %>% #from https://aa.usno.navy.mil/calculated/seasons?year=2018&tz=0.00&tz_sign=-1&tz_label=false&dst=false&submit=Get+Data
                      dplyr::mutate(start_date = start_date %>% as.POSIXct(),
                                    end_date = dplyr::lead(start_date, default = "2020-03-20" %>% as.POSIXct()) - lubridate::days(0))


data_hyp2 <- long_dst_date %>% left_join(seasons_startdates, by = join_by(between(date, start_date, end_date))) %>%
  mutate(season_year = paste0(lubridate::year(date), "-", season),
         season_year = ifelse(lubridate::month(date) < 6 & season_year == "2019-winter", "2018-winter", season_year),
         year = lubridate::year(date),
         big_season = ifelse(season == "spring", "summer", ifelse(season == "autumn", "winter", season)),
         big_season_year = paste0(lubridate::year(date), "-", big_season),
         big_season_year = ifelse(lubridate::month(date) < 6 & big_season_year == "2019-winter", "2018-winter", big_season_year),
  )

data_hyp2_308 <- data_hyp2 %>% dplyr::filter(tag_serial_number == "1293308")
data_hyp2_321 <- data_hyp2 %>% dplyr::filter(tag_serial_number == "1293321")

# data_hyp2_308 %>% dplyr::select(date, depth_median, depth_median_sgolay, season, season_year) %>% View()


## 2. check for normality of depths: large sample size, thus KS test

# if p<0.05: non normal distribution
ks_results_hyp2 <- data_hyp2 %>% group_by(tag_serial_number, season_year) %>%
  summarise(ks_test_pval = ks.test(depth_median %>% unique(), "pnorm")$p.value)
# nothing follows a normal distribution


## try different transformations

# shark 308

log308 <- data_hyp2_308$depth_median %>% log()
log308 %>% as.data.frame() %>% View()
ggpubr::ggqqplot(log308)
sqrt308 <- data_hyp2_308$depth_median %>% sqrt()
ggpubr::ggqqplot(sqrt308)


### boxcox transformation

# 308
#test with autumn 2018

data_test_308 <- data_hyp2_308 %>% dplyr::filter(season_year == "2018-autumn")
data_test_308_2 <- data_hyp2_308 %>% dplyr::filter(season_year == "2018-winter")

ggplot(data = data_test_308, aes(x = depth_median)) +
  geom_histogram()

ggplot(data = data_test_308_2, aes(x = depth_median)) +
  geom_histogram()

model308 <- lm(depth_median ~ 1, data = data_test_308)
bc <- boxcox(model308)
(lambda <- bc$x[which.max(bc$y)]) #get optimal lambda
new_model308 <- lm(((depth_median^lambda-1)/lambda) ~ 1, data = data_test_308)

ggpubr::ggqqplot(model308$residuals)
ggpubr::ggqqplot(new_model308$residuals)

gridExtra::grid.arrange(ggpubr::ggqqplot(model308$residuals) + labs(title = "original median depth"),
                        ggpubr::ggqqplot(new_model308$residuals) + labs(title = "lamda transformed median depth"))

shapiro.test(data_test_308$depth_median)

# test
data <- data_hyp2_308 %>% dplyr::filter(season_year == "2018-autumn")
variable_name <- "depth_median"

data$depth_median %>% as.data.frame() %>% View()

boxcox_transform <- function(data, variable_name){
  sample <- data %>% 
    dplyr::select(all_of(variable_name)) %>%
    dplyr::rename(variable = variable_name)
  
  bc <- boxcox(variable ~ 1, data = sample, plotit = F)
  lambda <- bc$x[which.max(bc$y)] #get optimal lambda
  
  sample_transformed <- ((sample$variable^lambda-1)/lambda) %>% 
    as.tibble() %>% 
    `colnames<-`(paste0(variable_name, "_transformed")) %>%
    mutate(lambda = lambda)
  
  residuals <- lm(sample_transformed[,1] %>% pull() ~ 1, data = sample_transformed)
  
  sample_transformed <- sample_transformed %>% 
    dplyr::mutate(residuals = lm(sample_transformed[,1] %>% pull() ~ 1, data = sample_transformed)$residuals,
                  original_variable = sample$variable)
  
  sample_transformed %>% return()
}

# test function

test_bc <- boxcox_transform(data = data_hyp2_321 %>% filter(season_year == "2018-winter"),
                            variable_name = "depth_median")

ggpubr::ggqqplot(test_bc$residuals)

data_hyp2_308 <- data_hyp2_308 %>% group_by(season_year) %>%
  dplyr::mutate(transform = boxcox_transform(., variable_name = "depth_median")[1])


# tag 308, do each season separately


tag308_2018_summer <- boxcox_transform(data = data_hyp2_308 %>% 
                                         dplyr::filter(season_year == "2018-summer"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-summer")

tag308_2018_autumn <- boxcox_transform(data = data_hyp2_308 %>% 
                                         dplyr::filter(season_year == "2018-autumn"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-autumn")

tag308_2018_winter <- boxcox_transform(data = data_hyp2_308 %>% 
                                         dplyr::filter(season_year == "2018-winter"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-winter")

tag308_2019_spring <- boxcox_transform(data = data_hyp2_308 %>% 
                                         dplyr::filter(season_year == "2019-spring"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2019-spring")

tag308_2019_summer <- boxcox_transform(data = data_hyp2_308 %>% 
                                         dplyr::filter(season_year == "2019-summer"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2019-summer")


tag308_depth_median_boxcox <- rbind(tag308_2018_summer, tag308_2018_autumn, tag308_2018_winter, tag308_2019_spring, tag308_2019_summer) %>%
  dplyr::mutate(year = str_split_i(season_year, pattern="-", i = 1),
                season = str_split_i(season_year, pattern="-", i = 2))

# tag 321 

tag321_2018_summer <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2018-summer"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-summer")

tag321_2018_autumn <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2018-autumn"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-autumn")

tag321_2018_winter <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2018-winter"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2018-winter")

tag321_2019_spring <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2019-spring"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2019-spring")

tag321_2019_summer <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2019-summer"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2019-summer")

tag321_2019_autumn <- boxcox_transform(data = data_hyp2_321 %>% 
                                         dplyr::filter(season_year == "2019-autumn"),
                                       variable_name = "depth_median") %>%
  dplyr::mutate(season_year = "2019-autumn")

tag321_depth_median_boxcox <- rbind(tag321_2018_summer, tag321_2018_autumn, tag321_2018_winter, tag321_2019_spring, tag321_2019_summer, tag321_2019_autumn) %>%
  dplyr::mutate(year = str_split_i(season_year, pattern="-", i = 1),
                season = str_split_i(season_year, pattern="-", i = 2))


ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2018-summer") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2018-autumn") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2018-winter") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2019-spring") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2019-summer") %>% dplyr::select(depth_median_transformed) %>% pull())
ggqqplot(tag321_depth_median_boxcox %>% dplyr::filter(season_year == "2019-autumn") %>% dplyr::select(depth_median_transformed) %>% pull()) 

ggqqplot(tag308_depth_median_boxcox %>% dplyr::filter(season_year == "2018-summer") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag308_depth_median_boxcox %>% dplyr::filter(season_year == "2018-autumn") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag308_depth_median_boxcox %>% dplyr::filter(season_year == "2018-winter") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag308_depth_median_boxcox %>% dplyr::filter(season_year == "2019-spring") %>% dplyr::select(depth_median_transformed) %>% pull()) 
ggqqplot(tag308_depth_median_boxcox %>% dplyr::filter(season_year == "2019-summer") %>% dplyr::select(depth_median_transformed) %>% pull())

ggplot(data = tag321_depth_median_boxcox, aes(x = depth_median_transformed)) + geom_histogram() + facet_grid(season_year ~ .)
ggplot(data = tag308_depth_median_boxcox, aes(x = depth_median_transformed)) + geom_histogram() + facet_grid(season_year ~ .)


# finally do ANOVA

# but forst some more exploration

ggplot(data = tag321_depth_median_boxcox, aes(x = season_year, y = depth_median_transformed)) +
  geom_boxplot()

# but since ANOVA is robust to non normality: repeated measures anova
aov(depth_median_transformed~factor(season)+Error(factor(year)), data = tag321_depth_median_boxcox)


# 2. new approach: t test ####

data_hyp2.2 <- long_dst_date %>% left_join(seasons_startdates, by = join_by(between(date, start_date, end_date))) %>%
  mutate(season_year = paste0(lubridate::year(date), "-", season),
         season_year = ifelse(lubridate::month(date) < 6 & season_year == "2019-winter", "2018-winter", season_year),
         year = lubridate::year(date),
         big_season = ifelse(season == "spring", "summer", ifelse(season == "autumn", "winter", season)),
         big_season_year = paste0(lubridate::year(date), "-", big_season),
         big_season_year = ifelse(lubridate::month(date) < 6 & big_season_year == "2019-winter", "2018-winter", big_season_year),
  )

# check for normality of depths for winter and summer
# ks_results_hyp2.2 <- data_hyp2.2 %>% group_by(tag_serial_number, big_season_year) %>%
#   summarise(ks_test_pval = ks.test(depth_median %>% unique(), "pnorm")$p.value)
ks_results_hyp2.2 <- data_hyp2.2 %>% group_by(tag_serial_number, big_season) %>%
  summarise(ks_test_pval = ks.test(depth_median %>% unique(), "pnorm")$p.value)
# not normally distributed

save_data(data = ks_results_hyp2.2, folder = models_path)

# check equality of variances
# if p > 0.05: variances are NOT different, i.e. equal
bartlett_hyp2 <- bartlett.test(depth_median ~ big_season, data = data_hyp2.2) # if samples = normally distributed: bartlett test
bartlett_hyp2$p.value
# p < 0.05: variances are not equal

# do t test (although not normally distributed and no equal variances)
hypothesis2.2_testresults <- data_hyp2.2 %>% group_by(tag_serial_number) %>% 
  summarise(t.test_pval = t.test(depth_median[big_season == "summer"], depth_median[big_season == "winter"], var.equal = F)$p.value,
            wilcox.test_pval = wilcox.test(depth_median[big_season == "summer"], depth_median[big_season == "winter"], var.equal = F)$p.value)
# if p < 0.05: median of samples is different

save_data(data = hypothesis2.2_testresults, folder = models_path)

# old ####

# 321

ggplot(data = data_hyp2_321, aes(x = depth_median)) +
  geom_histogram()

model321 <- lm(depth_median ~ 1, data = data_hyp2_321)
bc <- boxcox(model321)
(lambda <- bc$x[which.max(bc$y)]) #get optimal lambda
new_model321 <- lm(((depth_median^lambda-1)/lambda) ~ 1, data = data_hyp2_321)

ggpubr::ggqqplot(model321$residuals)
ggpubr::ggqqplot(new_model321$residuals)

gridExtra::grid.arrange(ggpubr::ggqqplot(model321$residuals) + labs(title = "original median depth"),
                        ggpubr::ggqqplot(new_model321$residuals) + labs(title = "lamda transformed median depth"))



## do statistical tests ####

# thus, non-parametric alternative of repeated measures anova: friedman test!
friedman.test(y=data_hyp2_308$depth_median, groups=data_hyp2_308$season, blocks=data_hyp2_308$year)