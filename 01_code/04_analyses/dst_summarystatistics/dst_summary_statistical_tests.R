# Script to perform statistical tests on the summary statistics of the dst depth logs

# 0. workspace ####
library(dplyr)
library(lubridate)
library(car)
# library(plotly)
# library(pracma)
# library(psdr)
# library(ggplot2)
# library(StreamMetabolism)
# library(suncalc)

rm(list = ls())

dir_path <- "C:/Users/lotte.pohl/Documents/github_repos/ADST_Mustelus_asterias"
# source(paste0(dir_path, "/02_scripts/04_analyses/dst_summarystatistics/dst_summary_calc.R"))
source(paste0(dir_path, "/02_scripts/02_load_data/load_dst_summarystatistics.R"))

# 1. prepare datasets ####

# manual separation of overwintering, migrating and oversummering periods of longterm dst tracks

dst_longterm_periods <- tibble(tag_serial_number = c(rep("1293308", times = 6), rep("1293321", times = 8)),
                               start_date = c("2018-08-02", "2018-08-22", "2018-10-10", "2019-03-23", "2019-05-13", "2019-08-04", 
                                        "2018-07-18", "2018-08-21", "2018-10-23", "2019-04-22", "2019-05-11", "2019-08-31", "2019-09-26", "2019-11-17") %>% 
                                 as.POSIXct(tz = "UTC"),
                               activity = c("summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "recording_end",
                                            "summer_residency", "winter_migration", "winter_residency", "summer_migration", "summer_residency", "winter_migration", "winter_residency", "recording_end")) %>%
  mutate(year = start_date %>% lubridate::year(),
         end_date = start_date %>% lead() %>% replace_na("2019-11-16" %>% as.POSIXct(tz = "UTC")),
         start_date = start_date - lubridate::days(1))


# dataframe with daily summary statistics and activity type

masterias_depth_daynight_activity <- masterias_depth_daynight %>% 
  filter(tag_serial_number %in% c("1293308", "1293321"), dusk == 0 & dawn == 0) %>% #filter out dusk and dawn: not used right now
  left_join(dst_longterm_periods,
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = date, y_lower = start_date, y_upper = end_date))) %>%
  dplyr::select(!c(start_date, end_date))


# dataframe with vertical movement behaviour and activity type

masterias_DVM_sum_day_activity <- masterias_DVM_sum_day %>% 
  filter(tag_serial_number %in% c("1293308", "1293321")) %>% 
  left_join(dst_longterm_periods,
            by = join_by(tag_serial_number == tag_serial_number,
                         between(x = date_24hcycle, y_lower = start_date, y_upper = end_date))) %>%
  dplyr::select(!c(start_date, end_date))

## function for comparison between 2 samples ####
compare_two_samples <- function(sample1, sample1_desc, sample2, sample2_desc, period_desc, question, results_path){
  
  # make df for levene test & to save
  sample1_df <- sample1 %>% as_tibble() %>%
    mutate(group = "A")
  sample2_df <- sample2 %>% as_tibble() %>%
    mutate(group = "B")
  df <- rbind(sample1_df, sample2_df)
  # save df
  save_data(data = df, folder = results_path)
  
  # 1. test if both samples are normally distributed: shapiro test
  stat_test <- if (shapiro.test(sample1)$p.value > 0.05 & shapiro.test(sample2)$p.value > 0.05) {
    
    # 1.1 if both samples normally distributed: check for equality of variances: levene test
    equalvar_test <- leveneTest(value ~ group, data  = df)
    equalvar_test <- equalvar_test %>% filter(rownames(equalvar_test) == "group")
    equalvar_method <- attributes(equalvar_test)$heading
    equalvar_pval <- equalvar_test$`Pr(>F)`
    equalvar_statistic <- equalvar_test$`F value`
    equalvar_conclusion <- ifelse(equalvar_pval < 0.05, 
                                  "p < 0.05: H0 (variances between groups are equal) rejected: Variances are NOT equal",
                                  "p > 0.05: Fail to reject H0 (variances between groups are equal): Variances ARE equal")
    
    if (equalvar_pval > 0.05) {
      # 1.2 if variances are equal: t-test
      t.test(sample1, sample2, var.equal = TRUE)
    } else {
      # 1.3 if variances are NOT equal: Welch's t-test
      t.test(sample1, sample2, var.equal = FALSE)
    }
  } else {
    # say that equality of variances was not tested bc samples are not normally distributed
    equalvar_method <- "not tested: samples not normal"
    equalvar_pval <- NA
    equalvar_statistic <- NA
    equalvar_conclusion <- NA
    
    # other test statistics for test of normality of samples
    shaptest_s1 <- shapiro.test(sample1)
    shaptest_s2 <- shapiro.test(sample2)
    shaptest_conclusion <- ifelse(shaptest_s1$p.value < 0.05 & shaptest_s2$p.value < 0.05, 
                                  "p < 0.05: H0 (sample comes from normal distribution) rejected: >= 1 sample is NOT normally distributed",
                                  "p > 0.05: Fail to reject H0 (sample comes from normal distribution): Sample IS normally distributed")
    
    # 2. If >=1 sample NOT normally distributed: Wilcoxon test
    wilcox.test(sample1, sample2)
  }
  
  conclusion <- ifelse(stat_test$p.value < 0.05, 
                       "p < 0.05: H0 (means/medians are equal) rejected: Means/medians are NOT equal",
                       "p > 0.05: Fail to reject H0 (smeans/medians are equal): Means/medians ARE equal")
  
  results_table <- tibble(
    question = question,
    sample1 = sample1_desc,
    sample2 = sample2_desc, 
    normality_method = shaptest_s1$method,
    normality_s1_pval = shaptest_s1$p.value,
    normality_s2_pval = shaptest_s2$p.value,
    normality_conclusion = shaptest_conclusion,
    equalvars_method = equalvar_method,
    equalvars_pval = equalvar_pval,
    equalvars_statistic = equalvar_statistic,
    equalvars_conclusion = equalvar_conclusion,
    stat_test_method = stat_test$method,
    stat_test_pval = stat_test$p.value,
    stat_test_statistic = stat_test$statistic,
    stat_test_conclusion = conclusion,
    comments = "")
  
  # save results
  save_data(data = results_table, folder = results_path)
  
  return(results_table)
}


# 2. DVM comparison between ####
# compare vertical movement in winter_migration 2018 and summer migration 2019 between 308 and 321

## 2.1 winter migration 2018: Significant ####
results_path_wintermigration_2018 <- paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_between/wintermigration_2018/")

### dataframe for test  
DVM_wintermigration_2018 <- masterias_DVM_sum_day_activity %>% filter(year == 2018, activity == "winter_migration")
### contingency table 
cont_table_DVM_wintermigration_2018 <- base::table(DVM_wintermigration_2018$vertical_movement, DVM_wintermigration_2018$tag_serial_number) # %>%
### result from chi 2 test 
result_DVM_wintermigration_2018 <- chisq.test(cont_table_DVM_wintermigration_2018)
### save results
save_data(data = DVM_wintermigration_2018, folder = results_path_wintermigration_2018)
save_data(data = cont_table_DVM_wintermigration_2018, folder = results_path_wintermigration_2018)
save_data(data = result_DVM_wintermigration_2018, folder = results_path_wintermigration_2018)

### summarise results 
results_summary_between_tracks <- tibble(question = "Do tag 308 and 321 show different vertical movement behaviour during their winter migration 2018?",
                                         H0 = "The vertical movements of tag 308 and 321 are independent/not associated during the winter migration 2018.",
                                         H1 = "The vertical movements of tag 308 and 321 are NOT independent/associated during the winter migration 2018.", 
                                         stat_test = result_DVM_wintermigration_2018$method,
                                         statistic = result_DVM_wintermigration_2018$statistic,
                                         pval = result_DVM_wintermigration_2018$p.value,
                                         conclusion = ifelse(pval < 0.05, "H0 rejected", "Fail to reject H0"),
                                         result = ifelse(pval < 0.05, H1, H0),
                                         results_path = results_path_wintermigration_2018,
                                         comments = "")

## 2.2 summer migration 2019: NS ####
results_path_summermigration_2019 <- paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_between/summermigration_2019/")

### dataframe for test
DVM_summermigration_2019 <- masterias_DVM_sum_day_activity %>% filter(year == 2019, activity == "summer_migration")
### contingency table
cont_table_DVM_summermigration_2019 <- base::table(DVM_summermigration_2019$vertical_movement, DVM_summermigration_2019$tag_serial_number) # %>%
### result from chi 2 test
result_DVM_summermigration_2019 <- chisq.test(cont_table_DVM_summermigration_2019)
### save results
save_data(data = DVM_summermigration_2019, folder = results_path_summermigration_2019)
save_data(data = cont_table_DVM_summermigration_2019, folder = results_path_summermigration_2019)
save_data(data = result_DVM_summermigration_2019, folder = results_path_summermigration_2019)

### summarise results
results_summary_between_tracks <- results_summary_between_tracks %>% 
  add_row(question = "Do tag 308 and 321 show different vertical movement behaviour during their summer migration 2019?",
                                         H0 = "The vertical movements of tag 308 and 321 are independent/not associated during the summer migration 2019.",
                                         H1 = "The vertical movements of tag 308 and 321 are NOT independent/associated during the summer migration 2019.", 
                                         stat_test = result_DVM_summermigration_2019$method,
                                         statistic = result_DVM_summermigration_2019$statistic,
                                         pval = result_DVM_summermigration_2019$p.value,
                                         conclusion = ifelse(pval < 0.05, "H0 rejected", "Fail to reject H0"),
                                         result = ifelse(pval < 0.05, H1, H0),
                                         results_path = results_path_summermigration_2019,
                                         comments = "one frequency is < 5, might cause chisq.test() to be incorrect")

## 2.2 summer residency 2019: NS ####
results_path_summerresidency_2019 <- paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_between/summerresidency_2019/")

### dataframe for test
DVM_summerresidency_2019 <- masterias_DVM_sum_day_activity %>% filter(year == 2019, activity == "summer_residency")
### contingency table
cont_table_DVM_summerresidency_2019 <- base::table(DVM_summerresidency_2019$vertical_movement, DVM_summerresidency_2019$tag_serial_number) # %>%
### result from chi 2 test
result_DVM_summerresidency_2019 <- chisq.test(cont_table_DVM_summerresidency_2019)
### save results
save_data(data = DVM_summerresidency_2019, folder = results_path_summerresidency_2019)
save_data(data = cont_table_DVM_summerresidency_2019, folder = results_path_summerresidency_2019)
save_data(data = result_DVM_summerresidency_2019, folder = results_path_summerresidency_2019)

### summarise results
results_summary_between_tracks <- results_summary_between_tracks %>% 
  add_row(question = "Do tag 308 and 321 show different vertical movement behaviour during their summer residency 2019?",
          H0 = "The vertical movements of tag 308 and 321 are independent/not associated during the summer residency 2019.",
          H1 = "The vertical movements of tag 308 and 321 are NOT independent/associated during the summer residency 2019.", 
          stat_test = result_DVM_summerresidency_2019$method,
          statistic = result_DVM_summerresidency_2019$statistic,
          pval = result_DVM_summerresidency_2019$p.value,
          conclusion = ifelse(pval < 0.05, "H0 rejected", "Fail to reject H0"),
          result = ifelse(pval < 0.05, H1, H0),
          results_path = results_path_summerresidency_2019,
          comments = "")

## save test result summary ####
save_data(data = results_summary_between_tracks, folder = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_between/"))
write_csv(results_summary_between_tracks, paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_between/results_summary_between_tracks.csv"))



# 3. daynight comparison within ####

## 3.1 winter migration 2018 ####

### 3.1.1 depth range day vs night ####

#### 3.1.1.1 f_308: Significant ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different depth range during the night than during the day?"
f_308_wintermig_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_range) %>% pull()

f_308_wintermig_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_range) %>% pull()

f_308_wintermigration2018_depthrange_daynight <- compare_two_samples(sample1 = f_308_wintermig_depthrange_night,
                                                                     sample2 = f_308_wintermig_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "f_308 winter migration 2018",
                                                                     question = "does the depth range of tag 308 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_range/f_308/"))
# ANSWER: Yes, it does!!
rm(f_308_wintermig_depthrange_day, f_308_wintermig_depthrange_night)

#### 3.1.1.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different depth range during the night than during the day?"
m_321_wintermig_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_range) %>% pull()

m_321_wintermig_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_range) %>% pull()

m_321_wintermigration2018_depthrange_daynight <- compare_two_samples(sample1 = m_321_wintermig_depthrange_night,
                                                                     sample2 = m_321_wintermig_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "m_321 winter migration 2018",
                                                                     question = "does the depth range of tag 321 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_range/m_321/"))
# ANSWER: Yes, it does!!
rm(m_321_wintermig_depthrange_night, m_321_wintermig_depthrange_day)

### 3.1.2 depth median day vs night ####

#### 3.1.2.1 f_308: NS ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different depth median during the night than during the day?"
f_308_wintermig_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_median) %>% pull()

f_308_wintermig_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_median) %>% pull()

f_308_wintermigration2018_depthmedian_daynight <- compare_two_samples(sample1 = f_308_wintermig_depthmedian_night,
                                                                     sample2 = f_308_wintermig_depthmedian_day,
                                                                     sample1_desc = "depth median night",
                                                                     sample2_desc = "depth median day",
                                                                     period_desc = "f_308 winter migration 2018",
                                                                     question = "does the depth median of tag 308 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_median/f_308/"))
f_308_wintermigration2018_depthmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_wintermig_depthmedian_day, f_308_wintermig_depthmedian_night)

#### 3.1.2.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different depth median during the night than during the day?"
m_321_wintermig_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_median) %>% pull()

m_321_wintermig_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_median) %>% pull()

m_321_wintermigration2018_depthmedian_daynight <- compare_two_samples(sample1 = m_321_wintermig_depthmedian_night,
                                                                     sample2 = m_321_wintermig_depthmedian_day,
                                                                     sample1_desc = "depth median night",
                                                                     sample2_desc = "depth median day",
                                                                     period_desc = "m_321 winter migration 2018",
                                                                     question = "does the depth median of tag 321 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_median/m_321/"))
m_321_wintermigration2018_depthmedian_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_depthmedian_night, m_321_wintermig_depthmedian_day)

### 3.1.3 depth max day vs night ####

#### 3.1.3.1 f_308: NS ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different depth max during the night than during the day?"
f_308_wintermig_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_max) %>% pull()

f_308_wintermig_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_max) %>% pull()

f_308_wintermigration2018_depthmax_daynight <- compare_two_samples(sample1 = f_308_wintermig_depthmax_night,
                                                                      sample2 = f_308_wintermig_depthmax_day,
                                                                      sample1_desc = "depth max night",
                                                                      sample2_desc = "depth max day",
                                                                      period_desc = "f_308 winter migration 2018",
                                                                      question = "does the depth max of tag 308 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_max/f_308/"))
f_308_wintermigration2018_depthmax_daynight %>% View()
# ANSWER: No!!
rm(f_308_wintermig_depthmax_day, f_308_wintermig_depthmax_night)

#### 3.1.3.2 m_321: NS ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different depth max during the night than during the day?"
m_321_wintermig_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_max) %>% pull()

m_321_wintermig_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_max) %>% pull()

m_321_wintermigration2018_depthmax_daynight <- compare_two_samples(sample1 = m_321_wintermig_depthmax_night,
                                                                      sample2 = m_321_wintermig_depthmax_day,
                                                                      sample1_desc = "depth max night",
                                                                      sample2_desc = "depth max day",
                                                                      period_desc = "m_321 winter migration 2018",
                                                                      question = "does the depth max of tag 321 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_max/m_321/"))
m_321_wintermigration2018_depthmax_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_depthmax_night, m_321_wintermig_depthmax_day)

### 3.1.4 depth min day vs night ####

#### 3.1.4.1 f_308: Significant ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different depth min during the night than during the day?"
f_308_wintermig_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_min) %>% pull()

f_308_wintermig_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_min) %>% pull()

f_308_wintermigration2018_depthmin_daynight <- compare_two_samples(sample1 = f_308_wintermig_depthmin_night,
                                                                      sample2 = f_308_wintermig_depthmin_day,
                                                                      sample1_desc = "depth min night",
                                                                      sample2_desc = "depth min day",
                                                                      period_desc = "f_308 winter migration 2018",
                                                                      question = "does the depth min of tag 308 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_min/f_308/"))
f_308_wintermigration2018_depthmin_daynight %>% View()
# ANSWER: No!!
rm(f_308_wintermig_depthmin_day, f_308_wintermig_depthmin_night)

#### 3.1.4.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different depth min during the night than during the day?"
m_321_wintermig_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(depth_min) %>% pull()

m_321_wintermig_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(depth_min) %>% pull()

m_321_wintermigration2018_depthmin_daynight <- compare_two_samples(sample1 = m_321_wintermig_depthmin_night,
                                                                      sample2 = m_321_wintermig_depthmin_day,
                                                                      sample1_desc = "depth min night",
                                                                      sample2_desc = "depth min day",
                                                                      period_desc = "m_321 winter migration 2018",
                                                                      question = "does the depth min of tag 321 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/depth_min/m_321/"))
m_321_wintermigration2018_depthmin_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_depthmin_night, m_321_wintermig_depthmin_day)



### 3.1.5 vertical_speed range day vs night ####

#### 3.1.5.1 f_308: Significant ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different vertical_speed range during the night than during the day?"
f_308_wintermig_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_wintermig_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_wintermigration2018_vertical_speedrange_daynight <- compare_two_samples(sample1 = f_308_wintermig_vertical_speedrange_night,
                                                                     sample2 = f_308_wintermig_vertical_speedrange_day,
                                                                     sample1_desc = "vertical_speed range night",
                                                                     sample2_desc = "vertical_speed range day",
                                                                     period_desc = "f_308 winter migration 2018",
                                                                     question = "does the vertical_speed range of tag 308 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_range/f_308/"))
f_308_wintermigration2018_vertical_speedrange_daynight %>% View()
# ANSWER: Yes, it does!!
rm(f_308_wintermig_vertical_speedrange_day, f_308_wintermig_vertical_speedrange_night)

#### 3.1.5.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different vertical_speed range during the night than during the day?"
m_321_wintermig_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_wintermig_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_wintermigration2018_vertical_speedrange_daynight <- compare_two_samples(sample1 = m_321_wintermig_vertical_speedrange_night,
                                                                     sample2 = m_321_wintermig_vertical_speedrange_day,
                                                                     sample1_desc = "vertical_speed range night",
                                                                     sample2_desc = "vertical_speed range day",
                                                                     period_desc = "m_321 winter migration 2018",
                                                                     question = "does the vertical_speed range of tag 321 during winter migration 2018 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_range/m_321/"))
m_321_wintermigration2018_vertical_speedrange_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_vertical_speedrange_night, m_321_wintermig_vertical_speedrange_day)

### 3.1.6 vertical_speed median day vs night ####

#### 3.1.6.1 f_308: NS ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different vertical_speed median during the night than during the day?"
f_308_wintermig_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_wintermig_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_wintermigration2018_vertical_speedmedian_daynight <- compare_two_samples(sample1 = f_308_wintermig_vertical_speedmedian_night,
                                                                      sample2 = f_308_wintermig_vertical_speedmedian_day,
                                                                      sample1_desc = "vertical_speed median night",
                                                                      sample2_desc = "vertical_speed median day",
                                                                      period_desc = "f_308 winter migration 2018",
                                                                      question = "does the vertical_speed median of tag 308 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_median/f_308/"))
f_308_wintermigration2018_vertical_speedmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_wintermig_vertical_speedmedian_day, f_308_wintermig_vertical_speedmedian_night)

#### 3.1.6.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different vertical_speed median during the night than during the day?"
m_321_wintermig_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_wintermig_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_wintermigration2018_vertical_speedmedian_daynight <- compare_two_samples(sample1 = m_321_wintermig_vertical_speedmedian_night,
                                                                      sample2 = m_321_wintermig_vertical_speedmedian_day,
                                                                      sample1_desc = "vertical_speed median night",
                                                                      sample2_desc = "vertical_speed median day",
                                                                      period_desc = "m_321 winter migration 2018",
                                                                      question = "does the vertical_speed median of tag 321 during winter migration 2018 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_median/m_321/"))
m_321_wintermigration2018_vertical_speedmedian_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_vertical_speedmedian_night, m_321_wintermig_vertical_speedmedian_day)

### 3.1.7 vertical_speed max day vs night ####

#### 3.1.7.1 f_308: Significant ####

# QUESTION: "During its winter migration 2018, does the female M. asterias have a significantly different vertical_speed max during the night than during the day?"
f_308_wintermig_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_wintermig_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_wintermigration2018_vertical_speedmax_daynight <- compare_two_samples(sample1 = f_308_wintermig_vertical_speedmax_night,
                                                                   sample2 = f_308_wintermig_vertical_speedmax_day,
                                                                   sample1_desc = "vertical_speed max night",
                                                                   sample2_desc = "vertical_speed max day",
                                                                   period_desc = "f_308 winter migration 2018",
                                                                   question = "does the vertical_speed max of tag 308 during winter migration 2018 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_max/f_308/"))
f_308_wintermigration2018_vertical_speedmax_daynight %>% View()
# ANSWER: Yes!!
rm(f_308_wintermig_vertical_speedmax_day, f_308_wintermig_vertical_speedmax_night)

#### 3.1.7.2 m_321: Significant ####

# QUESTION: "During its winter migration 2018, does the male M. asterias have a significantly different vertical_speed max during the night than during the day?"
m_321_wintermig_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_wintermig_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2018, activity == "winter_migration", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_wintermigration2018_vertical_speedmax_daynight <- compare_two_samples(sample1 = m_321_wintermig_vertical_speedmax_night,
                                                                   sample2 = m_321_wintermig_vertical_speedmax_day,
                                                                   sample1_desc = "vertical_speed max night",
                                                                   sample2_desc = "vertical_speed max day",
                                                                   period_desc = "m_321 winter migration 2018",
                                                                   question = "does the vertical_speed max of tag 321 during winter migration 2018 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/vertical_speed_max/m_321/"))
m_321_wintermigration2018_vertical_speedmax_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_wintermig_vertical_speedmax_night, m_321_wintermig_vertical_speedmax_day)

## save test result summary ####
winter_migration_2018_results <- rbind(f_308_wintermigration2018_depthrange_daynight,
                                       m_321_wintermigration2018_depthrange_daynight,
                                       f_308_wintermigration2018_depthmedian_daynight,
                                       m_321_wintermigration2018_depthmedian_daynight,
                                       f_308_wintermigration2018_vertical_speedrange_daynight,
                                       f_308_wintermigration2018_vertical_speedmedian_daynight,
                                       f_308_wintermigration2018_vertical_speedmax_daynight,
                                       m_321_wintermigration2018_vertical_speedrange_daynight,
                                       m_321_wintermigration2018_vertical_speedmedian_daynight,
                                       m_321_wintermigration2018_vertical_speedmax_daynight)

save_data(data = winter_migration_2018_results, folder = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_day_vs_night/"))
write_csv(winter_migration_2018_results, paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/winter_migration_2018_results.csv"))

## 3.2 summer migration 2019 ####

### 3.2.1 depth range day vs night ####

#### 3.2.1.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different depth range during the night than during the day?"
f_308_summermig_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_range) %>% pull()

f_308_summermig_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_range) %>% pull()

f_308_summermigration2019_depthrange_daynight <- compare_two_samples(sample1 = f_308_summermig_depthrange_night,
                                                                     sample2 = f_308_summermig_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "f_308 summer migration 2019",
                                                                     question = "does the depth range of tag 308 during summer migration 2019 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_range/f_308/"))
f_308_summermigration2019_depthrange_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_depthrange_day, f_308_summermig_depthrange_night)

#### 3.2.1.2 m_321: Significant ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different depth range during the night than during the day?"
m_321_summermig_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_range) %>% pull()

m_321_summermig_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_range) %>% pull()

m_321_summermigration2019_depthrange_daynight <- compare_two_samples(sample1 = m_321_summermig_depthrange_night,
                                                                     sample2 = m_321_summermig_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "m_321 summer migration 2019",
                                                                     question = "does the depth range of tag 321 during summer migration 2019 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_range/m_321/"))
m_321_summermigration2019_depthrange_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summermig_depthrange_night, m_321_summermig_depthrange_day)

### 3.2.2 depth median day vs night ####

#### 3.2.2.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different depth median during the night than during the day?"
f_308_summermig_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_median) %>% pull()

f_308_summermig_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_median) %>% pull()

f_308_summermigration2019_depthmedian_daynight <- compare_two_samples(sample1 = f_308_summermig_depthmedian_night,
                                                                      sample2 = f_308_summermig_depthmedian_day,
                                                                      sample1_desc = "depth median night",
                                                                      sample2_desc = "depth median day",
                                                                      period_desc = "f_308 summer migration 2019",
                                                                      question = "does the depth median of tag 308 during summer migration 2019 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_median/f_308/"))
f_308_summermigration2019_depthmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_depthmedian_day, f_308_summermig_depthmedian_night)

#### 3.2.2.2 m_321: NS ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different depth median during the night than during the day?"
m_321_summermig_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_median) %>% pull()

m_321_summermig_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_median) %>% pull()

m_321_summermigration2019_depthmedian_daynight <- compare_two_samples(sample1 = m_321_summermig_depthmedian_night,
                                                                      sample2 = m_321_summermig_depthmedian_day,
                                                                      sample1_desc = "depth median night",
                                                                      sample2_desc = "depth median day",
                                                                      period_desc = "m_321 summer migration 2019",
                                                                      question = "does the depth median of tag 321 during summer migration 2019 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_median/m_321/"))
m_321_summermigration2019_depthmedian_daynight %>% View()
# ANSWER: NO!!
rm(m_321_summermig_depthmedian_night, m_321_summermig_depthmedian_day)

### 3.2.3 depth max day vs night ####

#### 3.2.3.2 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different depth max during the night than during the day?"
f_308_summermig_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_max) %>% pull()

f_308_summermig_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_max) %>% pull()

f_308_summermigration2019_depthmax_daynight <- compare_two_samples(sample1 = f_308_summermig_depthmax_night,
                                                                   sample2 = f_308_summermig_depthmax_day,
                                                                   sample1_desc = "depth max night",
                                                                   sample2_desc = "depth max day",
                                                                   period_desc = "f_308 summer migration 2019",
                                                                   question = "does the depth max of tag 308 during summer migration 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_max/f_308/"))
f_308_summermigration2019_depthmax_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_depthmax_day, f_308_summermig_depthmax_night)

#### 3.2.3.2 m_321: NS ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different depth max during the night than during the day?"
m_321_summermig_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_max) %>% pull()

m_321_summermig_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_max) %>% pull()

m_321_summermigration2019_depthmax_daynight <- compare_two_samples(sample1 = m_321_summermig_depthmax_night,
                                                                   sample2 = m_321_summermig_depthmax_day,
                                                                   sample1_desc = "depth max night",
                                                                   sample2_desc = "depth max day",
                                                                   period_desc = "m_321 summer migration 2019",
                                                                   question = "does the depth max of tag 321 during summer migration 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_max/m_321/"))
m_321_summermigration2019_depthmax_daynight %>% View()
# ANSWER: No!!
rm(m_321_summermig_depthmax_night, m_321_summermig_depthmax_day)

### 3.2.4 depth min day vs night ####

#### 3.2.4.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different depth min during the night than during the day?"
f_308_summermig_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_min) %>% pull()

f_308_summermig_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_min) %>% pull()

f_308_summermigration2019_depthmin_daynight <- compare_two_samples(sample1 = f_308_summermig_depthmin_night,
                                                                   sample2 = f_308_summermig_depthmin_day,
                                                                   sample1_desc = "depth min night",
                                                                   sample2_desc = "depth min day",
                                                                   period_desc = "f_308 summer migration 2019",
                                                                   question = "does the depth min of tag 308 during summer migration 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_min/f_308/"))
f_308_summermigration2019_depthmin_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_depthmin_day, f_308_summermig_depthmin_night)

#### 3.2.4.2 m_321: Significant ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different depth min during the night than during the day?"
m_321_summermig_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(depth_min) %>% pull()

m_321_summermig_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(depth_min) %>% pull()

m_321_summermigration2019_depthmin_daynight <- compare_two_samples(sample1 = m_321_summermig_depthmin_night,
                                                                   sample2 = m_321_summermig_depthmin_day,
                                                                   sample1_desc = "depth min night",
                                                                   sample2_desc = "depth min day",
                                                                   period_desc = "m_321 summer migration 2019",
                                                                   question = "does the depth min of tag 321 during summer migration 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/depth_min/m_321/"))
m_321_summermigration2019_depthmin_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summermig_depthmin_night, m_321_summermig_depthmin_day)



### 3.2.5 vertical_speed range day vs night ####

#### 3.2.5.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different vertical_speed range during the night than during the day?"
f_308_summermig_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_summermig_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_summermigration2019_vertical_speedrange_daynight <- compare_two_samples(sample1 = f_308_summermig_vertical_speedrange_night,
                                                                              sample2 = f_308_summermig_vertical_speedrange_day,
                                                                              sample1_desc = "vertical_speed range night",
                                                                              sample2_desc = "vertical_speed range day",
                                                                              period_desc = "f_308 summer migration 2019",
                                                                              question = "does the vertical_speed range of tag 308 during summer migration 2019 differ between day and night?",
                                                                              results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_range/f_308/"))
f_308_summermigration2019_vertical_speedrange_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_vertical_speedrange_day, f_308_summermig_vertical_speedrange_night)

#### 3.2.5.2 m_321: NS ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different vertical_speed range during the night than during the day?"
m_321_summermig_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_summermig_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_summermigration2019_vertical_speedrange_daynight <- compare_two_samples(sample1 = m_321_summermig_vertical_speedrange_night,
                                                                              sample2 = m_321_summermig_vertical_speedrange_day,
                                                                              sample1_desc = "vertical_speed range night",
                                                                              sample2_desc = "vertical_speed range day",
                                                                              period_desc = "m_321 summer migration 2019",
                                                                              question = "does the vertical_speed range of tag 321 during summer migration 2019 differ between day and night?",
                                                                              results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_range/m_321/"))
m_321_summermigration2019_vertical_speedrange_daynight %>% View()
# ANSWER: No!!
rm(m_321_summermig_vertical_speedrange_night, m_321_summermig_vertical_speedrange_day)

### 3.2.6 vertical_speed median day vs night ####

#### 3.2.6.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different vertical_speed median during the night than during the day?"
f_308_summermig_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_summermig_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_summermigration2019_vertical_speedmedian_daynight <- compare_two_samples(sample1 = f_308_summermig_vertical_speedmedian_night,
                                                                               sample2 = f_308_summermig_vertical_speedmedian_day,
                                                                               sample1_desc = "vertical_speed median night",
                                                                               sample2_desc = "vertical_speed median day",
                                                                               period_desc = "f_308 summer migration 2019",
                                                                               question = "does the vertical_speed median of tag 308 during summer migration 2019 differ between day and night?",
                                                                               results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_median/f_308/"))
f_308_summermigration2019_vertical_speedmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_vertical_speedmedian_day, f_308_summermig_vertical_speedmedian_night)

#### 3.2.6.2 m_321: Significant ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different vertical_speed median during the night than during the day?"
m_321_summermig_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_summermig_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_summermigration2019_vertical_speedmedian_daynight <- compare_two_samples(sample1 = m_321_summermig_vertical_speedmedian_night,
                                                                               sample2 = m_321_summermig_vertical_speedmedian_day,
                                                                               sample1_desc = "vertical_speed median night",
                                                                               sample2_desc = "vertical_speed median day",
                                                                               period_desc = "m_321 summer migration 2019",
                                                                               question = "does the vertical_speed median of tag 321 during summer migration 2019 differ between day and night?",
                                                                               results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_median/m_321/"))
m_321_summermigration2019_vertical_speedmedian_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summermig_vertical_speedmedian_night, m_321_summermig_vertical_speedmedian_day)

### 3.2.7 vertical_speed max day vs night ####

#### 3.2.7.1 f_308: NS ####

# QUESTION: "During its summer migration 2019, does the female M. asterias have a significantly different vertical_speed max during the night than during the day?"
f_308_summermig_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_summermig_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_summermigration2019_vertical_speedmax_daynight <- compare_two_samples(sample1 = f_308_summermig_vertical_speedmax_night,
                                                                            sample2 = f_308_summermig_vertical_speedmax_day,
                                                                            sample1_desc = "vertical_speed max night",
                                                                            sample2_desc = "vertical_speed max day",
                                                                            period_desc = "f_308 summer migration 2019",
                                                                            question = "does the vertical_speed max of tag 308 during summer migration 2019 differ between day and night?",
                                                                            results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_max/f_308/"))
f_308_summermigration2019_vertical_speedmax_daynight %>% View()
# ANSWER: No!!
rm(f_308_summermig_vertical_speedmax_day, f_308_summermig_vertical_speedmax_night)

#### 3.2.7.2 m_321: Significant ####

# QUESTION: "During its summer migration 2019, does the male M. asterias have a significantly different vertical_speed max during the night than during the day?"
m_321_summermig_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_summermig_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_migration", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_summermigration2019_vertical_speedmax_daynight <- compare_two_samples(sample1 = m_321_summermig_vertical_speedmax_night,
                                                                            sample2 = m_321_summermig_vertical_speedmax_day,
                                                                            sample1_desc = "vertical_speed max night",
                                                                            sample2_desc = "vertical_speed max day",
                                                                            period_desc = "m_321 summer migration 2019",
                                                                            question = "does the vertical_speed max of tag 321 during summer migration 2019 differ between day and night?",
                                                                            results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/vertical_speed_max/m_321/"))
m_321_summermigration2019_vertical_speedmax_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summermig_vertical_speedmax_night, m_321_summermig_vertical_speedmax_day)

## save test result summary ####
summer_migration_2019_results <- rbind(f_308_summermigration2019_depthrange_daynight,
                                       m_321_summermigration2019_depthrange_daynight,
                                       f_308_summermigration2019_depthmedian_daynight,
                                       m_321_summermigration2019_depthmedian_daynight,
                                       f_308_summermigration2019_vertical_speedrange_daynight,
                                       f_308_summermigration2019_vertical_speedmedian_daynight,
                                       f_308_summermigration2019_vertical_speedmax_daynight,
                                       m_321_summermigration2019_vertical_speedrange_daynight,
                                       m_321_summermigration2019_vertical_speedmedian_daynight,
                                       m_321_summermigration2019_vertical_speedmax_daynight)

save_data(data = summer_migration_2019_results, folder = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_day_vs_night/"))
write_csv(x = summer_migration_2019_results, file =  paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_migration_2019_results.csv"))



## 3.3 summer residency 2019 ####

### 3.3.1 depth range day vs night ####

#### 3.3.1.1 f_308: Significant ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different depth range during the night than during the day?"
f_308_summerres_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_range) %>% pull()

f_308_summerres_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_range) %>% pull()

f_308_summerresidency2019_depthrange_daynight <- compare_two_samples(sample1 = f_308_summerres_depthrange_night,
                                                                     sample2 = f_308_summerres_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "f_308 summer residency 2019",
                                                                     question = "does the depth range of tag 308 during summer residency 2019 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_range/f_308/"))
f_308_summerresidency2019_depthrange_daynight %>% View()
# ANSWER: Yes!!
rm(f_308_summerres_depthrange_day, f_308_summerres_depthrange_night)

#### 3.3.1.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different depth range during the night than during the day?"
m_321_summerres_depthrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_range) %>% pull()

m_321_summerres_depthrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_range) %>% pull()

m_321_summerresidency2019_depthrange_daynight <- compare_two_samples(sample1 = m_321_summerres_depthrange_night,
                                                                     sample2 = m_321_summerres_depthrange_day,
                                                                     sample1_desc = "depth range night",
                                                                     sample2_desc = "depth range day",
                                                                     period_desc = "m_321 summer residency 2019",
                                                                     question = "does the depth range of tag 321 during summer residency 2019 differ between day and night?",
                                                                     results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_range/m_321/"))
m_321_summerresidency2019_depthrange_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summerres_depthrange_night, m_321_summerres_depthrange_day)

### 3.3.2 depth median day vs night ####

#### 3.3.2.1 f_308: NS ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different depth median during the night than during the day?"
f_308_summerres_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_median) %>% pull()

f_308_summerres_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_median) %>% pull()

f_308_summerresidency2019_depthmedian_daynight <- compare_two_samples(sample1 = f_308_summerres_depthmedian_night,
                                                                      sample2 = f_308_summerres_depthmedian_day,
                                                                      sample1_desc = "depth median night",
                                                                      sample2_desc = "depth median day",
                                                                      period_desc = "f_308 summer residency 2019",
                                                                      question = "does the depth median of tag 308 during summer residency 2019 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_median/f_308/"))
f_308_summerresidency2019_depthmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_summerres_depthmedian_day, f_308_summerres_depthmedian_night)

#### 3.3.2.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different depth median during the night than during the day?"
m_321_summerres_depthmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_median) %>% pull()

m_321_summerres_depthmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_median) %>% pull()

m_321_summerresidency2019_depthmedian_daynight <- compare_two_samples(sample1 = m_321_summerres_depthmedian_night,
                                                                      sample2 = m_321_summerres_depthmedian_day,
                                                                      sample1_desc = "depth median night",
                                                                      sample2_desc = "depth median day",
                                                                      period_desc = "m_321 summer residency 2019",
                                                                      question = "does the depth median of tag 321 during summer residency 2019 differ between day and night?",
                                                                      results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_median/m_321/"))
m_321_summerresidency2019_depthmedian_daynight %>% View()
# ANSWER: Yes!!
rm(m_321_summerres_depthmedian_night, m_321_summerres_depthmedian_day)

### 3.3.3 depth max day vs night ####

#### 3.3.3.3 f_308: NS ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different depth max during the night than during the day?"
f_308_summerres_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_max) %>% pull()

f_308_summerres_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_max) %>% pull()

f_308_summerresidency2019_depthmax_daynight <- compare_two_samples(sample1 = f_308_summerres_depthmax_night,
                                                                   sample2 = f_308_summerres_depthmax_day,
                                                                   sample1_desc = "depth max night",
                                                                   sample2_desc = "depth max day",
                                                                   period_desc = "f_308 summer residency 2019",
                                                                   question = "does the depth max of tag 308 during summer residency 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_max/f_308/"))
f_308_summerresidency2019_depthmax_daynight %>% View()
# ANSWER: No!!
rm(f_308_summerres_depthmax_day, f_308_summerres_depthmax_night)

#### 3.3.3.3 m_321: NS ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different depth max during the night than during the day?"
m_321_summerres_depthmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_max) %>% pull()

m_321_summerres_depthmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_max) %>% pull()

m_321_summerresidency2019_depthmax_daynight <- compare_two_samples(sample1 = m_321_summerres_depthmax_night,
                                                                   sample2 = m_321_summerres_depthmax_day,
                                                                   sample1_desc = "depth max night",
                                                                   sample2_desc = "depth max day",
                                                                   period_desc = "m_321 summer residency 2019",
                                                                   question = "does the depth max of tag 321 during summer residency 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_max/m_321/"))
m_321_summerresidency2019_depthmax_daynight %>% View()
# ANSWER: No!!
rm(m_321_summerres_depthmax_night, m_321_summerres_depthmax_day)

### 3.3.4 depth min day vs night ####

#### 3.3.4.1 f_308: Significant ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different depth min during the night than during the day?"
f_308_summerres_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_min) %>% pull()

f_308_summerres_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_min) %>% pull()

f_308_summerresidency2019_depthmin_daynight <- compare_two_samples(sample1 = f_308_summerres_depthmin_night,
                                                                   sample2 = f_308_summerres_depthmin_day,
                                                                   sample1_desc = "depth min night",
                                                                   sample2_desc = "depth min day",
                                                                   period_desc = "f_308 summer residency 2019",
                                                                   question = "does the depth min of tag 308 during summer residency 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_min/f_308/"))
f_308_summerresidency2019_depthmin_daynight %>% View()
# ANSWER: Yes!!
rm(f_308_summerres_depthmin_day, f_308_summerres_depthmin_night)

#### 3.3.4.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different depth min during the night than during the day?"
m_321_summerres_depthmin_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(depth_min) %>% pull()

m_321_summerres_depthmin_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(depth_min) %>% pull()

m_321_summerresidency2019_depthmin_daynight <- compare_two_samples(sample1 = m_321_summerres_depthmin_night,
                                                                   sample2 = m_321_summerres_depthmin_day,
                                                                   sample1_desc = "depth min night",
                                                                   sample2_desc = "depth min day",
                                                                   period_desc = "m_321 summer residency 2019",
                                                                   question = "does the depth min of tag 321 during summer residency 2019 differ between day and night?",
                                                                   results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/depth_min/m_321/"))
m_321_summerresidency2019_depthmin_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summerres_depthmin_night, m_321_summerres_depthmin_day)



### 3.3.5 vertical_speed range day vs night ####

#### 3.3.5.1 f_308: Significant ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different vertical_speed range during the night than during the day?"
f_308_summerres_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_summerres_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

f_308_summerresidency2019_vertical_speedrange_daynight <- compare_two_samples(sample1 = f_308_summerres_vertical_speedrange_night,
                                                                              sample2 = f_308_summerres_vertical_speedrange_day,
                                                                              sample1_desc = "vertical_speed range night",
                                                                              sample2_desc = "vertical_speed range day",
                                                                              period_desc = "f_308 summer residency 2019",
                                                                              question = "does the vertical_speed range of tag 308 during summer residency 2019 differ between day and night?",
                                                                              results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_range/f_308/"))
f_308_summerresidency2019_vertical_speedrange_daynight %>% View()
# ANSWER: Yes!!
rm(f_308_summerres_vertical_speedrange_day, f_308_summerres_vertical_speedrange_night)

#### 3.3.5.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different vertical_speed range during the night than during the day?"
m_321_summerres_vertical_speedrange_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_summerres_vertical_speedrange_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_range) %>% pull()

m_321_summerresidency2019_vertical_speedrange_daynight <- compare_two_samples(sample1 = m_321_summerres_vertical_speedrange_night,
                                                                              sample2 = m_321_summerres_vertical_speedrange_day,
                                                                              sample1_desc = "vertical_speed range night",
                                                                              sample2_desc = "vertical_speed range day",
                                                                              period_desc = "m_321 summer residency 2019",
                                                                              question = "does the vertical_speed range of tag 321 during summer residency 2019 differ between day and night?",
                                                                              results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_range/m_321/"))
m_321_summerresidency2019_vertical_speedrange_daynight %>% View()
# ANSWER: Yes!!
rm(m_321_summerres_vertical_speedrange_night, m_321_summerres_vertical_speedrange_day)

### 3.3.6 vertical_speed median day vs night ####

#### 3.3.6.1 f_308: NS ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different vertical_speed median during the night than during the day?"
f_308_summerres_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_summerres_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

f_308_summerresidency2019_vertical_speedmedian_daynight <- compare_two_samples(sample1 = f_308_summerres_vertical_speedmedian_night,
                                                                               sample2 = f_308_summerres_vertical_speedmedian_day,
                                                                               sample1_desc = "vertical_speed median night",
                                                                               sample2_desc = "vertical_speed median day",
                                                                               period_desc = "f_308 summer residency 2019",
                                                                               question = "does the vertical_speed median of tag 308 during summer residency 2019 differ between day and night?",
                                                                               results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_median/f_308/"))
f_308_summerresidency2019_vertical_speedmedian_daynight %>% View()
# ANSWER: No!!
rm(f_308_summerres_vertical_speedmedian_day, f_308_summerres_vertical_speedmedian_night)

#### 3.3.6.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different vertical_speed median during the night than during the day?"
m_321_summerres_vertical_speedmedian_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_summerres_vertical_speedmedian_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_median) %>% pull()

m_321_summerresidency2019_vertical_speedmedian_daynight <- compare_two_samples(sample1 = m_321_summerres_vertical_speedmedian_night,
                                                                               sample2 = m_321_summerres_vertical_speedmedian_day,
                                                                               sample1_desc = "vertical_speed median night",
                                                                               sample2_desc = "vertical_speed median day",
                                                                               period_desc = "m_321 summer residency 2019",
                                                                               question = "does the vertical_speed median of tag 321 during summer residency 2019 differ between day and night?",
                                                                               results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_median/m_321/"))
m_321_summerresidency2019_vertical_speedmedian_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summerres_vertical_speedmedian_night, m_321_summerres_vertical_speedmedian_day)

### 3.3.7 vertical_speed max day vs night ####

#### 3.3.7.1 f_308: Significant ####

# QUESTION: "During its summer residency 2019, does the female M. asterias have a significantly different vertical_speed max during the night than during the day?"
f_308_summerres_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_summerres_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293308", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

f_308_summerresidency2019_vertical_speedmax_daynight <- compare_two_samples(sample1 = f_308_summerres_vertical_speedmax_night,
                                                                            sample2 = f_308_summerres_vertical_speedmax_day,
                                                                            sample1_desc = "vertical_speed max night",
                                                                            sample2_desc = "vertical_speed max day",
                                                                            period_desc = "f_308 summer residency 2019",
                                                                            question = "does the vertical_speed max of tag 308 during summer residency 2019 differ between day and night?",
                                                                            results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_max/f_308/"))
f_308_summerresidency2019_vertical_speedmax_daynight %>% View()
# ANSWER: Yes!!
rm(f_308_summerres_vertical_speedmax_day, f_308_summerres_vertical_speedmax_night)

#### 3.3.7.2 m_321: Significant ####

# QUESTION: "During its summer residency 2019, does the male M. asterias have a significantly different vertical_speed max during the night than during the day?"
m_321_summerres_vertical_speedmax_night <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 0) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_summerres_vertical_speedmax_day <- masterias_depth_daynight_activity %>% ungroup() %>%
  filter(tag_serial_number == "1293321", year == 2019, activity == "summer_residency", day == 1) %>% dplyr::select(vertical_speed_max) %>% pull()

m_321_summerresidency2019_vertical_speedmax_daynight <- compare_two_samples(sample1 = m_321_summerres_vertical_speedmax_night,
                                                                            sample2 = m_321_summerres_vertical_speedmax_day,
                                                                            sample1_desc = "vertical_speed max night",
                                                                            sample2_desc = "vertical_speed max day",
                                                                            period_desc = "m_321 summer residency 2019",
                                                                            question = "does the vertical_speed max of tag 321 during summer residency 2019 differ between day and night?",
                                                                            results_path = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/vertical_speed_max/m_321/"))
m_321_summerresidency2019_vertical_speedmax_daynight %>% View()
# ANSWER: Yes, it does!!
rm(m_321_summerres_vertical_speedmax_night, m_321_summerres_vertical_speedmax_day)

## save test result summary ####
summer_residency_2019_results <- rbind(f_308_summerresidency2019_depthrange_daynight,
                                       m_321_summerresidency2019_depthrange_daynight,
                                       f_308_summerresidency2019_depthmedian_daynight,
                                       m_321_summerresidency2019_depthmedian_daynight,
                                       f_308_summerresidency2019_vertical_speedrange_daynight,
                                       f_308_summerresidency2019_vertical_speedmedian_daynight,
                                       f_308_summerresidency2019_vertical_speedmax_daynight,
                                       m_321_summerresidency2019_vertical_speedrange_daynight,
                                       m_321_summerresidency2019_vertical_speedmedian_daynight,
                                       m_321_summerresidency2019_vertical_speedmax_daynight)

save_data(data = summer_residency_2019_results, folder = paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_day_vs_night/"))
write_csv(x = summer_residency_2019_results, file =  paste0(dir_path, "/04_analysis_results/dst_summary/longterm_tracks_comparison_within/summer_residency_2019_results.csv"))
