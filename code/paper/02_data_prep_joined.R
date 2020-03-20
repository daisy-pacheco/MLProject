library(tidyverse)
library(zoo)

# load data
cohort_79 <- load("prepped_79.RData")
cohort_97 <- load("prepped_97.RData")

# join data
joined_data <- rbind(cohort_79, cohort_97) # make sure have same columns first

# train test split

# imputation NOT DONE --- OLD VERSION!!!
## linear interpolation for values measured at least twice
## otherwise, mean imputation
imputed_data <- joined_data %>% 
  group_by(id) %>% 
  mutate(rosenberg_score = na.approx(rosenberg_score, na.rm = FALSE)) %>% 
  mutate(rosenberg_score = ifelse(is.na(rosenberg_score), 
                                  mean(rosenberg_score, na.rm = T), 
                                  rosenberg_score)) %>% 
  mutate(rotter_score = na.approx(rotter_score, na.rm = FALSE)) %>% 
  mutate(rotter_score = ifelse(is.na(rotter_score), 
                               mean(rotter_score, na.rm = T), 
                               rotter_score)) %>% 
  mutate(tenure = ifelse(is.na(tenure), 
                         mean(tenure, na.rm = T), 
                         tenure)) %>% 
  mutate(hours_worked_week = ifelse(is.na(hours_worked_week), 
                                    mean(hours_worked_week, na.rm = T), 
                                    hours_worked_week)) %>% 
  mutate(hourly_pay = ifelse(is.na(hourly_pay), 
                             mean(hourly_pay, na.rm = T), 
                             hourly_pay)) %>% 
  ungroup()

imputed_data_final <- imputed_data %>% 
  filter(hours_worked_week >= 35)

# centering NOT DONE --- OLD VERSION!!!
## group mean centering level 1 (job) variables
centered_data <- imputed_data_final %>%
  dplyr::group_by(id) %>% 
  dplyr:: mutate(hourly_pay_mean_per_person = mean(hourly_pay, na.rm = TRUE),
                 hourly_pay_centered = hourly_pay - hourly_pay_mean_per_person,
                 avg_age_per_job_mean_per_person = mean(avg_age_per_job, na.rm = TRUE),
                 avg_age_per_job_centered = avg_age_per_job - avg_age_per_job_mean_per_person,
                 tenure_mean_per_person = mean(tenure, na.rm = TRUE),
                 tenure_centered = tenure - tenure_mean_per_person,
                 hours_mean_per_person = mean(hours_worked_week, na.rm = TRUE),
                 hours_worked_week_centered = hours_worked_week - hours_mean_per_person) %>%
  dplyr::select(-hourly_pay_mean_per_person, -avg_age_per_job_mean_per_person, 
                -tenure_mean_per_person, -hours_mean_per_person) %>%
  ungroup

## grand mean centering level 2 (individual) variables
centered_data <- centered_data %>%
  dplyr:: mutate(rotter_score_centered = rotter_score - (mean(rotter_score, na.rm = TRUE)),
                 rosenberg_score_centered = rosenberg_score - (mean(rosenberg_score, na.rm = TRUE)))

# final transformations, dropping uncentered, dropping NAs
prepped_data <- centered_data %>% 
  dplyr::select(-age, -rosenberg_score, -rotter_score, -tenure, 
                -hourly_pay) %>% 
  dplyr::drop_na()

# save(prepped_data, file = "prepped_data.RData")