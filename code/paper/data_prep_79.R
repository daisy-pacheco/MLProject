library(tidyverse)

original_data <- read_csv('data/paper/fulldata_79.csv')
names_dictionary <- read.csv('data/paper/dictionary_79.csv', sep = ";")
cpi <- read_csv('data/paper/CPI.csv')

tidy_data <- original_data %>% 
  tidyr::pivot_longer(
    -c(R0000100, R0214700, R0214800, R0010300),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::left_join(
    names_dictionary
  ) %>% 
  dplyr::filter(value >= 0) %>% 
  dplyr::rename(
    id = R0000100,
    ethnicity = R0214700,
    gender = R0214800,
    religion = R0010300
  ) %>% 
  dplyr::select(-variable_id, -religion, -ethnicity, -gender)

yearly_variables <- tidy_data %>% 
  filter(is.na(job_number)) %>% 
  select(id, year, variable, value) %>% 
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  select(-highest_grade, -urban_rural, -net_family_income, 
         -military_pay, -sample_id)

tidy_data_fix <- tidy_data %>% 
  filter(!is.na(job_number)) %>% 
  left_join(yearly_variables)

columnar_data <- tidy_data_fix %>% 
  group_by(id, variable, job_number, year, job_satisfaction_global, 
           rotter_score, age, rosenberg_score) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  left_join(cpi) %>% 
  select(-row)

data_mutations <- columnar_data %>% 
  mutate(
    job_satisfaction = 
      case_when(
        currently_working == 1 & is.na(job_satisfaction) ~ job_satisfaction_global,
        currently_working == 0 & is.na(job_satisfaction) ~ NA_real_,
        currently_working == NA ~ NA_real_,
        TRUE ~ job_satisfaction
        )
    ) %>% 
  ungroup() %>%
  select(-job_satisfaction_global) %>% 
  mutate(
    job_satisfaction = dplyr::case_when(
      job_satisfaction == 4 ~ 1,
      job_satisfaction == 3 ~ 2,
      job_satisfaction == 2 ~ 3,
      job_satisfaction == 1 ~ 4,
      TRUE ~ job_satisfaction
    )
  ) %>% 
  mutate(
    hourly_pay = log((hourly_pay * cpi) + 1),
    tenure = tenure / 52,
    avg_age_job_year = ((age - tenure) + age) / 2,
    public = case_when(year <= 1993 & sector == 2 ~ 1, 
                       year > 1993 & sector == 1 ~ 1,
                       TRUE ~ 0)
  ) %>% 
  filter(public == 1 & 
           !is.na(employer_id) & 
           !is.na(job_satisfaction)) %>% 
  select(id, year, job_number, employer_id, 
         job_satisfaction,
         age, avg_age_job_year, tenure,
         hours_worked_week, hourly_pay,
         rotter_score, rosenberg_score)

# imputation 
## linear interpolation for values measured at least twice
## otherwise, mean imputation
library(zoo)

imputed_data <- data_mutations %>% 
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


### COME BACK TO CENTERING LATER ###
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

save(prepped_data, file = "prepped_data_79.RData")
