library(tidyverse)

original_data_97 <- read_csv("data/paper/fulldata_97.csv")
names_dictionary_97 <- read.csv("data/paper/dictionary_97.csv", sep = ";")
cpi <- read_csv('data/paper/CPI.csv')

tidy_data_97 <- original_data_97 %>% 
  tidyr::pivot_longer(
    -c(R0000100),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::left_join(
    names_dictionary_97
  ) %>% 
  dplyr::filter(value >= 0)  %>% 
  dplyr::rename(
    id = R0000100
  ) %>% 
  dplyr::select(-variable_id) 

personality_vars_97 <- tidy_data_97 %>% 
  filter(grepl("personality", variable)) %>% 
  select(-job_number, -year) %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

yearly_variables_97 <- tidy_data_97 %>% 
  dplyr::filter(is.na(job_number)) %>% 
  dplyr::filter(!grepl("personality", variable)) %>% 
  dplyr::select(id, year, variable, value) %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

tidy_data_fix_97 <- tidy_data_97 %>% 
  dplyr::filter(!is.na(job_number)) %>% 
  dplyr::left_join(yearly_variables_97)

columnar_data_97 <- tidy_data_fix_97 %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  dplyr::left_join(cpi)

data_mutations_97 <- columnar_data_97 %>% 
  dplyr::mutate(
    job_satisfaction = dplyr::case_when(
      job_satisfaction == 5 ~ 1,
      job_satisfaction == 4 ~ 2,
      job_satisfaction == 3 ~ 3,
      job_satisfaction == 2 ~ 4,
      job_satisfaction == 1 ~ 5,
      TRUE ~ job_satisfaction
    )
  ) %>%
  dplyr::mutate(
    unique_employer_id = paste0(id, "_", employer_id),
    start_date = as.Date(paste0("1", "/", start_month, "/", start_year), format("%d/%m/%Y")),
    stop_date = as.Date(paste0("1", "/", stop_month, "/", stop_year), format("%d/%m/%Y")),
    hourly_pay = log((hourly_pay * cpi) + 1),
    tenure = as.numeric((stop_date - start_date) / 365),
    avg_age_job_year = ((age - tenure) + age) / 2,
    public = ifelse(industry != 1 | is.na(industry), 0, 1)
  ) %>% 
  dplyr::group_by(id) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    public == 1 & 
      !is.na(employer_id) & 
      !is.na(job_satisfaction)
  ) %>% 
  dplyr::select(
    id, 
    year, 
    job_number, 
    employer_id, 
    job_satisfaction,
    age, 
    avg_age_job_year, 
    tenure,
    hours_worked, 
    hourly_pay, 
    unique_employer_id
  ) 

mutations_with_personality_97 <- data_mutations_97 %>% 
  left_join(personality_vars_97) 


### IMPUTATION NOT ADAPTED YET ###
# imputation 
## linear interpolation for values measured at least twice
## otherwise, mean imputation
library(zoo)

imputed_data_97 <- data_mutations_97 %>% 
  group_by(id) %>% 
  mutate(tenure = ifelse(is.na(tenure), 
                         mean(tenure, na.rm = T), 
                         tenure)) %>% 
  mutate(hours_worked = ifelse(is.na(hours_worked), 
                               mean(hours_worked, na.rm = T), 
                               hours_worked)) %>% 
  mutate(hourly_pay = ifelse(is.na(hourly_pay), 
                             mean(hourly_pay, na.rm = T), 
                             hourly_pay)) %>% 
  ungroup()

imputed_data_final_97 <- imputed_data_97 %>% 
  filter(hours_worked >= 35)



test <- columnar_data_97 %>% 
  group_by(id, employer_id, year) %>% 
  summarize(mean_satisfaction = mean(job_satisfaction, na.rm = TRUE))
