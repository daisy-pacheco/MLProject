library(tidyverse)

original_data_79 <- read_csv('data/paper/fulldata_79.csv')
names_dictionary_79 <- read.csv('data/paper/dictionary_79.csv', sep = ";")
cpi <- read_csv('data/paper/CPI.csv')

tidy_data_79 <- original_data_79 %>% 
  tidyr::pivot_longer(
    -c(R0000100, R0214700, R0214800, R0010300),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::left_join(
    names_dictionary_79
  ) %>% 
  dplyr::filter(value >= 0) %>% 
  dplyr::rename(
    id = R0000100,
    ethnicity = R0214700,
    gender = R0214800,
    religion = R0010300
  ) %>% 
  dplyr::select(-variable_id, -religion, 
                -ethnicity, -gender)

personality_vars_79 <- tidy_data_79 %>% 
  filter(grepl("personality", variable)) %>% 
  mutate(value = ifelse(value < 1, NA, value)) %>% 
  select(-job_number) %>% 
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  group_by(id) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  select(-year)

yearly_variables_79 <- tidy_data_79 %>% 
  dplyr::filter(variable %in% c("job_satisfaction_global", "age")) %>% 
  dplyr::select(id, year, variable, value) %>%
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

tidy_data_fix_79 <- tidy_data_79 %>% 
  filter(!is.na(job_number)) %>% 
  left_join(yearly_variables_79)

columnar_data_79 <- tidy_data_fix_79 %>% 
  group_by(id, variable, job_number, 
           year, job_satisfaction_global, age) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  left_join(cpi) %>% 
  select(-row)

data_mutations_79 <- columnar_data_79 %>% 
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
         hours_worked_week, hourly_pay)

mutations_with_personality_79 <- data_mutations_79 %>% 
  left_join(personality_vars_79) %>% 
  mutate(id = paste(id, "_79"),
         cohort = "1979")

# save(mutations_with_personality_79, file = "./data/prepped_79.RData")