library(tidyverse)

original_data_97 <- read_csv("data/paper/fulldata_97.csv")
names_dictionary_97 <- read.csv("data/paper/dictionary_97.csv", sep = ";")

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

yearly_variables_97 <- tidy_data_97 %>% 
  filter(is.na(job_number)) %>% 
  select(id, year, variable, value) %>% 
  pivot_wider(
    names_from = variable,
    values_from = value
  )

tidy_data_fix_97 <- tidy_data_97 %>% 
  filter(!is.na(job_number)) %>% 
  left_join(yearly_variables_97)

columnar_data_97 <- tidy_data_fix_97 %>% 
  group_by(id, variable, job_number, year, age) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  left_join(cpi)

data_mutations_97 <- columnar_data_97 %>% 
  mutate(
    job_satisfaction = dplyr::case_when(
      job_satisfaction == 5 ~ 1,
      job_satisfaction == 4 ~ 2,
      job_satisfaction == 3 ~ 3,
      job_satisfaction == 2 ~ 4,
      job_satisfaction == 1 ~ 5,
      TRUE ~ job_satisfaction
    )
  ) %>% 
  mutate(
    start_date = as.Date(paste0("1", "/", start_month, "/", start_year), format("%d/%m/%Y")),
    stop_date = as.Date(paste0("1", "/", stop_month, "/", stop_year), format("%d/%m/%Y")),
    tenure = (stop_date - start_date) / 365,
    avg_age_job_year = ((age - tenure) + age) / 2,
    public = ifelse(industry == 1, 1,0)
  ) %>% 
  filter(public == 1 & 
           !is.na(employer_id) & 
           !is.na(job_satisfaction))



test <- columnar_data_97 %>% 
  group_by(id, employer_id, year) %>% 
  summarize(mean_satisfaction = mean(job_satisfaction, na.rm = TRUE))
