library(tidyverse)
library(scales)

original_data_97 <- read_csv("data/paper/fulldata_97.csv")
names_dictionary_97 <- read.csv("data/paper/dictionary_97.csv", sep = ";")
cpi <- read_csv('data/paper/CPI.csv')

tidy_data_97 <- original_data_97 %>% 
  tidyr::pivot_longer(
    -c(R0000100, R1482600, R0536300, R0552200),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::left_join(
    names_dictionary_97
  ) %>% 
  dplyr::filter(value >= 0)  %>% 
  dplyr::rename(
    id = R0000100,
    ethnicity = R1482600,
    male = R0536300,
    religion = R0552200
  ) %>% 
  dplyr::select(-variable_id) 

personality_vars_97 <- tidy_data_97 %>% 
  filter(grepl("personality", variable)) %>% 
  select(-job_number, -year, -male, -religion, -ethnicity) %>% 
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
  group_by(id) %>%
  fill(highest_grade, .direction = c("down")) %>% 
  fill(highest_grade, .direction = c("up")) %>%   
  ungroup() %>% 
  dplyr::mutate(
    cohort = "1997",
    start_date = as.Date(paste0("1", "/", start_month, "/", start_year), format("%d/%m/%Y")),
    stop_date = as.Date(paste0("1", "/", stop_month, "/", stop_year), format("%d/%m/%Y")),
    tenure = as.numeric((stop_date - start_date) / 365),
    hourly_pay = log((hourly_pay * cpi) + 1),
    family_income = log((family_income * cpi) + 1),
    avg_age_job_year = ((age - tenure) + age) / 2,
    public = ifelse(industry != 1 | is.na(industry), 0, 1),
    male = as_factor(
      ifelse(male == 1, 1, 0)
    ),
    white = as_factor(
      ifelse(ethnicity == 4, 1, 0)
      ),
    religious = as_factor(
      ifelse(religion %in% c(25, 26, 27), 0, 1)
    ), 
    urban = as_factor(
      ifelse(urban == 2, NA, urban)
    )
  ) %>% 
  dplyr::filter(
    public == 1 & 
      !is.na(employer_id) & 
      !is.na(job_satisfaction)
  ) %>% 
  select(-religion, -age, -start_month, -start_year, 
         -stop_month, -stop_year, -industry, -cpi, -start_date,
         -stop_date, -public, -ethnicity)

mutations_with_personality_97 <- data_mutations_97 %>% 
  left_join(personality_vars_97) %>% 
  mutate(id = paste(id, "_97"))

# rescale dependent variable
final_data_97 <- mutations_with_personality_97 %>% 
  mutate(job_satisfaction = rescale(job_satisfaction, newrange = c(1, 10)))

# save(final_data_97, file = "./data/paper/prepped_97.RData")