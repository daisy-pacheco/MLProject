library(tidyverse)

original_data <- read_csv('testdata.csv')
names_dictionary <- read_csv('test-dict.csv')

tidy_data <- original_data %>% 
  tidyr::pivot_longer(
    -c(R0000100),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::left_join(
    names_dictionary
  ) %>% 
  dplyr::filter(value >= 0)  %>% 
  dplyr::rename(
    id = R0000100
  ) %>% 
  dplyr::select(-variable_id)

columnar_data <- tidy_data %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

test <- columnar_data %>% 
  group_by(id, employer_id) %>% 
  summarize(mean_satisfaction = mean(job_satisfaction, na.rm = TRUE))
