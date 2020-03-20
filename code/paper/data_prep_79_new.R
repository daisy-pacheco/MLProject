library(tidyverse)

original_data_79 <- read_csv('data/paper/fulldata_79.csv')
names_dictionary_79 <- read.csv('data/paper/dictionary_79.csv', sep = ";")
cpi <- read_csv('data/paper/CPI.csv')

tidy_personality <- original_data_79 %>% 
  select(R0000100, 
         T4998600, 
         T4998601, 
         T4998602, 
         T4998603, 
         T4998604, 
         T4998605,
         T4998606, 
         T4998607, 
         T4998608, 
         T4998609, 
         T5733900,
         T5733901, 
         T5733902, 
         T5733903, 
         T5733904, 
         T5733905,
         T5733906, 
         T5733907, 
         T5733908, 
         T5733909) %>% 
  dplyr::rename(
    id = R0000100,
    personality_14_1 = T4998600,
    personality_14_2 = T4998601,
    personality_14_3 = T4998602,
    personality_14_4 = T4998603,
    personality_14_5 = T4998604,
    personality_14_6 = T4998605,
    personality_14_7 = T4998606,
    personality_14_8 = T4998607,
    personality_14_9 = T4998608,
    personality_14_10 = T4998609,
    personality_16_1 = T5733900,
    personality_16_2 = T5733901,
    personality_16_3 = T5733902,
    personality_16_4 = T5733903,
    personality_16_5 = T5733904,
    personality_16_6 = T5733905,
    personality_16_7 = T5733906,
    personality_16_8 = T5733907,
    personality_16_9 = T5733908,
    personality_16_10 =  T5733909
  ) %>% 
  tidyr::pivot_longer(
    -c(id),
    names_to = "variable_id",
    values_to = "value",
    values_drop_na = TRUE
  ) %>% 
  dplyr::filter(value >= 0)
    
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

yearly_variables_79 <- tidy_data_79 %>% 
  dplyr::filter(is.na(job_number)) %>% 
  dplyr::select(id, year, variable, value) %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) 
