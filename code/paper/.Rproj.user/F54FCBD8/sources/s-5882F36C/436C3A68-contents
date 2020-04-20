library(tidyverse)
library(zoo)

# load data
load("data/paper/prepped_79.RData")
load("data/paper/prepped_97.RData")

# join data
joined_data <- rbind(final_data_79, final_data_97) 

# train test split
unique_ids <- joined_data$id %>% unique
train_id_sample <- sample(1:length(unique_ids), 
                   size = ceiling(0.7 * length(unique_ids)))

train_ids <- unique_ids[train_id_sample]
test_ids <- unique_ids[-train_id_sample]

train_data <- joined_data[joined_data$id %in% train_ids, ]
test_data <- joined_data[joined_data$id %in% test_ids, ]

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
  filter(hours_worked_week >= 35)

# centering 
centerData <- function(data){
  centered_data <- data %>% 
    # group mean centering level 1 (job) variables
    group_by(id) %>% 
    mutate(
      hourly_pay_mean_per_person = mean(hourly_pay, na.rm = TRUE),
      hourly_pay_centered = hourly_pay - hourly_pay_mean_per_person,
      avg_age_job_year_per_person = mean(avg_age_job_year, na.rm = TRUE),
      avg_age_job_year_centered = avg_age_job_year - avg_age_job_year_per_person,
      tenure_mean_per_person = mean(tenure, na.rm = TRUE),
      tenure_centered = tenure - tenure_mean_per_person,
      hours_mean_per_person = mean(hours_worked, na.rm = TRUE),
      hours_worked_centered = hours_worked - hours_mean_per_person
    ) %>%
    ungroup() %>% 
    # grand mean centering level 2 (individual) variables
    mutate(
      personality_1_centered = personality_1 - mean(personality_1, na.rm = TRUE),
      personality_2_centered = personality_2 - mean(personality_2, na.rm = TRUE),
      personality_3_centered = personality_3 - mean(personality_3, na.rm = TRUE),
      personality_4_centered = personality_4 - mean(personality_4, na.rm = TRUE),
      personality_5_centered = personality_5 - mean(personality_5, na.rm = TRUE),
      personality_6_centered = personality_6 - mean(personality_6, na.rm = TRUE),
      personality_7_centered = personality_7 - mean(personality_7, na.rm = TRUE),
      personality_8_centered = personality_8 - mean(personality_8, na.rm = TRUE),
      personality_9_centered = personality_9 - mean(personality_9, na.rm = TRUE),
      personality_10_centered = personality_10 - mean(personality_10, na.rm = TRUE)
    ) %>% 
    select(-hourly_pay_mean_per_person, -avg_age_job_year_per_person, 
           -tenure_mean_per_person, -hours_mean_per_person)

  return(centered_data)
}

centered_train_data <- centerData(imputed_train_data)
centered_test_data <- centerData(imputed_test_data)

# drop remaining NAs and unused columns
cleanData <- function(data){
  clean_data <- data %>% 
    select(id, year, job_number, employer_id,
           job_satisfaction_scaled, hourly_pay_centered, 
           avg_age_job_year_centered, tenure_centered,
           hours_worked_centered, personality_1_centered,
           personality_2_centered, personality_3_centered,
           personality_4_centered, personality_5_centered, 
           personality_6_centered, personality_7_centered, 
           personality_8_centered, personality_9_centered, 
           personality_10_centered) %>% 
    drop_na()

  return(clean_data)
}

clean_train_data <- cleanData(centered_train_data)
clean_test_data <- cleanData(centered_test_data)
