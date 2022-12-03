library(tidyverse)
library(tidyverse)
library(zoo)
library(VIM)
library(mice)

set.seed(0)

# read data
original_data <- readr::read_csv('data/fulldata.csv')
names_dictionary <- readr::read_csv('data/dictionary.csv')
cpi <- readr::read_csv('data/CPI.csv')

# tidy data
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
  dplyr::select(-variable_id)

yearly_variables <- tidy_data %>% 
  dplyr::filter(is.na(job_number)) %>% 
  dplyr::select(id, year, variable, value) %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )

tidy_data_fix <- tidy_data %>% 
  dplyr::filter(!is.na(job_number)) %>% 
  dplyr::left_join(yearly_variables)
  
columnar_data <- tidy_data_fix %>% 
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  dplyr::left_join(cpi)

data_mutations <- columnar_data %>% 
  dplyr::mutate(
    job_satisfaction = 
      dplyr::case_when(
        currently_working == 1 & is.na(job_satisfaction) ~ job_satisfaction_global,
        currently_working == 0 & is.na(job_satisfaction) ~ NA_real_,
        currently_working == NA ~ NA_real_,
        TRUE ~ job_satisfaction
        )
    ) %>% 
  dplyr::select(-job_satisfaction_global) %>% 
  dplyr::mutate(
    job_satisfaction = dplyr::case_when(
      job_satisfaction == 4 ~ 1,
      job_satisfaction == 3 ~ 2,
      job_satisfaction == 2 ~ 3,
      job_satisfaction == 1 ~ 4,
      TRUE ~ job_satisfaction
    )
  ) %>% 
  dplyr::mutate(
    hourly_pay = log((hourly_pay * cpi) + 1),
    net_family_income = log((net_family_income * cpi) + 1),
    tenure = tenure / 52
  ) %>% 
  dplyr::mutate(avg_age_per_job = ((age - tenure) + age) / 2) %>% 
  group_by(id) %>%
  fill(highest_grade, .direction = c("down")) %>% # when the direction is down the missing is replaced with the previous value within id
  fill(highest_grade, .direction = c("up")) %>%   # when direction is up the replacement of missing will be with the next available value
  ungroup %>% 
  dplyr::mutate(
    gender = as_factor(
      ifelse(gender == 1, "male","female")
    ),
    ethnicity = as_factor(dplyr::case_when(
      ethnicity == 1 ~ "hispanic",
      ethnicity == 2 ~ "black",
      ethnicity == 3 ~ "non_hispanic_or_black"
    )),
    religion = as_factor(dplyr::case_when(
      religion == 0 ~ "no_religion",
      religion %in% c(1, 2, 3, 4, 5, 6) ~ "christian",
      religion == 7 ~ "catholic",
      religion == 8 ~ "jewish",
      religion == 9 ~ 'other'
    )),
    urban = as_factor(
      ifelse(urban_rural, 1, 0)
    ),
    highest_grade = as_factor(
      dplyr::case_when(
        highest_grade <= 8 ~ "primary_school",
        highest_grade <= 12 ~ "high_school",
        highest_grade <= 16 ~ "college",
        highest_grade <= 20 ~ "advanced",
        highest_grade < 100 ~ "other"
      )
    ),
    union = as_factor(
      ifelse(union == 1, 1, 0)
    ),
    industry = as_factor(dplyr::case_when(
      year <= 2000 & industry >= 17 & industry <= 28 ~ "agriculture",
      year <= 2000 & industry >= 47 & industry <= 57 ~ "mining",
      year <= 2000 & industry >= 67 & industry <= 77 ~ "construction",
      year <= 2000 & industry >= 107 & industry <= 398 ~ "manufacturing",
      year <= 2000 & industry >= 407 & industry <= 479 ~ "transportation_utilities",
      year <= 2000 & industry >= 507 & industry <= 698 ~ "sales",
      year <= 2000 & industry >= 707 & industry <= 718 ~ "finance_real_estate",
      year <= 2000 & industry >= 727 & industry <= 759 ~ "business_repair",
      year <= 2000 & industry >= 769 & industry <= 798 ~ "personalservices",
      year <= 2000 & industry >= 807 & industry <= 809 ~ "entertainment",
      year <= 2000 & industry >= 828 & industry <= 897 ~ "professionalservices",
      year <= 2000 & industry >= 907 & industry <= 937 ~ "publicadmin",
      year > 2000 & industry >= 170 & industry <= 290 ~ "agriculture",
      year > 2000 & industry >= 370 & industry <= 490 ~ "mining",
      year > 2000 & industry >= 570 & industry <= 690 ~ "transportation_utilities",
      year > 2000 & industry == 770 ~ "construction",
      year > 2000 & industry >= 1070 & industry <= 3990 ~ "manufacturing",
      year > 2000 & industry >= 4070 & industry <= 4590 ~ "sales",
      year > 2000 & industry >= 4670 & industry <= 5790 ~ "sales",
      year > 2000 & industry >= 6070 & industry <= 6390 ~ "transportation_utilities",
      year > 2000 & industry >= 6470 & industry <= 6780 ~ "information",
      year > 2000 & industry >= 6870 & industry <= 6990 ~ "finance_real_estate",
      year > 2000 & industry >= 7070 & industry <= 7190 ~ "finance_real_estate",
      year > 2000 & industry >= 7270 & industry <= 7490 ~ "professionalservices",
      year > 2000 & industry >= 7570 & industry <= 7790 ~ "professionalservices",
      year > 2000 & industry >= 7860 & industry <= 7890 ~ "educationalservices",
      year > 2000 & industry >= 7970 & industry <= 8470 ~ "health_care",
      year > 2000 & industry >= 8560 & industry <= 8590 ~ "entertainment",
      year > 2000 & industry >= 8660 & industry <= 8690 ~ "service_industry",
      year > 2000 & industry >= 8770 & industry <= 9290 ~ "service_industry",
      year > 2000 & industry >= 9370 & industry <= 9870 ~ "pulicadmin",
      TRUE ~ 'other'
    ))
  )
  
job_satisfaction_data <- data_mutations %>% 
  dplyr::filter(!is.na(job_satisfaction)) %>%  # drop any rows without data for the target
  dplyr::select(-military_pay, -sample_id)

# train-test split (for predictive MERF model)
sample <- sample(c(TRUE, FALSE), nrow(job_satisfaction_data), replace=TRUE, prob=c(0.7,0.3))
train_data  <- job_satisfaction_data[sample, ]
test_data   <- job_satisfaction_data[!sample, ]

# handling missing data
imputeData <- function(data){
  mean_imputed <- data %>% 
    group_by(id) %>%
    dplyr::mutate(net_family_income = ifelse(is.na(net_family_income), 
                                             mean(net_family_income, na.rm = T), 
                                             net_family_income)) %>% 
    dplyr::mutate(rosenberg_score = na.approx(rosenberg_score, na.rm=FALSE)) %>% 
    dplyr::mutate(rosenberg_score = ifelse(is.na(rosenberg_score), 
                                           mean(rosenberg_score, na.rm = T), 
                                           rosenberg_score)) %>% 
    dplyr::mutate(rotter_score = na.approx(rotter_score, na.rm=FALSE)) %>% 
    dplyr::mutate(rotter_score = ifelse(is.na(rotter_score), 
                                        mean(rotter_score, na.rm = T), 
                                        rotter_score)) %>% 
    dplyr::mutate(tenure = ifelse(is.na(tenure), 
                                  mean(tenure, na.rm = T), 
                                  tenure)) %>% 
    dplyr::mutate(hours_worked_week = ifelse(is.na(hours_worked_week), 
                                             mean(hours_worked_week, na.rm = T), 
                                             hours_worked_week)) %>% 
    dplyr::mutate(hourly_pay = ifelse(is.na(hourly_pay), 
                                      mean(hourly_pay, na.rm = T), 
                                      hourly_pay)) %>% 
    dplyr::select(-union, -sector) %>% 
    ungroup()
  
  data_binary <- data %>% dplyr::select(id, union)
  imputed_binary <- mice(data_binary, method = "logreg")
  data_binary <- complete(imputed_binary)
  
  data$sector <- as_factor(data$sector)
  data_catgorical <- data %>% dplyr::select(id, sector)
  imputed_categorical <- mice(data_catgorical, method = "polyreg")
  data_catgorical <- complete(imputed_categorical)
  
  mice_imputed <- cbind(data_binary, data_catgorical)
  mice_imputed$id <- NULL
  mice_imputed$id <- NULL
  
  imputed_data_final <- cbind(mean_imputed, mice_imputed)
  
  return(imputed_data_final)
}

imputed_full_data <- imputeData(job_satisfaction_data)
imputed_train_data <- imputeData(train_data)
imputed_test_data <- imputeData(test_data)

fullTimePublicPayInteractions <- function(data){

  data <- data %>% 
    dplyr::select(id, religion, ethnicity, gender, year, highest_grade, urban, age, net_family_income, rosenberg_score,
                  rotter_score, job_number, tenure, hours_worked_week, hourly_pay, industry, job_satisfaction, avg_age_per_job, 
                  sector, union) %>% 
    dplyr::mutate(full_time = as_factor(case_when(
      hours_worked_week >= 35 ~ 1,
      is.na(hours_worked_week) ~ NA_real_,
      TRUE ~ 0))) %>% 
    dplyr::mutate(
      pay_tenure = hourly_pay * tenure, 
      pay_age = hourly_pay * avg_age_per_job
    ) %>%
    dplyr::mutate(public = case_when(year <= 1993 & sector == 2 ~ 1, 
                                     year > 1993 & sector == 1 ~ 1,
                                     TRUE ~ 0))
  return(data)
}

interactions_full_data <- fullTimePublicPayInteractions(imputed_full_data)
interactions_train_data <- fullTimePublicPayInteractions(imputed_train_data)
interactions_test_data <- fullTimePublicPayInteractions(imputed_test_data)

# centering 
centerData <- function(data){
  centered_data <- data %>% 
    # group mean centering level 1 (job) variables
    group_by(id) %>% 
    mutate(
      hourly_pay_mean_per_person = mean(hourly_pay, na.rm = TRUE),
      hourly_pay = hourly_pay - hourly_pay_mean_per_person,
      avg_age_per_job_mean_per_person = mean(avg_age_per_job, na.rm = TRUE),
      avg_age_per_job = avg_age_per_job - avg_age_per_job_mean_per_person,
      tenure_mean_per_person = mean(tenure, na.rm = TRUE),
      tenure = tenure - tenure_mean_per_person,
      hours_mean_per_person = mean(hours_worked_week, na.rm = TRUE),
      hours_worked_week = hours_worked_week - hours_mean_per_person,
      pay_tenure_mean_per_person = mean(pay_tenure, na.rm = TRUE),
      pay_tenure = pay_tenure - pay_tenure_mean_per_person,
      pay_age_mean_per_person = mean(pay_age, na.rm = TRUE),
      pay_age = pay_age - pay_age_mean_per_person
    ) %>%
    ungroup() %>% 
    # grand mean centering level 2 (individual) variables
    mutate_at(vars(net_family_income, rotter_score, rosenberg_score),
              funs(. - mean(., na.rm = TRUE))) %>% 
    select(-hourly_pay_mean_per_person, 
           -avg_age_per_job_mean_per_person,
           -tenure_mean_per_person,
           -hours_mean_per_person,
           -pay_tenure_mean_per_person,
           -pay_age_mean_per_person)
  
  return(centered_data)
}

centered_full_data <- centerData(interactions_full_data)
centered_train_data <- centerData(interactions_train_data)
centered_test_data <- centerData(interactions_test_data)

finalTransformations <- function(data){
  data <- data %>% 
    dplyr::select(
      id,
      religion,
      ethnicity,
      gender,
      year,
      highest_grade,
      urban, 
      job_number,
      hours_worked_week,
      industry,
      job_satisfaction,
      avg_age_per_job,
      union,
      full_time,
      public,
      hourly_pay,
      tenure,
      hours_worked_week,
      rotter_score,
      rosenberg_score,
      net_family_income,
      pay_tenure,
      pay_age
    ) %>% 
    dplyr::mutate(
      highest_grade = factor(
        highest_grade,
        levels = c("primary_school", "high_school", "college", "advanced", "other")),
      public = factor(public)) %>% 
    drop_na()
  
  return(data)
}

final_full_data <- finalTransformations(centered_full_data)
final_train_data <- finalTransformations(centered_train_data)
final_test_data <- finalTransformations(centered_test_data)

#save(prepped_data, file = "data/full_prepped_data.RData")
write_csv(final_train_data, "data/train_data.csv")
write_csv(final_test_data, "data/test_data.csv")


