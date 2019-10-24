library(tidyverse)
library(xray)

original_data <- readr::read_csv('data/fulldata.csv')
names_dictionary <- readr::read_csv('data/dictionary.csv')
cpi <- readr::read_csv('data/CPI.csv')

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
    net_family_income = ((net_family_income * cpi) + 1),
    tenure = tenure / 52
  ) %>% 
  dplyr::mutate(
    public = case_when(
      year <= 1993 & sector == 2 ~ 1, 
      year > 1993 & sector == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::mutate(
    full_time = as_factor(case_when(
      hours_worked_week >= 35 ~ 1,
      is.na(hours_worked_week) ~ NA_real_,
      TRUE ~ 0
    )
  )) %>%
  dplyr::mutate(avg_age_per_job = ((age - tenure) + age) / 2) %>% 
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
    urban_rural = as_factor(
      ifelse(urban_rural, "urban", "rural")
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
      ifelse(union == 1, "union", "no_union")
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
  dplyr::filter(!is.na(job_satisfaction)) %>%
  dplyr::select(-military_pay, -sample_id)

# train test split
set.seed(123)

uniqueIds <- job_satisfaction_data$id %>% unique
trainIds <- sample(1:length(uniqueIds), size = ceiling(0.7 * length(uniqueIds)))
train <- uniqueIds[trainIds]
test <- uniqueIds[-trainIds]

trainData <- job_satisfaction_data[job_satisfaction_data$id %in% train, ]
testData <- job_satisfaction_data[job_satisfaction_data$id %in% test, ]

# group mean centering level 1 (job) variables
trainData <- trainData %>%
  dplyr::group_by(id) %>% 
  dplyr:: mutate(hourly_pay_mean_per_person = mean(hourly_pay, na.rm = TRUE),
         hourly_pay_centered = hourly_pay - hourly_pay_mean_per_person,
         avg_age_per_job_mean_per_person = mean(avg_age_per_job, na.rm = TRUE),
         avg_age_per_job_centered = avg_age_per_job - avg_age_per_job_mean_per_person,
         tenure_mean_per_person = mean(tenure, na.rm = TRUE),
         tenure_centered = tenure - tenure_mean_per_person) %>%
  dplyr::select(-hourly_pay_mean_per_person, -avg_age_per_job_mean_per_person, 
                -tenure_mean_per_person) %>%
  ungroup

# grand mean centering level 2 (individual) variables
trainData <- trainData %>%
  dplyr:: mutate(rotter_score_centered = rotter_score - (mean(rotter_score, na.rm = TRUE)),
                 rosenberg_score_centered = rosenberg_score - (mean(rosenberg_score, na.rm = TRUE)),
                 net_family_income_centered = net_family_income - (mean(net_family_income, na.rm = TRUE)))


# experiments  
xray::anomalies(job_satisfaction_data)

lm_fit <- lm(
  job_satisfaction ~ cpi + industry + public + avg_age_per_job + year + tenure + ethnicity + religion + urban_rural + gender,
  data = job_satisfaction_data
)

summary(lm_fit)

lm_fit2 <- lm(
  job_satisfaction ~ industry + public + avg_age_per_job_centered + tenure_centered + ethnicity + religion + urban_rural + gender,
  data = trainData
)

summary(lm_fit2)

