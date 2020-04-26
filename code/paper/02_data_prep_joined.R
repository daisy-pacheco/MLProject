library(tidyverse)
library(zoo)
library(VIM)
library(mice)

set.seed(123)

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

# imputation 
## visualize missing data
joined_data %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  xlab("Observation") +
  theme(axis.text.y  = element_text(size = 4))

imputeData <- function(data){
  # linear interpolation for values measured at least twice
  linear_imp_data <- data %>% 
    group_by(id) %>% 
    mutate(tenure = na.approx(tenure, na.rm = FALSE),
           hours_worked = na.approx(hours_worked, na.rm = FALSE),
           hourly_pay = na.approx(hours_worked, na.rm = FALSE),
           avg_age_job_year = na.approx(avg_age_job_year, na.rm = FALSE))
  
  # mice for other yearly variables
  ini <- mice(linear_imp_data, maxit = 0)
  pred <- ini$pred
  pred[ ,"id"] <- -2 # specify grouping variable
  pred[ ,-1] <- 1 # fixed effects
  meth <- ini$meth
  meth[c(7:9)] <- "2l.lmer" # for multi-level imputation
  meth[c(11:20)] <- "" # ignore these
  impu <- mice(linear_imp_data, meth = meth, pred = pred, print = FALSE)
  mice_data <- complete(impu)
  
  # knn for personality
  data_one_row <- mice_data %>% 
    select(id, cohort, year, starts_with("personality")) %>% 
    mutate(keep = case_when(
      cohort == "1979" & year == 2008 ~ 1,
      cohort == "1979" & year == 2010 ~ 1,
      cohort == "1979" & year == 2012 ~ 1,
      cohort == "1979" & year == 2014 ~ 1,
      cohort == "1979" & year == 2016 ~ 1,
      cohort == "1997" & year == 2004 ~ 1,
      cohort == "1997" & year == 2006 ~ 1,
      cohort == "1997" & year == 2008 ~ 1,
      cohort == "1997" & year == 2010 ~ 1,
      cohort == "1997" & year == 2012 ~ 1,
      TRUE ~ 0
    )) %>% 
    filter(keep == 1) %>% 
    group_by(id) %>% 
    filter(year == min(year)) %>% 
    select(-keep)
  
  knn_data <- kNN(data_one_row[, -1], k = 3)
  knn_data <- knn_data[, 1:12]
  id <- data.frame(data_one_row[, 1])
  knn_done <- cbind(id, knn_data[, -2])
  
  # re-join data sets
  imputed_data <- mice_data %>% 
    select(-starts_with("personality")) %>% 
    left_join(knn_done, by = c("id", "cohort"))
  
  imputed_data <- imputed_data %>% 
    filter(hours_worked >= 35) %>% 
    drop_na()
  
  return(imputed_data)
  
}

imputed_train_data <- imputeData(train_data)
imputed_test_data <- imputeData(test_data)

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
    select(id, year, job_number, employer_id,
           job_satisfaction_scaled, hourly_pay_centered, 
           avg_age_job_year_centered, tenure_centered,
           hours_worked_centered, personality_1_centered,
           personality_2_centered, personality_3_centered,
           personality_4_centered, personality_5_centered, 
           personality_6_centered, personality_7_centered, 
           personality_8_centered, personality_9_centered, 
           personality_10_centered)
  
  return(centered_data)
}

final_train_data <- centerData(imputed_train_data)
final_test_data <- centerData(imputed_test_data)



