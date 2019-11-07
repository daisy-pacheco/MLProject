library(tidyverse)
set.seed(123)

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35) %>% 
  select(-full_time)

# filtering for public sector
public_data <- full_time_data %>% 
  filter(public == 1) %>% 
  select(-public)


# mixor
library(mixor)

## full time only
full_time_model <- mixor(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + union + public + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                           rotter_score_centered + rosenberg_score_centered,
                         id = id, 
                         link = "logit",
                         data = full_time_data)

summary(full_time_model)

## public sector full time only
public_model <- mixor(job_satisfaction ~
                        religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                        job_number + union + 
                        hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                        rotter_score_centered + rosenberg_score_centered,
                      id = id, 
                      link = "logit",
                      data = public_data)

summary(public_model)

## balanced class example 
library(caret)
full_time_data$job_satisfaction <- as.factor(full_time_data$job_satisfaction)
balanced_data <- downSample(x = full_time_data,
                             y = full_time_data$job_satisfaction)
balanced_data <- balanced_data[order(balanced_data$id),]

balanced_model <- mixor(job_satisfaction ~
                          religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                          job_number + union + public + 
                          hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                          rotter_score_centered + rosenberg_score_centered,
                        id = id, 
                        link = "logit",
                        data = balanced_data)

summary(balanced_model)


# predicting
preds <- predict(full_time_model)
cm <- table(preds$class, full_time_data$job_satisfaction)

precrecall <- function(mytable, verbose=TRUE) {
  truePositives <- mytable[1, 1]
  falsePositives <- sum(mytable[1, ]) - truePositives
  falseNegatives <- sum(mytable[ ,1]) - truePositives
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / (truePositives + falseNegatives)
  if (verbose) {
    print(mytable)
    cat("\n precision =", round(precision, 2), 
        "\n recall =", round(recall, 2), "\n")
  }
  invisible(c(precision, recall))
}

precrecall(cm) 
sum(diag(cm)) / sum(cm)


# kable table
library(kableExtra)

public_info <- public_model[4]
full_time_info <- full_time_model[4]


public_info %>% kable() %>% kable_styling()


