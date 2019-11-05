library(tidyverse)

full_data <- prepped_data

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35)

# filtering for public sector
public_data <- full_time_data %>% 
  filter(public == 1)


# models
library(mixor)
set.seed(123)

# full time only
full_time_model <- mixor(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + industry + union + public + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + hours_worked_week_centered +
                           rotter_score_centered + rosenberg_score_centered,
                         id = id, 
                         link = "logit",
                         data = full_time_data)

summary(full_time_model)
plot(full_time_model)

# public sector full time only
public_model <- mixor(job_satisfaction ~
                        religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                        job_number + industry + union + 
                        hourly_pay_centered + avg_age_per_job_centered + tenure_centered + hours_worked_week_centered +
                        rotter_score_centered + rosenberg_score_centered,
                      id = id, 
                      link = "logit",
                      data = public_data)

summary(public_model)
plot(public_model)


# balanced class example 
library(caret)
balanced_data <- downSample((x = full_time_data[ ,-job_satisfaction],
                             y = full_time_data$job_satisfaction))

balanced_model <- mixor(job_satisfaction ~
                          religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                          job_number + industry + union + full_time + public + 
                          hourly_pay_centered + avg_age_per_job_centered + tenure_centered + hours_worked_week_centered +
                          rotter_score_centered + rosenberg_score_centered,
                        id = id, 
                        link = "logit",
                        data = balanced_data)

summary(balanced_model)
plot(balanced_model)


# predicting
preds <- predict(model)
cm <- table(preds$class, subsample$job_satisfaction)

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