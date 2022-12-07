library(tidyverse)
set.seed(123)

# load
load("data/full_prepped_data.RData")

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35) %>% 
  select(-full_time)

# filtering for public sector
public_data <- full_time_data %>% 
  filter(public == 1) %>% 
  select(-public)

private_data <- full_time_data %>% 
  filter(public == 0) %>% 
  select(-public)


# mixor ------------------------------------------------------------------------
library(mixor)

full_time_mixor <- mixor(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + union + public + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                           rotter_score_centered + rosenberg_score_centered,
                         id = id, 
                         link = "logit",
                         data = full_time_data)
summary(full_time_mixor)

public_mixor <- mixor(job_satisfaction ~
                        religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                        job_number + union + 
                        hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                        rotter_score_centered + rosenberg_score_centered,
                      id = id, 
                      link = "logit",
                      data = public_data)
summary(public_mixor)

private_mixor <- mixor(job_satisfaction ~
                        religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                        job_number + union + 
                        hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                        rotter_score_centered + rosenberg_score_centered,
                      id = id, 
                      link = "logit",
                      data = private_data)
summary(private_data)

# GLMMadaptive --------------------------------------------------------------------------
library(GLMMadaptive)

full_time_data$job_satisfaction = as.factor(full_time_data$job_satisfaction)
public_data$job_satisfaction = as.factor(public_data$job_satisfaction)
private_data$job_satisfaction = as.factor(private_data$job_satisfaction)

full_glmm <- mixed_model(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + union + public + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                           rotter_score_centered + rosenberg_score_centered,
                         random = ~ 1 | id, 
                         data = full_time_data,
                         family = binomial())
summary(full_glmm)

public_glmm <- mixed_model(job_satisfaction ~
                             religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                             job_number + union + 
                             hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                             rotter_score_centered + rosenberg_score_centered,
                           random = ~ 1 | id, 
                           data = public_data,
                           family = binomial())
summary(public_glmm)

private_glmm <- mixed_model(job_satisfaction ~
                    religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                    job_number + union + 
                    hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                    rotter_score_centered + rosenberg_score_centered,
                  random = ~ 1 | id, 
                  data = private_data,
                  family = binomial())
summary(private_glmm)

"
TODOs:

1. Add public * age and public * tenure to the models. The features have already been
created in the updated prepped data.

2. Create tables with N and goodness-of-fit measures, in Word format

3. Check linear model assumptions (VIF, residual plots)

4. Create marginal probability plots for public vs. private, and tenure vs. age
Details here: https://drizopoulos.github.io/GLMMadaptive/articles/Ordinal_Mixed_Models.html
"