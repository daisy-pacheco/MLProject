library(tidyverse)
library(lme4)
library(DMwR)
library(glmmLasso)

train_data <- centered_data
train_data$highest_grade <- relevel(train_data$highest_grade, "primary_school")


# all jobs
model1 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + (1 | id) + (1 | job_number), 
               data = train_data)

summary(model1)

model2 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                    avg_age_per_job_centered +
                    tenure_centered + 
                    industry +
                    union +
                    full_time +
                    public +
                    (1 | id) + (1 | job_number), 
                  data = train_data)

summary(model2)

model3 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + 
                 industry +
                 union +
                 full_time +
                 public +
                 gender +
                 religion +
                 ethnicity + 
                 highest_grade +
                 urban_rural +
                 net_family_income_centered +
                 rotter_score_centered + 
                 rosenberg_score_centered +
                 (1 | id) + (1 | job_number), 
               data = train_data)

summary(model3)

# full time only
full_time <- train_data %>% 
  filter(hours_worked_week >= 35)

model4 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + (1 | id) + (1 | job_number), 
               data = full_time)

summary(model4)

model5 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + 
                 industry +
                 union +
                 public +
                 (1 | id) + (1 | job_number), 
               data = full_time)

summary(model5)


model6 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + 
                 industry +
                 union +
                 full_time +
                 public +
                 gender +
                 religion +
                 ethnicity + 
                 highest_grade +
                 urban_rural +
                 net_family_income_centered +
                 rotter_score_centered + 
                 rosenberg_score_centered +
                 (1 | id) + (1 | job_number), 
               data = train_data)

summary(model6)

# regularization
df <- train_data[complete.cases(train_data),]

model_reg <- glmmLasso(fix = job_satisfaction ~ hourly_pay_centered + 
                         avg_age_per_job_centered +
                         tenure_centered, 
                       rnd = list(job_satisfaction = ~ 1 + id), 
                       lambda = 10,
                       data = df)


