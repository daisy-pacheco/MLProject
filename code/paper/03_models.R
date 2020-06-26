library(tidyverse)
library(lme4)
library(lmerTest)

set.seed(123)

# load data
load("data/paper/final_train_data.RData")
load("data/paper/final_test_data.RData")

# turn off scientific notation
options(scipen = 999)

# RMSE function
RMSE <- function(predicted, real){
  sqrt(mean((predicted - real)^2))
}

# LMER -----------------------------------------------------
lmerModel <- lmer(job_satisfaction ~ 
                    year + 
                    cohort +
                    hourly_pay  + 
                    avg_age_job_year + 
                    tenure +
                    hours_worked +
                    job_number + 
                    union +
                    white +
                    highest_grade +
                    family_income + 
                    male +
                    religious +
                    urban + 
                    pay_tenure +
                    pay_age +
                    (1 | id), 
                  data = final_train_data)

summary(lmerModel)

# prediction
testY <- final_test_data$job_satisfaction

preds <- predict(lmerModel, final_test_data, allow.new.levels = TRUE)

RMSE(preds, testY)
# 2.14

