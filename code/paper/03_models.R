library(tidyverse)
library(lme4)
library(lmerTest)
library(rpart)
library(rpart.plot)

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

# DECISION TREE ---------------------------------------------------
treeDataTrain <- final_train_data[setdiff(names(final_train_data), "id")]
treeDataTest <- final_test_data[setdiff(names(final_test_data), "id")]

treeModel <- rpart(
  formula = job_satisfaction ~ .,
  data    = treeDataTrain,
  method  = "anova", # for regression
  control = list(cp = 0.0015) # complexity parameter
)

# evaluation
treeModel

# visualization
rpart.plot(treeModel, cex = 0.6)

# prediction
preds <- predict(treeModel, treeDataTest)
testY <- treeDataTest$job_satisfaction
RMSE(preds, testY)
# 2.15

