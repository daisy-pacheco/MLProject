library(tidyverse)
library(lme4)
library(lmerTest)
library(xgboost)
library(vip)
library(earth)
library(caret)
library(pdp) 

set.seed(123)

# load data
load("data/paper/final_train_data.RData")
load("data/paper/final_test_data.RData")

# turn off scientific notation
options(scipen = 999)

# LMER -----------------------------------------------------
lmer_data <- rbind(final_test_data, final_train_data)

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
                    personality_1 +
                    personality_2 +
                    personality_3 +
                    personality_4 +
                    personality_5 +
                    personality_6 +
                    personality_7 +
                    personality_8 +
                    personality_9 +
                    personality_10 +
                    (1 | id), 
                  data = lmer_data)

summary(lmerModel)

# XGBoost -----------------------------------------------------

# variables
X <- final_train_data %>% 
  select(-id, -employer_id, -job_number, -job_satisfaction) %>%
  mutate_at(vars(male, white, religious, urban, cohort),
            funs(as.numeric)) %>% 
  as.matrix()

Y <- final_train_data$job_satisfaction

# hyperparameters
hyper_grid <- expand.grid(
  eta = 0.01,
  max_depth = 3, 
  min_child_weight = 3,
  subsample = 0.5, 
  colsample_bytree = 0.5,
  gamma = c(0, 1, 10, 100, 1000),
  lambda = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  alpha = c(0, 1e-2, 0.1, 1, 100, 1000, 10000),
  error = 0,          
  trees = 0          
)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)
  m <- xgb.cv(
    data = X,
    label = Y,
    nrounds = 1000,
    objective = "reg:linear",
    early_stopping_rounds = 50, 
    nfold = 10,
    verbose = 0,
    params = list( 
      eta = hyper_grid$eta[i], 
      max_depth = hyper_grid$max_depth[i],
      min_child_weight = hyper_grid$min_child_weight[i],
      subsample = hyper_grid$subsample[i],
      colsample_bytree = hyper_grid$colsample_bytree[i],
      gamma = hyper_grid$gamma[i], 
      lambda = hyper_grid$lambda[i], 
      alpha = hyper_grid$alpha[i]
    ) 
  )
  hyper_grid$error[i] <- min(m$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- m$best_iteration
}

hyper_grid %>%
  filter(error > 0) %>%
  arrange(error) %>%
  glimpse()

params <- list(
  eta = 0.01,
  max_depth = 3,
  min_child_weight = 3,
  subsample = 0.5,
  colsample_bytree = 0.5,
  gamma = 1
)

xgbTrain <- xgboost(
  params = params,
  data = X,
  label = Y,
  nrounds = 1000,
  objective = "reg:linear",
  verbose = 0
)

# prediction 
testY <- final_test_data$job_satisfaction

test_matrix <- final_test_data %>% 
  select(-id, -employer_id, -job_number, -job_satisfaction) %>%
  mutate_at(vars(male, white, religious, urban, cohort),
            funs(as.numeric)) %>% 
  as.matrix()

preds <- predict(xgbTrain, as.matrix(test_matrix))

RMSE <- function(predicted, real){
  sqrt(mean((predicted - real)^2))
}

RMSE(preds, testY)
# 2.048

# feature importance
vip(xgbTrain)

# MARS ------------------------------------------------------------------
mars_train_data <- final_train_data %>%
  select(-id, -job_number, -employer_id)

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

cv_mars <- train(
  x = subset(mars_train_data, select = -job_satisfaction),
  y = mars_train_data$job_satisfaction,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

cv_mars$bestTune

# variable importance plots
p1 <- vip(cv_mars, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)

# partial dependence plots (to better understand the relationship between these features and job_satisfaction)
p1 <- partial(cv_mars, pred.var = "personality_1", grid.resolution = 10) %>% 
  autoplot()
p2 <- partial(cv_mars, pred.var = "white", grid.resolution = 10) %>% 
  autoplot()
p3 <- partial(cv_mars, pred.var = c("personality_1", "white"), 
              grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
              screen = list(z = -20, x = -60))

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

# final model
mars_model <- earth(
  job_satisfaction ~ .,  
  data = mars_train_data,
  degree = 1,
  nk = 26
)

summary(mars_model)

# prediction 
test_y_mars <- final_test_data$job_satisfaction

test_matrix_mars <- final_test_data %>% 
  select(-id, -employer_id, -job_number, -job_satisfaction)

preds_mars <- predict(mars_model, test_matrix_mars)

RMSE_mars <- function(predicted, real){
  sqrt(mean((predicted - real)^2))
}

RMSE_mars(preds_mars, test_y_mars)
# 2.066 

