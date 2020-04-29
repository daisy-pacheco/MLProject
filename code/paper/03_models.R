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
lmerModel <- lmer(job_satisfaction_scaled ~ 
                    year + 
                    hourly_pay_centered  + 
                    avg_age_job_year_centered + 
                    tenure_centered +
                    hours_worked_centered +
                    personality_1_centered +
                    personality_2_centered +
                    personality_3_centered +
                    personality_4_centered +
                    personality_5_centered +
                    personality_6_centered +
                    personality_7_centered +
                    personality_8_centered +
                    personality_9_centered +
                    personality_10_centered +
                    (1 | id), 
                  data = final_train_data)

summary(lmerModel)

# XGBoost -----------------------------------------------------

# variables
X <- final_train_data %>% select(-id, -employer_id, -job_number, -job_satisfaction_scaled) %>% as.matrix()
Y <- final_train_data$job_satisfaction_scaled

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
testY <- final_test_data$job_satisfaction_scaled
test_matrix <- final_test_data %>% select(-id, -employer_id, -job_number, -job_satisfaction_scaled) %>% as.matrix()

preds <- predict(xgbTrain, as.matrix(test_matrix))

RMSE <- function(predicted, real){
  sqrt(mean((predicted - real)^2))
}

RMSE(preds, testY)
# 2.076 with centered
# 2.078 with uncentered data

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
  x = subset(mars_train_data, select = -job_satisfaction_scaled),
  y = mars_train_data$job_satisfaction_scaled,
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
p1 <- partial(cv_mars, pred.var = "personality_1_centered", grid.resolution = 10) %>% 
  autoplot()
p2 <- partial(cv_mars, pred.var = "tenure_centered", grid.resolution = 10) %>% 
  autoplot()
p3 <- partial(cv_mars, pred.var = c("personality_1_centered", "tenure_centered"), 
              grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
              screen = list(z = -20, x = -60))

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

# final model
marsModel <- earth(
  job_satisfaction_scaled ~ .,  
  data = mars_train_data,
  degree = 3,
  nk = 23
)

summary(marsModel)
