library(tidyverse)
library(h2o)
h2o.init()

data <- centered_data
data$highest_grade <- relevel(data$highest_grade, "primary_school")

data <- data[complete.cases(data), ]

set.seed(123)  
index <- createDataPartition(data$job_satisfaction, p = 0.7, 
                             list = FALSE)
trainData <- data[index, ]
testData  <- data[-index, ]

trainData <- trainData[sample(nrow(trainData), 100), ]
testData <-  testData[sample(nrow(testData), 40), ]

Y = "job_satisfaction"
X <- setdiff(names(trainData), Y)

train_h2o <- trainData %>% 
  as.h2o

test_h2o <- testData %>% 
  as.h2o

best_rf <- h2o.randomForest(
  x = X, y = Y, training_frame = train_h2o, ntrees = 1000, mtries = 20,
  max_depth = 30, min_rows = 1, sample_rate = 0.8, nfolds = 10,
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
  stopping_tolerance = 0
)

best_xgb <- h2o.xgboost(
  x = X, y = Y, training_frame = train_h2o, ntrees = 5000, learn_rate = 0.05,
  max_depth = 3, min_rows = 3, sample_rate = 0.8, categorical_encoding = "Enum",
  nfolds = 10, fold_assignment = "Modulo", 
  keep_cross_validation_predictions = TRUE, seed = 123, stopping_rounds = 50,
  stopping_metric = "RMSE", stopping_tolerance = 0
)

best_gbm <- h2o.gbm(
  x = X, y = Y, training_frame = train_h2o, ntrees = 5000, learn_rate = 0.01,
  max_depth = 7, min_rows = 5, sample_rate = 0.8, nfolds = 10,
  fold_assignment = "Modulo", keep_cross_validation_predictions = TRUE,
  seed = 123, stopping_rounds = 50, stopping_metric = "RMSE",
  stopping_tolerance = 0
)

ensemble_tree <- h2o.stackedEnsemble(
  x = X, y = Y, training_frame = train_h2o, model_id = "my_tree_ensemble",
  base_models = list(best_rf, best_gbm, best_xgb),
  metalearner_algorithm = "drf"
)

get_rmse <- function(model) {
  results <- h2o.performance(model, newdata = test_h2o)
  results@metrics$RMSE
}
list(best_rf, best_gbm, best_xgb) %>%
  purrr::map_dbl(get_rmse) 

# Stacked results
h2o.performance(ensemble_tree, newdata = test_h2o)@metrics$RMSE

h2o.varimp_plot(best_rf)
h2o.varimp_plot(best_gbm)
h2o.varimp_plot(best_xgb)


