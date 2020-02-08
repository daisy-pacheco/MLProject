library(tidyverse)
library(h2o)

set.seed(34566)

load('data/final_train_data.RData')


job_satisfaction_type <- "binary"

if (job_satisfaction_type == "binary") {
  data_to_model <- centered_data[!stringr::str_detect(colnames(centered_data), "centered")] %>% 
    dplyr::select(-id, -job_number) %>% 
    dplyr::mutate(public = as.factor(public)) %>% 
    dplyr::mutate(job_satisfaction = ifelse(job_satisfaction < 4, 0, 1))
} else {
  data_to_model <- centered_data[!stringr::str_detect(colnames(centered_data), "centered")] %>% 
    dplyr::select(-id, -job_number) %>% 
    dplyr::mutate(public = as.factor(public))
}

h2o::h2o.init()
data_to_model_h2o <- h2o::as.h2o(data_to_model)
data_to_model_h2o[ , y] <-  h2o::as.factor(data_to_model_h2o[ , y])
split_data <- h2o.splitFrame(data_to_model_h2o, ratios = 0.75)
train_data <- split_data[[1]]
validation_data <- split_data[[2]]

y <- 'job_satisfaction'
x <- setdiff(names(data_to_model_h2o), c(y))

gbm_params <- list(
  learn_rate = seq(0.01, 0.9, 0.01),
  max_depth = seq(2, 30, 1),
  sample_rate = seq(0.1, 1.0, 0.1),
  col_sample_rate = seq(0.1, 1.0, 0.1)
)

search_criteria <- list(strategy = "RandomDiscrete", max_models = 5, seed = 123984)

gbm_fit <- h2o.grid(
  "gbm",
  x = x, 
  y = y,
  grid_id = "gbm_fit",
  training_frame = train_data,
  validation_frame = validation_data,
  ntrees = 100,
  seed = 123,
  hyper_params = gbm_params,
  search_criteria = search_criteria
)

gbm_fit_perf <- h2o.getGrid(
  grid_id = "gbm_fit",
  sort_by = ifelse(job_satisfaction_type == "binary", "auc", "rmse"),
  decreasing = TRUE
)

best_gbm <- h2o.getModel(gbm_fit_perf@model_ids[[1]])

best_gbm_perf_train <- h2o::h2o.performance(
  model = best_gbm,
  newdata = train_data
)

best_gbm_perf_validation <- h2o::h2o.performance(
  model = best_gbm,
  newdata = validation_data
)

h2o.varimp(best_gbm)
h2o.varimp_plot(best_gbm)



train_lime <- as.data.frame(train_data)
explainer_lime <- lime::lime(train_lime, model = best_gbm)

prediction <- h2o.predict(best_gbm, validation_data)

prediction <- cbind(
  as.data.frame(validation_data) %>% 
    dplyr::select(y), 
  as_tibble(prediction)
) %>% 
  arrange(desc(p1))

validation_lime <- as.data.frame(validation_data)

validation_lime_js1 <-  validation_lime %>% 
  dplyr::filter(job_satisfaction == 1)
validation_lime_js1 <- validation_lime_js1[sample(nrow(validation_lime_js1), 3), ]

validation_lime_js0 <-  validation_lime %>% 
  dplyr::filter(job_satisfaction == 0)
validation_lime_js0 <- validation_lime_js0[sample(nrow(validation_lime_js0), 3), ]

explanation_lime_js1 <- lime::explain(validation_lime_js1, explainer_lime, n_labels = 2, n_features = 5)
explanation_lime_js0 <- lime::explain(validation_lime_js0, explainer_lime, n_labels = 2, n_features = 5)

lime::plot_features(explanation_lime_js1)
lime::plot_features(explanation_lime_js0)

h2o::h2o.shutdown(FALSE)
