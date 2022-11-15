library(lime)
library(shapper)
library(lme4)
library(dplyr)

num_data_train <- final_train_data %>%
  select(-id, -employer_id) %>% 
  mutate_all(funs(as.numeric))

num_data_train$id <- final_train_data$id

num_data_test <- final_test_data %>%
  select(-id, -employer_id) %>% 
  mutate_all(funs(as.numeric))

num_data_test$id <- final_test_data$id


# LMER -----------------------------------------------------

lmerModel <- lmer(job_satisfaction ~ 
                    year + 
                    hourly_pay  + 
                    avg_age_job_year + 
                    tenure +
                    hours_worked +
                    (1 | id), 
                  data = num_data)

summary(lmerModel)

# LM ----------------------------------------------------------
lmModel <- lm(job_satisfaction ~ 
                    year + 
                    hourly_pay  + 
                    avg_age_job_year + 
                    tenure +
                    hours_worked,
                  data = data.frame(num_data))

# SHAP ---------------------------------------------------------
p_function <- function(model, data) predict(model, newdata = data, type = "response")

ive <- individual_variable_effect(lmModel, data = num_data[,-6], predict_function = p_function,
                                  new_observation = num_data[1,], nsamples = 50)




# LIME ---------------------------------------------------------

features <- as.data.frame(final_train_data) %>% 
  select(-job_satisfaction, -id, -job_number, -employer_id)

# Train the explainer
explainer <- lime(features, lmerModel, bin_continuous = TRUE, quantile_bins = FALSE)

# Use the explainer on new observations
explanation <- explain(final_test_data[1:4,], explainer, n_labels = 1, n_features = 4)

tibble::glimpse(explanation)

plot_features(explanation, ncol = 3)





# 1) create a data frame with just the features
features <- as.data.frame(final_train_data) %>% 
  select(-job_satisfaction, -id, -job_number, -employer_id)

# 2) Create a vector with the actual responses
response <- as.data.frame(final_train_data) %>% 
  pull(job_satisfaction)

# Create explainer object
components_lime <- lime(
  x = features,
  model = lmerModel, 
  n_bins = 10
)

lime_explanation <- lime::explain(
  x = final_train_data[1:4,], 
  explainer = components_lime, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = 0.25,
  n_features = 10, 
  feature_select = "highest_weights"
)

# Error: The class of model must have a model_type method. See ?model_type to get an overview of models supported out of the box