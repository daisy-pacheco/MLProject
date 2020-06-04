library(lime)
library(shapper)
library(randomForest)

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