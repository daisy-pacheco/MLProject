library(visdat)
library(caret)
library(recipes)

# Visualization of missing data
#1
joined_data %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  xlab("Observation") +
  theme(axis.text.y  = element_text(size = 4))

#2
vis_miss(joined_data, cluster = TRUE)

# KNN

train_data_fix <- train_data %>% 
  dplyr::select(-id, -job_satisfaction) %>% 
  dplyr::mutate(cohort = as_factor(cohort))

blueprint_knn <- recipe(job_satisfaction_scaled ~ ., data = train_data_fix) %>%
  step_knnimpute(all_predictors())

prepare_knn <- prep(blueprint_knn, training=train_data_fix)

baked_train_knn <- bake(prepare_knn, new_data = train_data_fix)

# TREE-BASED

blueprint_tb <- recipe(job_satisfaction_scaled ~ ., data = train_data_fix) %>%
  step_bagimpute(all_predictors())

prepare_tb <- prep(blueprint_tb, training=train_data_fix)

baked_train_tb <- bake(prepare_tb, new_data = train_data_fix)

# Comparison of imputation for hours_worked
compare_imp <- cbind(train_data$id, train_data$job_number, train_data_fix$hours_worked, baked_train$hours_worked, baked_train_tb$hours_worked)
