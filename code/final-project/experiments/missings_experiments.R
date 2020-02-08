### this works, but not with group_by(id):
library(imputeTS)

data_to_imputeTS <- train_data %>% 
  select(id, net_family_income, hours_worked_week, hourly_pay)

imputed_dataTS <- na_interpolation(data_to_imputeTS, option = "linear") # the problem with this is that it does not take into account the longitudinal aspect

# Error in is.finite(maxgap): default method not implemented for type 'list':
imputed_dataTS <- data_to_imputeTS %>% 
  group_by(id) %>% 
  na_interpolation(data_to_imputeTS, option = "linear")

### this works, but not with group_by(id):
library(Hmisc)

impute_arg <- aregImpute(~ highest_grade + net_family_income + hours_worked_week + hourly_pay +
                           union, data = trainData, n.impute = 2)

### this gave me negative values:
library(caret)
library(RANN)

data_to_impute <- trainData %>% 
  select(-id, -rosenberg_score, -rotter_score, -sector, -union)
as.data.frame(data_to_impute)

imputed_data_all = preProcess(data_to_impute, "knnImpute")

imputed_data_all_pred = predict(imputed_data_all, data_to_impute)

### mice

library(mice)

data_to_impute_numeric <- trainData %>% 
  select(id, rotter_score)

imputed_Data_numeric <- mice(data_to_impute_numeric, method = 'pmm', seed = 500)
data_to_impute_numeric <- complete(imputed_Data_numeric)
