library(tidyverse)
set.seed(123)

# load
load("data/full_prepped_data.RData")

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35) %>% 
  select(-full_time)

# filtering for public sector
public_data <- full_time_data %>% 
  filter(public == 1) %>% 
  select(-public)


# mixor ------------------------------------------------------------------------
library(mixor)

## full time only
full_time_model <- mixor(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + union + public + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                           rotter_score_centered + rosenberg_score_centered,
                         id = id, 
                         link = "logit",
                         data = full_time_data)

summary(full_time_model)

## public sector full time only
public_model <- mixor(job_satisfaction ~
                        religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                        job_number + union + 
                        hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                        rotter_score_centered + rosenberg_score_centered,
                      id = id, 
                      link = "logit",
                      data = public_data)

summary(public_model)

# ordinal --------------------------------------------------------------------------
library(ordinal)

public_data$job_satisfaction <- as.factor(public_data$job_satisfaction)

public_model_clmm <- clmm(job_satisfaction ~
                           religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                           job_number + union + 
                           hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                           rotter_score_centered + rosenberg_score_centered + (1|id),
                         link = "logit",
                         data = public_data)


## balanced class example -----------------------------------------------------
library(caret)
full_time_data$job_satisfaction <- as.factor(full_time_data$job_satisfaction)
balanced_data <- downSample(x = full_time_data,
                             y = full_time_data$job_satisfaction)
balanced_data <- balanced_data[order(balanced_data$id),]

balanced_model <- mixor(job_satisfaction ~
                          religion + ethnicity + gender + highest_grade + urban_rural + net_family_income_centered +
                          job_number + union + public + 
                          hourly_pay_centered + avg_age_per_job_centered + tenure_centered + 
                          rotter_score_centered + rosenberg_score_centered,
                        id = id, 
                        link = "logit",
                        data = balanced_data)

summary(balanced_model)


# predicting
preds <- predict(full_time_model)
cm <- table(preds$class, full_time_data$job_satisfaction)

precrecall <- function(mytable, verbose=TRUE) {
  truePositives <- mytable[1, 1]
  falsePositives <- sum(mytable[1, ]) - truePositives
  falseNegatives <- sum(mytable[ ,1]) - truePositives
  precision <- truePositives / (truePositives + falsePositives)
  recall <- truePositives / (truePositives + falseNegatives)
  if (verbose) {
    print(mytable)
    cat("\n precision =", round(precision, 2), 
        "\n recall =", round(recall, 2), "\n")
  }
  invisible(c(precision, recall))
}

precrecall(cm) 
sum(diag(cm)) / sum(cm)


# kable table
library(kableExtra)

## grabbing model results and joining
full_time_info <- as.data.frame(full_time_model[4]) %>% 
  format(scientific = F) %>% 
  rownames_to_column() %>% 
  rename(Variable = rowname) %>% 
  mutate(Estimate = as.numeric(Model.Estimate),
         StdError = as.numeric(Model.Std..Error),
         Pvalue = as.numeric(Model.P...z..),
         Significant = case_when(Pvalue <= 0.1 & Pvalue > 0.05 ~ "*", 
                                 Pvalue <= 0.05 & Pvalue > 0.01 ~ "**", 
                                 Pvalue <= 0.01 ~ "***",
                                 TRUE ~ " ")) %>% 
  mutate_if(is.numeric, round, digits = 4) %>% 
  select(-Model.Estimate, -Model.Std..Error, -Model.z.value, -Model.P...z..)

public_info <- as.data.frame(public_model[4]) %>% 
  format(scientific = F) %>% 
  rownames_to_column() %>% 
  rename(Variable = rowname) %>% 
  mutate(EstimatePublic = as.numeric(Model.Estimate),
         StandardErrorPublic = as.numeric(Model.Std..Error),
         PvaluePublic = as.numeric(Model.P...z..),
         SignificantPublic = case_when(PvaluePublic <= 0.1 & PvaluePublic > 0.05 ~ "*", 
                                       PvaluePublic <= 0.05 & PvaluePublic > 0.01 ~ "**", 
                                       PvaluePublic <= 0.01 ~ "***",
                                 TRUE ~ " ")) %>% 
  mutate_if(is.numeric, round, digits = 4) %>% 
  select(-Model.Estimate, -Model.Std..Error, -Model.z.value, -Model.P...z..)

results_table <- left_join(full_time_info, public_info, by = "Variable")
results_table[] <- replace(as.matrix(results_table), is.na(results_table), "")
colnames(results_table) <- c("Variable", "Estimate", "StdError", "Pvalue", "Significant", 
                             "Estimate", "StdError", "Pvalue", "Significant")

## creating table
final_table <- results_table %>%
  kable("latex") %>% 
  kable_styling() %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(5, border_right = T) %>% 
  add_header_above(c(" " = 1, "All Full Time Employees" = 4, "Public Sector Only" = 4)) %>% 
  add_header_above(c(" ", "Mixed-Effects Longitudinal Model of Job Satisfaction" = 8)) %>% 
  footnote(general = "* = 0.1, ** = 0.05, *** = 0.01")

final_table
