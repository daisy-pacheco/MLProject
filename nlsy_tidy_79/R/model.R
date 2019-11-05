library(tidyverse)

train_data <- centered_data
train_data$highest_grade <- relevel(train_data$highest_grade, "primary_school")

# dropping missings since both mixor and ordinalNet drop them anyway
## could consider dropping personality variables instead
train_data <- train_data[complete.cases(train_data), ]

train_data <- train_data %>% 
  select(-net_family_income, -rosenberg_score, -rotter_score, 
         -tenure, -avg_age_per_job, -hourly_pay, -sector) %>% 
  mutate(public = factor(public),
         job_satisfaction = factor(job_satisfaction))

# tiny sample for tests
set.seed(123)
ids <- train_data$id %>% unique
sampleIds <- sample(1:length(ids), size = ceiling(0.1* length(ids)))
sample <- ids[sampleIds]
sample <- train_data[train_data$id %in% sample, ]

full_time_sample <- sample %>% 
  filter(hours_worked_week >= 35)


### MIXOR ###
library(mixor)

# all jobs
mixor1 <- mixor(job_satisfaction ~
                  hourly_pay_centered + 
                  avg_age_per_job_centered +
                  tenure_centered,
                id = id, 
                link = "logit",
                data = sample)

summary(mixor1)
plot(mixor1)

mixor2 <- mixor(job_satisfaction ~ 
                  hourly_pay_centered + 
                  avg_age_per_job_centered +
                  tenure_centered +
                  religion +
                  ethnicity +
                  gender +
                  year +
                  highest_grade +
                  urban_rural +
                  net_family_income_centered +
                  rosenberg_score_centered +
                  rotter_score_centered +
                  job_number +
                  full_time +
                  industry +
                  union +
                  public,
                id = id, 
                link = "logit",
                data = sample)

summary(mixor2)
plot(mixor2)












## predicting
preds <- predict(mixor1)
cm <- table(preds$class, subsample$job_satisfaction)

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

## precision and recall
precrecall(cm) 

## accuracy
sum(diag(cm)) / sum(cm)



