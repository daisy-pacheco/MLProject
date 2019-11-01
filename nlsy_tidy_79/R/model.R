library(tidyverse)
library(ordinalNet)

train_data <- centered_data
train_data$highest_grade <- relevel(train_data$highest_grade, "primary_school")

# dropping missings since both mixor and ordinalNet drop them anyway
## could consider dropping personality variables instead
train_data <- train_data[complete.cases(train_data), ]

### MIXOR ###
library(mixor)

# all jobs
mixor1 <- mixor(job_satisfaction ~
                  hourly_pay_centered + 
                  avg_age_per_job_centered +
                  tenure_centered +
                  job_number, 
                id = id, 
                link = "logit",
                data = train_data)

summary(mixor1)
plot(mixor1)

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


# full time only
full_time <- train_data %>% 
  filter(hours_worked_week >= 35)

mixor2 <- mixor(job_satisfaction ~
                  hourly_pay_centered + 
                  avg_age_per_job_centered +
                  tenure_centered +
                  job_number, 
                id = id, 
                link = "logit",
                data = full_time)

summary(mixor2)


### LME4 ###
library(lme4)

# all jobs
lmeModel1 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + (1 | id) + (1 | job_number), 
               data = train_data)

summary(lmeModel1)

lmeModel2 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + 
                 industry +
                 union +
                 full_time +
                 public +
                 gender +
                 religion +
                 ethnicity + 
                 highest_grade +
                 urban_rural +
                 net_family_income_centered +
                 rotter_score_centered + 
                 rosenberg_score_centered +
                 (1 | id) + (1 | job_number), 
               data = train_data)

summary(lmeModel2)

# full time only
full_time <- train_data %>% 
  filter(hours_worked_week >= 35)

lmeModel3 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + (1 | id) + (1 | job_number), 
               data = full_time)

summary(lmeModel3)

lmeModel4 <- lmer(job_satisfaction ~ hourly_pay_centered + 
                 avg_age_per_job_centered +
                 tenure_centered + 
                 industry +
                 union +
                 full_time +
                 public +
                 gender +
                 religion +
                 ethnicity + 
                 highest_grade +
                 urban_rural +
                 net_family_income_centered +
                 rotter_score_centered + 
                 rosenberg_score_centered +
                 (1 | id) + (1 | job_number), 
               data = train_data)

summary(lmeModel4)


### REGULARIZATION ###
y <- as.factor(df$job_satisfaction)
x <- df %>% 
  select(hourly_pay_centered, avg_age_per_job_centered, tenure_centered) %>% 
  as.matrix()

fit1 <- ordinalNet(x = x, y = y,   
                   family = "cumulative", 
                   link = "logit",
                   parallelTerms = TRUE, 
                   nonparallelTerms = FALSE)

summary(fit1)
coef(fit1)
