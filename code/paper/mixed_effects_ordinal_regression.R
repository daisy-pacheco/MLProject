library(tidyverse)
library(GLMMadaptive)
library(car)
library(lattice)

set.seed(123)

# load
load("data/full_prepped_data.RData")

prepped_data$job_satisfaction = as.factor(prepped_data$job_satisfaction)

# filtering for full time
full_time_data <- prepped_data %>% 
  filter(hours_worked_week >= 35) %>% 
  select(-full_time)

# filtering for public sector
public_data <- full_time_data %>% 
  filter(public == 1) %>% 
  select(-public)

private_data <- full_time_data %>% 
  filter(public == 0) %>% 
  select(-public)

# Mixed-Effects Ordinal Regression --------------------------------------------------------
full_glmm <- mixed_model(job_satisfaction ~
                           religion + 
                           ethnicity + 
                           gender + 
                           highest_grade + 
                           urban_rural +
                           net_family_income_centered +
                           job_number + 
                           union + 
                           hourly_pay_centered + 
                           rotter_score_centered + 
                           rosenberg_score_centered +
                           public + 
                           avg_age_per_job_centered + 
                           tenure_centered + 
                         random = ~ 1 | id, 
                         data = full_time_data,
                         family = binomial())

summary(full_glmm)

full_glmm_interact <- mixed_model(job_satisfaction ~
                                    religion + 
                                    ethnicity + 
                                    gender + 
                                    highest_grade + 
                                    urban_rural +
                                    net_family_income_centered +
                                    job_number + 
                                    union + 
                                    hourly_pay_centered + 
                                    rotter_score_centered + 
                                    rosenberg_score_centered +
                                    public + 
                                    avg_age_per_job_centered + 
                                    tenure_centered + 
                                    public * avg_age_per_job_centered + 
                                    public * tenure_centered,
                                  random = ~ 1 | id, 
                                  data = full_time_data,
                                  family = binomial())

summary(full_glmm_interact)

public_glmm <- mixed_model(job_satisfaction ~
                             religion + 
                             ethnicity + 
                             gender + 
                             highest_grade + 
                             urban_rural + 
                             net_family_income_centered +
                             job_number + 
                             union + 
                             hourly_pay_centered + 
                             rotter_score_centered + 
                             rosenberg_score_centered +
                             avg_age_per_job_centered + 
                             tenure_centered,
                           random = ~ 1 | id, 
                           data = public_data,
                           family = binomial())

summary(public_glmm)

private_glmm <- mixed_model(job_satisfaction ~
                              religion + 
                              ethnicity + 
                              gender + 
                              highest_grade + 
                              urban_rural +
                              net_family_income_centered +
                              job_number + 
                              union + 
                              hourly_pay_centered + 
                              rotter_score_centered + 
                              rosenberg_score_centered +
                              avg_age_per_job_centered + 
                              tenure_centered,
                            random = ~ 1 | id, 
                            data = private_data,
                            family = binomial(),
                            max_coef_value = 15)

summary(private_glmm)


# Marginal Probability Plots -------------------------------------------------------
# TODO finish these -- they aren't working yet.

plot_data_m <- effectPlotData(public_glmm, public_data)

expit <- function (x) exp(x) / (1 + exp(x))
my_panel_bands <- function(x, y, upper, lower, fill, col, subscripts, ..., font, 
                           fontface) {
  upper <- upper[subscripts]
  lower <- lower[subscripts]
  panel.polygon(c(x, rev(x)), c(upper, rev(lower)), col = fill, border = FALSE, ...)
}

xyplot(expit(pred) ~ tenure_centered | avg_age_per_job_centered, data = plot_data_m)
