library(tidyverse)
library(glmnet)
set.seed(123)

X <- prepped_data %>% 
  select(-id, -avg_age_per_job, -hours_worked_week, 
         -industry, -full_time, -hourly_pay_centered)
X <- model.matrix(job_satisfaction ~ ., X)[, -1]

Y <- prepped_data$job_satisfaction

#fit lasso regression
lasso <- glmnet(
  x = X,
  y = Y,
  alpha = 1)

#plot evolution of MSE vs lambda values
pred_reg = data.table(predict(lasso, X))

RMSE_reg = sqrt(apply(pred_reg[ ,(.SD - prepped_data$job_satisfaction)^2, .SD = 1:ncol(pred_reg)], 2, mean))

DF_plot = data.frame(lambda = lasso$lambda, rmse = RMSE_reg)

ggplot(DF_plot, aes(x = lambda, y = rmse)) +
  geom_line() +
  ggtitle("Evolution of test error vs lambda value") +
  scale_x_log10()

#apply CV lasso regression
lasso_cv <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1)

#plot results of CV
plot(lasso_cv, main = "Lasso penalty\n\n")

#minimum MSE = 0.5365813
min(lasso_cv$cvm)  

#lambda for this minimum MSE = 0.0003839942
lasso_cv$lambda.min

#largest lambda value within one SE of the MSE = 0.01093629 (This shows how much we can constrain the coefficients while still maximizing predictive accuracy)
lasso_cv$lambda.1se

#coefficientes at lambda.1se
library(coefplot)
extract.coef(lasso_cv, lambda = "lambda.1se")

#plot with names of variables
plot_coeff_evolution = function(regularization, type = 'Lasso')
{
  require(ggplot2)
  lambda = regularization$lambda
  coeff = as.matrix(regularization$beta)
  rowName = rownames(coeff)
  coeff = data.table(coeff)
  coeff[ ,name:=rowName]
  coeff = melt(coeff, id.vars = 'name')
  coeff[ ,variable:=rep(lambda, each = length(unique(name)))]
  ggplot(coeff, aes(x = variable, y = value, color = name)) +
    geom_line() +
    xlab('Value of lambda') +
    ylab('Value of coefficient') +
    scale_x_log10() + 
    geom_vline(xintercept = lasso_cv$lambda.1se, linetype = "longdash") + 
    geom_vline(xintercept = lasso_cv$lambda.min, linetype = "longdash") + 
    theme(axis.text=element_text(size=14), axis.title=element_text(size=14,face="bold"))
}

plot_coeff_evolution(lasso, "Lasso")


