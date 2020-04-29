# Modeling packages
library(earth)     # for fitting MARS models
library(caret)     # for automating the tuning process

# Model interpretability packages
library(vip)       # for variable importance
library(pdp)       # for variable relationships

# The MARS fitting procedure is is a two stage process. The forward stage is the same idea as forward stepwise regression. This could result in a large number of terms and overfitting so the backwards stage tries to mitigate this be dropping some terms.

# Fit a basic MARS model
final_train_data_1 <- final_train_data %>%
  select(-id, -job_number, -employer_id)

mars1 <- earth(
  job_satisfaction_scaled ~ .,  
  data = final_train_data_1 
)

# Print model summary
print(mars1)
# --> Results:
# Selected 20 of 25 terms, and 9 of 15 predictors
# Termination condition: Reached nk 31 (tells us that the forward pass stopped when it reached 31 terms)

summary(mars1)

plot(mars1, which = 1)
# --> Results:
# The vertical dashed lined at 20 tells us the optimal number of terms retained where marginal increases in GCV R2 are less than 0.001

# Fit a MARS model assessing potential interactions between differente hinge functions
mars2 <- earth(
  job_satisfaction_scaled ~ .,  
  data = final_train_data_1,
  degree = 2 # to assess potential interactions
)

summary(mars2)

# TUNING: There are two important tuning parameters associated with our MARS model: the maximum degree of interactions and the number of terms retained in the final model.

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)

# Cross-validated model
set.seed(123)  # for reproducibility
cv_mars <- train(
  x = subset(final_train_data_1, select = -job_satisfaction_scaled),
  y = final_train_data_1$job_satisfaction_scaled,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# View results
cv_mars$bestTune
# --> Results:
# The model that provides the optimal combination includes first degree interaction effects and retains 23 terms

cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)

ggplot(cv_mars)

cv_mars$resample

# FEATURE INTERPRETATION

# variable importance plots
p1 <- vip(cv_mars, num_features = 40, geom = "point", value = "gcv") + ggtitle("GCV")
p2 <- vip(cv_mars, num_features = 40, geom = "point", value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)
# --> Results:
# 6 features have an importance value of zero, which means they were not included in the final model

# Construct partial dependence plots, to better understand the relationship between these features and job_satisfaction
p1 <- partial(cv_mars, pred.var = "personality_1_centered", grid.resolution = 10) %>% 
  autoplot()
p2 <- partial(cv_mars, pred.var = "tenure_centered", grid.resolution = 10) %>% 
  autoplot()
p3 <- partial(cv_mars, pred.var = c("personality_1_centered", "tenure_centered"), 
              grid.resolution = 10) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, colorkey = TRUE, 
              screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

# Final model with no interaction and maximum number of model terms to include equal to 23

mars_final <- earth(
  job_satisfaction_scaled ~ .,  
  data = final_train_data_1,
  degree = 1,
  nk = 23
)

print(mars_final)

# Another function for visualization
plotmo(mars_final)
