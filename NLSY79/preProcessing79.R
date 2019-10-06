library(dplyr)
library(caret)

# switching variable names to words vs codes
# *** THIS PROCESS SHOULD BE IMPROVED ***
varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames$V2

# making a tiny sample for testing feature engineering 
set.seed(456) 

index <- createDataPartition(trainData79$CASEID, p = 0.99, 
                             list = FALSE)

tinySample79  <- trainData79[-index, ]

### FEATURE ENGINEERING ###

# new column: average tenure per person per year
tinySample79$avgTenure <- tinySample79 %>%
  mutate(avgTenure = mean())

