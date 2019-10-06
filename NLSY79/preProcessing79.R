library(dplyr)

# switching variable names to words vs codes
# *** THIS PROCESS SHOULD BE IMPROVED ***
varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames$V3

# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample79 <- sample_n(trainData79, 20)


### FEATURE ENGINEERING ###

