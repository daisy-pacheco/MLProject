library(dplyr)

# switching variable names to words vs codes
varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames[match(names(trainData79), varNames[ ,'V1']),'V3']


# making a tiny sample for testing feature engineering 
set.seed(456) 

tinySample79 <- sample_n(trainData79, 20)


### FEATURE ENGINEERING ###

# finding most recent job for 1979 - 1993

stopweeksTrain <- trainData79 %>% 
  select(CASEID_R0000100, starts_with('EMPLOYERS_ALL_STOPWEEK'))





