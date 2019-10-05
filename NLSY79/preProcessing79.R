library(dplyr)

varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData79) <- varNames$V2



