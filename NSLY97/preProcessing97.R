library(dplyr)

varNames <- read.csv("varNames.csv", header = FALSE)

names(trainData97) <- varNames$V2

tinySample97 <- sample(trainData97, size = 100)

names(trainData97)
