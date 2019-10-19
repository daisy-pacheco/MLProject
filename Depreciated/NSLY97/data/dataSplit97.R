library(caret)

set.seed(456) 
index <- createDataPartition(new_data$R0000100, p = 0.7, 
                             list = FALSE)
trainData97 <- new_data[index, ]
testData97  <- new_data[-index, ]

save(trainData97, file = "trainData97.RData")
save(testData97, file = "testData97.RData")