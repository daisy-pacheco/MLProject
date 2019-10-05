library(caret)

set.seed(456) 
index <- createDataPartition(new_data$R0000100, p = 0.7, 
                               list = FALSE)
trainData79 <- new_data[index, ]
testData79  <- new_data[-index, ]

save(trainData79, file = "trainData79.RData")
save(testData79, file = "testData79.RData")

