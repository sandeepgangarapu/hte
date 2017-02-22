library(caret)
data("segmentationData")
segmentationData$Cell<-NULL
training <- subset(segmentationData, Case="Train")
testing <- subset(segmentationData, Case="Test")
training$Case <- NULL
testing$Case <- NULL
str(training[,1:6])
trainX <- training[,names(training)!='Class']
preProcValues <- preProcess(trainX, method = c('center','scale'))
scaledTrain <- predict(preProcValues, trainX)
summary(scaledTrain)
