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
library(rpart)
rpart1 <- rpart(Class ~., data=training, control = rpart.control(maxdepth = 3))
rpart1
library(rpart.plot)
library(partykit)

rpart.plot(rpart1)
plot(as.party(rpart1))
rpartFull <- rpart(Class ~., data=training)
plot(as.party(rpartFull))
rpartPred <- predict(rpartFull, testing, type= 'class')
confusionMatrix(rpartPred, testing$Class)



cvCtrl <- trainControl(method='repeatedcv', repeats = 3)
model <- train(Class ~., data = training, method = 'rpart', 
               tuneLength = 30,
               trControl = cvCtrl)
