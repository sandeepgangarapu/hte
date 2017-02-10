#Checking if these changes reflect in github desktop and in github web client

#TODO:
#Data cleaning
#DummyVars
#Feature reduction using variance
#Feature reduction using PCA
library(rattle)
library(caret)
library(pROC)
function(data,
         TreatmentVar,
         OutcomeVar,
         OutcomeVarIsFactor=FALSE,
         PredictorsVar,
         method='ranger',
         #traintestpartition = 0.8
         ) {
  #This commented code was used for test and train, but train function and custom train control in caret
  #will automaticaly select model by CV, hence removed
  #set.seed(1234)
  #splitIndex <- createDataPartition(data[,OutcomeVar],
                                    #p = traintestpartition,
                                    #list = FALSE,
                                    #times = 1)
  #trainDF <- data[ splitIndex,]
  #testDF  <- data[-splitIndex,]
  RFControl <- trainControl(method = "cv",
                           number = 10,
                           allowParallel = TRUE,
                           verboseIter = FALSE)
  control_data <- data[data[TreatmentVar]== 0,]
  treatment_data <- data[data[TreatmentVar]== 1,]
  RFModel_c <- train(control_data[,PredictorsVar],
                     control_data[,OutcomeVar],
                     method = "ranger",
                     tuneLength = 3, #improved accuracy
                     metric = "RMSE",
                     prox = FALSE,
                     trControl = RFControl)
  RFModel_t <- train(treatment_data[,PredictorsVar],
                     treatment_data[,OutcomeVar],
                     method = "ranger",
                     tuneLength = 3, #improved accuracy
                     metric = "RMSE",
                     prox = FALSE,
                     trControl = RFControl)
  
  # GLMnetControl <- trainControl(method = "cv",
  #                               number = 10,
  #                               summaryFunction = twoClassSummary,
  #                               classProbs = TRUE,
  #                               verboseIter = TRUE)
  # GLMnetModel <- train(trainDF[,PredictorsVar],
  #                      trainDF[,OutcomeVar],
  #                      method = "glmnet",
  #                      metric = "ROC",
  #                      trControl = GLMnetControl)
  # model_list <- list(item1 = RFModel,
  #                    item2 = GLMnetModel)
  # resamples <- resamples(model_list)
  # median_ROC_1 <- median(resamples$values[['item1~ROC']])
  # median_ROC_1 <- median(resamples$values[['item2~ROC']])
  # chosen_model <- ifelse(median_ROC_1>median_ROC_2, RFModel, GLMnetModel)
  predictions_c <- predict(object = RFModel_c,
                         data[,PredictorsVar],
                         type = 'prob')
  predictions_t <- predict(object = RFModel_t,
                         data[,PredictorsVar],
                         type = 'prob')
  tau_2s <- predictions_t - predictions_c
  data['tau'] <- tau_2s
  #USE CART (CARET Rpart method) to create a tree
  target_var <- 'tau'
  tree <- train(data[,PredictorsVar],
                data[,target_var],
                method = "rpart")
  #fancyRpartPlot(tree$finalModel)
  return(tree)
}

collage <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage.csv')
