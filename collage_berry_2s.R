#Reading treatment control data with covariates and target variable
collage_data <- read.csv('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')

#changing some variable to factors
col_names <- c("cell","satisfied","survey")
collage_data[col_names] <- lapply(collage_data[col_names], factor)

TreatmentVar <- 'cell'
PredictorsVar <- c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey")
OutcomeVar <-  "number_referrals"
colnames(collage_data)
RFControl <- trainControl(method = "cv",
                          number = 10,
                          allowParallel = TRUE,
                          verboseIter = FALSE)

control_data <- collage_data[collage_data[TreatmentVar]== '1',]
treatment_data <- collage_data[collage_data[TreatmentVar]== '4',]
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