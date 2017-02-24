#-------------------------------------------------------------------------------------------------------------------
#TODO
#1. Make 'ranger' method work for fitting models on control and treatment data
#-------------------------------------------------------------------------------------------------------------------
#BIG WARNINGS
#1. RMSE is very low. Check why!
#-------------------------------------------------------------------------------------------------------------------
#Reading treatment control data with covariates and target variable
collage_data <- read.csv('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')
str(collage_data)

#changing some variable to factors
col_names <- c("cell","satisfied","survey")
collage_data[col_names] <- lapply(collage_data[col_names], factor)


TreatmentVar <- 'cell'
PredictorsVar <- c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey")
OutcomeVar <-  "number_referrals"

#Dividing control and treatments groups so as to fit seperate models
control_data <- collage_data[collage_data[TreatmentVar]== '1',]
treatment_data <- collage_data[collage_data[TreatmentVar]== '4',]

#Creating train control object using 10 fold cross validation
RFControl <- trainControl(method = "cv",
                          number = 10,
                          allowParallel = TRUE,
                          verboseIter = FALSE)

#Training a Random Forest model on control group data
RFModel_c <- train(number_referrals ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
                   control_data,
                   #'ranger' model can be used for faster processing,
                   #but it's giving errors w.r.t RMSE, hence using rf as of now
                   method = "rf",
                   metric = "RMSE",
                   #This argument checks n no. of combinations max for tree splitting
                   #As there are 6 predictor vars, using 3
                   tuneLength = 3, #improved accuracy
                   prox = FALSE,
                   trControl = RFControl)

#Training a Random Forest model on control group data
RFModel_t <- train(number_referrals ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
                   treatment_data,
                   method = "rf",
                   tuneLength = 3, #improved accuracy
                   metric = "RMSE",
                   prox = FALSE,
                   trControl = RFControl)

#Now we predict treatment and control outcomes for everyone using fitted models
predictions_t <- predict(object = RFModel_t,
                         collage_data)
predictions_c <- predict(object = RFModel_c,
                         collage_data)

tau_2s <- predictions_t - predictions_c
data['tau'] <- tau_2s
#USE CART (CARET Rpart method) to create a tree
target_var <- 'tau'
tree <- train(data[,PredictorsVar],
              data[,target_var],
              method = "rpart")

