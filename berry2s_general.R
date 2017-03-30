#TODO:
#1. Generalize dedup function
#2. 'ranger' model can be used for faster processing, 
#but it's giving errors w.r.t RMSE, hence using rf as of now

#Input
#1. input data frame
#2. Treatment variable
#3. Target variable
#4. Covariates
#5. Covariates that are factors
#Output
#1. Treatment effect list
library(ggplot2)
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(rpart.utils)
# This function deduplicates the paths to leaf nodes
#TODO:1
deduplication <- function(paths)
{
  new_paths <- list()
  for (i in 1:length(paths)){
    a <- strsplit(paths[[i]], ',')
    lpag<- NULL; lpal<- NULL; msag<- NULL; msal<- NULL; npsg<- NULL; npsl<- NULL; npag<- NULL; npal<- NULL; sat<- NULL; sur<- NULL;
    for (j in 1:length(a)){
      #print(a[[j]])
      if(grepl('lastday_purchase_all>', a[[j]])) {lpag = a[[j]]} #else {lpag = NULL}
      if(grepl('lastday_purchase_all<', a[[j]])) {lpal = a[[j]]} #else {lpal = NULL}
      if(grepl('money_spend_all>', a[[j]])) {msag = a[[j]]} #else {msag = NULL}
      if(grepl('money_spend_all<', a[[j]])) {msal = a[[j]]} #else {msal = NULL}
      if(grepl('NPS>', a[[j]])) {npsg = a[[j]]} #else {npsg = NULL}
      if(grepl('NPS<', a[[j]])) {npsl = a[[j]]} #else {npsl = NULL}
      if(grepl('num_purchase_all>', a[[j]])) {npag = a[[j]]} #else {npag = NULL}
      if(grepl('num_purchase_all<', a[[j]])) {npal = a[[j]]} #else {npal = NULL}
      if(grepl('satisfied', a[[j]])) {sat = a[[j]]} #else {sat = NULL}
      if(grepl('survey', a[[j]])) {sur = a[[j]]} #else {sur = NULL}
    }
    new_paths[[i]] <- c(lpag, lpal, msag, msal, npsg, npsl, npag, npal, sat, sur)
  }
  return(new_paths)
}

#This function uses berry-2s to find the heterogeneity in treatment effects and
#outputs subgroups ordered by treatment effect size  
berry2s <- function(data, treatment_variable, target_variable, covariates, covariate_factors)
{
  #Converting convariates that are supposed to be factors into factords
  data[covariate_factors] <- lapply(data[covariate_factors], factor)
  #Dividing data into control group and treatment group
  control_data <- data[data[treatment_variable]== '1',]
  treatment_data <- data[data[treatment_variable]== '4',]
  #Creating train control object using 10 fold cross validation
  RFControl <- trainControl(method = "cv",
                            number = 10,
                            allowParallel = TRUE,
                            verboseIter = FALSE)
  
  #Training a Random Forest model on control group data
  RFModel_c <- train(x = control_data[,covariates],
                     y = control_data[,target_variable],
                     #TODO:2
                     method = "rf",
                     metric = "RMSE",
                     #This argument checks n no. of combinations max for tree splitting
                     #As there are 6 predictor vars, using 3
                     tuneLength = 3,
                     prox = FALSE,
                     trControl = RFControl)
  
  #Training a Random Forest model on treatment group data
  RFModel_t <- train(x = treatment_data[,covariates],
                     y = treatment_data[,target_variable],
                     #TODO:2
                     method = "rf",
                     metric = "RMSE",
                     #This argument checks n no. of combinations max for tree splitting
                     #As there are 6 predictor vars, using 3
                     tuneLength = 3,
                     prox = FALSE,
                     trControl = RFControl)
  
  #Now we predict treatment and control outcomes for everyone using fitted models
  predictions_t <- predict(object = RFModel_t,
                           data)
  predictions_c <- predict(object = RFModel_c,
                           data)
  
  #Calculating treatment effect for each entry
  tau_2s <- predictions_t - predictions_c
  #Adding the treatment effect back to the full data
  data['tau'] <- tau_2s
  
  #Mean and standard deviation of treatment effects
  mean_tau2s=mean(tau_2s)
  sd_tau2s=sd(tau_2s)
  
  #Creating tree with treatment effect as outcome variable to find heterogeneity
  tree <- rpart(x = data[,covariates],
                 y = data[,'tau'])
  
  #Extracting useful info from rpart tree
  frame <- tree$frame
  
  #Finding the node values of nodes that are leaves
  leaves <- row.names(frame)[frame$var == '<leaf>']
  
  #Finding path of leaf nodes
  leaf_paths <- path.rpart(tree2, leaves, print.it = FALSE)
  dedup_paths<- deduplication(leaf_paths)
  #Subsetting frame that contains leaf_nodes
  leaf_frame <- frame[frame$var == '<leaf>',]
  
  #Adding the standard deviation at which the yval of a particular node lies
  #on tau_2s distribution
  leaf_frame['sd'] <- (leaf_frame$yval-mean_tau2s)/sd_tau2s
  
  #Adding t statistic to see if the treatment effect(y_val) is significantly
  #different from zero
  leaf_frame['t_test'] <- (leaf_frame$yval*sqrt(leaf_frame$n))/sd_tau2s
  
  #Adding paths to leaf_frame
  leaf_frame$path <- sapply(dedup_paths, paste0, collapse = ',')
  
  #Ordering frame by descending order of treatment effect
  leaf_frame <- leaf_frame[order(-leaf_frame$yval),] 
  
  #Subsetting leaf frame to only keep useful values
  leaf_frame <- leaf_frame[c("n","yval","sd","t_test","dev","complexity","path")] 
  
  
  return(leaf_frame)
}


a <- berry2s(collage_data,
             treatment_variable = "cell",
             target_variable = "number_referrals",
             covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
             covariate_factors = c("cell","satisfied","survey"))

