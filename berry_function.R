#QUESTIONS
#How good is Rpart tree we constructed?
#Should we do test and training in rpart too?
#Sample sizes vary between less than 30 and greater than 30. How do we conduct tests? 
#ttest should only be used for sample size less than 30

#TODO:
#2. 'ranger' model can be used for faster processing, 
#but it's giving errors w.r.t RMSE, hence using rf as of now

#Input
#1. input data frame
#2. Treatment variable
#3. Target variable
#4. Covariates
#5. Covariates that are factors
#Output
#1. Tree
source("supplementary_functions.R")
library(ggplot2)
library(caret)
library(rattle)
library(rpart)

#This function uses berry-2s to find the heterogeneity in treatment effects and
#outputs subgroups ordered by treatment effect size  
berry2s <- function(data, control_group_placeholder, treatment_group_placeholder,
                    treatment_variable, target_variable, covariates, 
                    covariate_factors)
{
  #Converting convariates that are supposed to be factors into factors
  data[covariate_factors] <- lapply(data[covariate_factors], factor)
  #Dividing data into control group and treatment group
  control_data <- data[data[treatment_variable]== control_group_placeholder,]
  treatment_data <- data[data[treatment_variable]== treatment_group_placeholder,]
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
  

  #Create formula
  covariate_vec <- paste(covariates, collapse = '+')
  formula_vec <- paste('tau ~ ', covariate_vec, sep = '')
  formula_actual <- as.formula(formula_vec)
  #Creating tree with treatment effect as outcome variable to find heterogeneity
  or_tree <- rpart(formula_actual, data)
  tree_cp <- or_tree$cptable[,1][which.min(or_tree$cptable[,4])]
  tree <- prune(or_tree, tree_cp)
  predictions <- predict(tree, collage_data2)
  
  #Extracting useful info from rpart tree
  frame <- tree$frame
  
  #Finding the node values of nodes that are leaves
  leaves <- row.names(frame)[frame$var == '<leaf>']
  
  #Finding path of leaf nodes
  leaf_paths <- path.rpart(tree, leaves, print.it = FALSE)
  dedup_paths<- deduplication(leaf_paths)
  #Subsetting frame that contains leaf_nodes
  leaf_frame <- frame[frame$var == '<leaf>',]
  
  #Adding the standard deviation at which the yval of a particular node lies
  #on tau_2s distribution
  std<- vector()
  for (i in 1:length(leaf_frame$yval)){
    a <- leaf_frame$yval[i]
    b <- which(predictions==a)
    c <- vector()
    for(j in 1:length(b)){
      d<- collage_data[,'tau'][b[j]]
      c <- append(c,d)
    }
    std <- append(std, sd(c))
  }
  #Adding the standard deviation at which the yval of a particular node lies
  #on tau_2s distribution
  leaf_frame['std'] <- std
  
  #Adding t statistic to see if the treatment effect(y_val) is significantly
  #different from zero
  leaf_frame['t_statistic'] <- (leaf_frame$yval*sqrt(leaf_frame$n))/leaf_frame$std
  leaf_frame['p_value'] <- 2*pt(-abs(leaf_frame$t_statistic),df=leaf_frame$n - 1)
  
  #Adding paths to leaf_frame
  leaf_frame$path <- sapply(dedup_paths, paste0, collapse = ',')
  
  #Ordering frame by descending order of treatment effect
  leaf_frame <- leaf_frame[order(-leaf_frame$yval),] 
  
  #Subsetting leaf frame to only keep useful values
  leaf_frame <- leaf_frame[c("n","yval","std","p_value","path")] 
  
  
  return(leaf_frame)
}



