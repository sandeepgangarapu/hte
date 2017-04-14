#-------------------------------------------------------------------------------------------------------------------
#TODO
#1. Make 'ranger' method work for fitting models on control and treatment data
#2. Prune RFModel_c and RFModel_t
#-------------------------------------------------------------------------------------------------------------------
#BIG WARNINGS
#1. RMSE is very low. Check why!
#-------------------------------------------------------------------------------------------------------------------
#
#install.packages('rattle')
#install.packages('ggplot2')
library(ggplot2)
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(rpart.utils)
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

#-------------------------------------------------------------------------------------------------------------------
#Pre processing of data
#-------------------------------------------------------------------------------------------------------------------

#Reading treatment control data with covariates and target variable
collage_data <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')
str(collage_data)

#changing some variable to factors
col_names <- c("cell","satisfied","survey")
collage_data[col_names] <- lapply(collage_data[col_names], factor)
#histograms of individual predictors
hist(collage_data$money_spend_all, labels = TRUE, breaks = 40)
hist(collage_data$lastday_purchase_all, labels = TRUE, breaks = 40)
barplot(prop.table(table(collage_data$satisfied)))
barplot(prop.table(table(collage_data$survey)))
hist(collage_data$NPS, labels = TRUE, breaks = 11)
hist(collage_data$number_referrals, labels = TRUE, breaks = 6)
hist(collage_data$num_purchase_all, labels = TRUE, breaks = 40)

#Making a copy
collage_data2 <- collage_data

TreatmentVar <- 'cell'
PredictorsVar <- c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey")
OutcomeVar <-  "number_referrals"

#Dividing control and treatments groups so as to fit seperate models
control_data <- collage_data[collage_data[TreatmentVar]== '1',]
treatment_data <- collage_data[collage_data[TreatmentVar]== '2',]

#-------------------------------------------------------------------------------------------------------------------
#End of Pre processing of data
#-------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------
#Model Creation
#-------------------------------------------------------------------------------------------------------------------

#Creating train control object using 10 fold cross validation
RFControl <- trainControl(method = "cv",
                          number = 10,
                          allowParallel = TRUE,
                          verboseIter = FALSE)

#TODO: 2 : Prune these trees
#Training a Random Forest model on control group data
RFModel_c <- train(number_referrals ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
                   control_data,
                   #'ranger' model can be used for faster processing,
                   #but it's giving errors w.r.t RMSE, hence using rf as of now
                   method = "rf",
                   metric = "RMSE",
                   #This argument checks n no. of combinations max for tree splitting
                   #As there are 6 predictor vars, using 3
                   tuneLength = 3,
                   prox = FALSE,
                   trControl = RFControl)

#Training a Random Forest model on treatment group data
RFModel_t <- train(number_referrals ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
                   treatment_data,
                   method = "rf",
                   metric = "RMSE",
                   tuneLength = 3,
                   prox = FALSE,
                   trControl = RFControl)

#Now we predict treatment and control outcomes for everyone using fitted models
predictions_t <- predict(object = RFModel_t,
                         collage_data)
predictions_c <- predict(object = RFModel_c,
                         collage_data)

#Calculating treatment effect for each entry
tau_2s <- predictions_t - predictions_c
#Adding the treatment effect back to the full data
collage_data2['tau'] <- tau_2s

#-------------------------------------------------------------------------------------------------------------------
#End of Model Creation
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
#Model Analysis
#-------------------------------------------------------------------------------------------------------------------

#Histogram of treatment effects of all individual observations
hist(tau_2s, labels = TRUE, breaks = 40)
#Adding a Normal curve on top of histogram 
curve(dnorm(x, mean=mean(tau_2s), sd=sd(tau_2s)), add=TRUE, col='darkblue', lwd=2)
#Mean and standard deviation of treatment effects
mean_tau2s=mean(tau_2s)
sd_tau2s=sd(tau_2s)

#-------------------------------------------------------------------------------------------------------------------
#End of Model Analysis
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
#Treatment effect generation
#-------------------------------------------------------------------------------------------------------------------

#Creating tree with treatment effect as outcome variable to find heterogeneity
or_tree <- rpart(tau ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
               collage_data2)
#Tree visualization using fancy Rpart plot
fancyRpartPlot(or_tree)
summary(or_tree)

#-------------------------------------------------------------------------------------------------------------------
#End of Treatment effect generation
#-------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------
#TREE PRUNING - This may not be required as it is giving vely les leaves :(
#------------------------------------------------------------------------------------
#As a rule of thumb, it's best to prune a decision tree using the cp of smallest tree
#that is within one standard deviation of the tree with the smallest xerror.
#In this example, the best xerror is 0.776 with standard deviation 0.108.
#So, we want the smallest tree with xerror less than 0.884.
#This is the tree with cp = 0.0158, so we'll want to prune our tree with a 
#cp slightly greater than than 0.0158.
tree_cp <- or_tree$cptable[,1][which.min(or_tree$cptable[,4])]
tree <- prune(or_tree, tree_cp)
fancyRpartPlot(tree)
predictions <- predict(tree, collage_data2)
leaf_frame$yval

#------------------------------------------------------------------------------------
#END OF TREE PRUNING
#------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
#Treatment effect Analysis
#-------------------------------------------------------------------------------------------------------------------
#*****ADDITIONAL CODE
rpart.lists(tree2)
rpart.rules(tree2)
list.rules.rpart(tree2)
model.frame(tree2)
unlist(paths[1], use.names = FALSE)[-1]
#*****ADDITIONAL CODE

#Extracting usefyl info from rpart tree
frame <- tree$frame

#Finding the node values of nodes that are leaves
leaves <- row.names(frame)[frame$var == '<leaf>']

#Finding path of leaf nodes
leaf_paths <- path.rpart(tree, leaves, print.it = FALSE)
dedup_paths<- deduplication(leaf_paths)
#Subsetting frame that contains leaf_nodes
leaf_frame <- frame[frame$var == '<leaf>',]
std<- vector()
for (i in 1:length(leaf_frame$yval)){
  a <- leaf_frame$yval[i]
  b <- which(predictions==a)
  c <- vector()
  for(j in 1:length(b)){
    d<- collage_data2[,'tau'][b[j]]
    c <- append(c,d)
  }
  std <- append(std, sd(c))
}
#Adding the standard deviation at which the yval of a particular node lies
#on tau_2s distribution
leaf_frame['sd'] <- std

#Adding t statistic to see if the treatment effect(y_val) is significantly
#different from zero
leaf_frame['t_test'] <- (leaf_frame$yval*sqrt(leaf_frame$n))/sd_tau2s

#Adding paths to leaf_frame
leaf_frame$path <- sapply(dedup_paths, paste0, collapse = ',')

#Ordering frame by descending order of treatment effect
leaf_frame <- leaf_frame[order(-leaf_frame$yval),] 

#Subsetting leaf frame to only keep useful values
leaf_frame <- leaf_frame[c("n","yval","sd","t_test","dev","complexity","path")] 

#-------------------------------------------------------------------------------------------------------------------
#End of Treatment effect Analysis
#-------------------------------------------------------------------------------------------------------------------


