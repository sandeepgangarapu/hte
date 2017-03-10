#-------------------------------------------------------------------------------------------------------------------
#TODO
#1. Make 'ranger' method work for fitting models on control and treatment data
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
#-------------------------------------------------------------------------------------------------------------------
#Reading treatment control data with covariates and target variable
collage_data <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')
str(collage_data)

#changing some variable to factors
col_names <- c("cell","satisfied","survey")
collage_data[col_names] <- lapply(collage_data[col_names], factor)
#Making a copy
collage_data2 <- collage_data

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
hist(tau_2s, labels = TRUE, breaks = 40)
curve(dnorm(x, mean=mean(tau_2s), sd=sd(tau_2s)), add=TRUE, col='darkblue', lwd=2)
mean=mean(tau_2s)
sd=sd(tau_2s)
#Adding the treatment effect back to the full data
collage_data2['tau'] <- tau_2s

#Creating tree with treatment effect as outcome variable to find heterogeneity
tree2 <- rpart(tau ~ satisfied+NPS+lastday_purchase_all+num_purchase_all+money_spend_all+survey,
               collage_data2)
rpart.plot(tree2)
a <- asRules(tree2)
tau1 = 0.00105637918831131
tau2 = 0.00864863016300949
tau3 = 0.00487099700048721
tau4 = 0.0153623655658251
tau5 = 0.0224704192251592
tau6 = 0.00894090905181208
tau7 = 0.0714950277312821
tau8 = 0.0151063943308175
n1 = 10279
n2 = 3612
n3 = 2414
n4 = 723
n5 = 474
n6 = 330
n7 = 238
n8 = 117
s1 = (tau1 - mean)/sd
h1 = (tau1*sqrt(n1))/sd
s2 = (tau2 - mean)/sd
h2 = (tau2*sqrt(n2))/sd
s3 = (tau3 - mean)/sd
h3 = (tau3*sqrt(n3))/sd
s4 = (tau4 - mean)/sd
h4 = (tau4*sqrt(n4))/sd
s5 = (tau5 - mean)/sd
h5 = (tau5*sqrt(n5))/sd
s6 = (tau6 - mean)/sd
h6 = (tau6*sqrt(n6))/sd
s7 = (tau7 - mean)/sd
h7 = (tau7*sqrt(n7))/sd
s8 = (tau8 - mean)/sd
h8 = (tau8*sqrt(n8))/sd

# Rule number: 48 [tau=0.00105637918831131 cover=10279 (56%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all>=68.5
# num_purchase_all< 1.5
# 
# Rule number: 50 [tau=0.00864863016300949 cover=3612 (20%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all< 68.5
# NPS< 9.5
# 
# Rule number: 196 [tau=0.00487099700048721 cover=2414 (13%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all>=68.5
# num_purchase_all>=1.5
# NPS< 9.5
# num_purchase_all< 3.5
# 
# Rule number: 99 [tau=0.0153623655658251 cover=723 (4%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all>=68.5
# num_purchase_all>=1.5
# NPS>=9.5
# 
# Rule number: 102 [tau=0.0224704192251592 cover=474 (3%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all< 68.5
# NPS>=9.5
# money_spend_all< 78.97
# 
# Rule number: 394 [tau=0.00894090905181208 cover=330 (2%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all>=68.5
# num_purchase_all>=1.5
# NPS< 9.5
# num_purchase_all>=3.5
# money_spend_all>=51.46
# 
# Rule number: 103 [tau=0.0714950277312821 cover=238 (1%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all< 68.5
# NPS>=9.5
# money_spend_all>=78.97
# 
# Rule number: 5 [tau=-0.0151063943308175 cover=117 (1%)]
# money_spend_all< 3.305
# satisfied=0
# 
# Rule number: 26 [tau=0.0363326207937419 cover=74 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all>=595.3
# money_spend_all>=722.8
# 
# Rule number: 60 [tau=0.0350059171868227 cover=52 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all< 14.5
# lastday_purchase_all>=2.5
# lastday_purchase_all>=4.5
# NPS< 7.5
# 
# Rule number: 54 [tau=0.0761083854348245 cover=32 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all>=595.3
# money_spend_all< 722.8
# money_spend_all< 695.5
# 
# Rule number: 17 [tau=-0.160792633206209 cover=23 (0%)]
# money_spend_all< 3.305
# satisfied=1
# lastday_purchase_all>=38.5
# lastday_purchase_all>=77.5
# 
# Rule number: 14 [tau=-0.0509239208838036 cover=21 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all< 14.5
# lastday_purchase_all< 2.5
# 
# Rule number: 31 [tau=0.260462335867456 cover=19 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all< 14.5
# lastday_purchase_all>=2.5
# lastday_purchase_all< 4.5
# 
# Rule number: 16 [tau=-0.456002846211447 cover=17 (0%)]
# money_spend_all< 3.305
# satisfied=1
# lastday_purchase_all>=38.5
# lastday_purchase_all< 77.5
# 
# Rule number: 123 [tau=0.305118363675444 cover=14 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all< 14.5
# lastday_purchase_all>=2.5
# lastday_purchase_all>=4.5
# NPS>=7.5
# num_purchase_all>=2.5
# 
# Rule number: 122 [tau=0.0473911398090226 cover=10 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all< 14.5
# lastday_purchase_all>=2.5
# lastday_purchase_all>=4.5
# NPS>=7.5
# num_purchase_all< 2.5
# 
# Rule number: 55 [tau=0.34966343154613 cover=7 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all>=595.3
# money_spend_all< 722.8
# money_spend_all>=695.5
# 
# Rule number: 395 [tau=0.389805540903097 cover=7 (0%)]
# money_spend_all>=3.305
# lastday_purchase_all>=14.5
# money_spend_all< 595.3
# lastday_purchase_all>=68.5
# num_purchase_all>=1.5
# NPS< 9.5
# num_purchase_all>=3.5
# money_spend_all< 51.46
# 
# Rule number: 9 [tau=0.0103288495532001 cover=7 (0%)]
# money_spend_all< 3.305
# satisfied=1
# lastday_purchase_all< 38.5