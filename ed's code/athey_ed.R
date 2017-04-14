install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree")
install_github("swager/randomForestCI")

library(causalTree)
library(randomForestCI) 
library(dplyr)

data1 <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')
data2 <- data1 %>% filter(cell == 4) %>% mutate(treatment=1)
data3 <- data1 %>% filter(cell == 1) %>% mutate(treatment=0)
data <- rbind(data2,data3)
data <- data %>% select(-c(cell, con_rate))
treat_idx <-which(data$treatment == 1)
cont_idx <-which(data$treatment == 0)
train_idx <- c(sample(treat_idx, length(treat_idx)/2),
               sample(cont_idx, length(cont_idx)/2))
train_data <- data[train_idx,]
est_data <- data[-train_idx,]

treatment_var = "treatment"
outcome_var = "number_referrals"

cov_cols = colnames(train_data)[!(colnames(train_data) %in% c(outcome_var, treatment_var))]
f = sprintf("%s ~ %s",outcome_var,paste(cov_cols,collapse = ' + '))


##### CausalTree #######
tree <- honest.causalTree(as.formula(f),
                          data = train_data,treatment = train_data[[treatment_var]],
                          est_data = est_data, est_treatment = et_dsata[[treatment_var]],
                          split.Rule = "CT", split.Honest = T, cv.option="CT", 
                          cv.Honest=T, HonestSampleSize = nrow(est_data))
#split.Bucket= ?, bucketNum = ?, bucketMax = ?, minsize = ?,
#split.alpha = ?, cv.alpha = ?

tree_cp <- tree$cptable[,1][which.min(tree$cptable[,4])]
pruned_tree <- prune(tree, tree_cp)
leaf_results <- get_leaf_results(est_data, pruned_tree, treatment_var, outcome_var)


##### InteractionTree #####
tstat <- honest.causalTree(as.formula(f),
                           data = train_data,treatment = train_data[[treatment_var]],
                           est_data = est_data, est_treatment = est_data[[treatment_var]],
                           split.Rule = "tstats", split.Honest = T,cv.option="CT", 
                           cv.Honest=T, HonestSampleSize = nrow(est_data))
#split.Bucket= ?, bucketNum = ?, bucketMax = ?, minsize = ?,
#split.alpha = ?, cv.alpha = ?

tstatcp <- tstat$cptable[,1][which.min(tstat$cptable[,4])]
tstat_pruned <- prune(tstat, tstatcp)
rpart.plot(tstat_pruned)
leaf_results <- get_leaf_results(est_data, tstat_pruned, treatment_var, outcome_var)

##### Causal Forest #####
#source("/Users/Ed/Desktop/causalForest.R")
#source("/Users/Ed/Desktop/randomForestCI.R")
num_train_row <- nrow(train_data)
num_train_covs <- length(cov_cols)

forest <- causalForest(as.formula(f), 
                       data=train_data, treatment=train_data[[treatment_var]],
                       split.Rule="CT", split.Honest=T, cv.option="CT", cv.Honest=T,
                       sample.size.total= floor(nrow(train_data) / 2), sample.size.train.frac=0.5,
                       #num.trees=min(num_train_row,20), ncolx=num_train_covs, #nodesize = 25,
                       num.trees=1, ncolx=num_train_covs, #nodesize = 25,
                       #ncov_sample=num_train_covs
                       ncov_sample=round(num_train_covs/3) #Stefan's simulation choice
                       #ncov_sample=round(log(num_train_covs)/log(2)) #Breiman used this for ncov_samples
)

test_data <- rbind(est_data[,cov_cols],train_data[,cov_cols])
colnames(test_data) <- paste("x", 1:length(cov_cols), sep="")
effects <- randomForestInfJack(forest, test_data, calibrate = TRUE)
colnames(effects) <- c('estimate', 'variance')
colnames(test_data) <- cov_cols
profile_results <- get_profile_results(test_data, effects, FALSE)

#Notes to Stefan: 1) causalForrest used x1, x2, ... for the column names and therefore so predict need to ensure
#cols are labelled as such. 2) when infJack calibrates takes a random sample of half the trees. If you only have
#two trees, then you end up with only one tree, and when you re-run infJack with calibration off and the selected
#tree(s), pred = pred[, used.trees] makes pred a single column, and rowMeans(pred) throws an error


library(dplyr)
profile_results %<>%
  mutate(detected = p_value <= 0.05) %>%
  mutate(affected = star=='regular+aide' | star=='small')
  
detected = profile_results[['detected']]
affected = profile_results[['affected']]



num_vals_intersect = sum(detected & affected) 
num_vals_union = sum(detected | affected) 

num_self_vals = sum(detected)
num_other_vals = sum(affected)
num_total_vals = nrow(profile_results)

precision = num_vals_intersect/num_self_vals
recall = num_vals_intersect/num_other_vals
overlap = num_vals_intersect/num_vals_union

paste(c(overlap, precision, recall), collapse = " ")
num_self_vals/num_total_vals
num_other_vals/num_total_vals


