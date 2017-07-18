data <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv')
control_placeholder <- 1
egoistic_placeholder <- 2
equitable_placeholder <- 3
altruistic_placeholder <- 4
  control_group_placeholder = control_placeholder
  treatment_group_placeholder = altruistic_placeholder
  treatment_variable = "cell"
  target_variable = "number_referrals"
  covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey")
  covariate_factors = c("satisfied","survey")
control_data <- data[data[treatment_variable]==control_group_placeholder,]
control_data[treatment_variable] <- 0
treatment_data <- data[data[treatment_variable]==treatment_group_placeholder,]
treatment_data[treatment_variable] <- 1
data <- rbind(control_data, treatment_data)
treat_idx <-which(data[treatment_variable] == 1)
cont_idx <-which(data[treatment_variable] == 0)
train_idx <- c(sample(treat_idx, length(treat_idx)/2),
               sample(cont_idx, length(cont_idx)/2))
train_data <- data[train_idx,]
est_data <- data[-train_idx,]

f = sprintf("%s ~ %s",target_variable,paste(covariates,collapse = ' + '))
tree <- honest.causalTree(as.formula(f),
                          data = train_data,treatment = train_data[[treatment_variable]],
                          est_data = est_data, est_treatment = est_data[[treatment_variable]],
                          split.Rule = "CT", split.Honest = T, cv.option="CT", 
                          cv.Honest=T, HonestSampleSize = nrow(est_data))
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
leaf_frame['sd'] <- (leaf_frame$yval-mean_tau2s)/sd_tau2s

#Adding t statistic to see if the treatment effect(y_val) is significantly
#different from zero
leaf_frame['t_statistic'] <- (leaf_frame$yval*sqrt(leaf_frame$n))/sd_tau2s
leaf_frame['p_value'] <- 2*pt(-abs(leaf_frame$t_statistic),df=leaf_frame$n - 1)

#Adding paths to leaf_frame
leaf_frame$path <- sapply(dedup_paths, paste0, collapse = ',')

#Ordering frame by descending order of treatment effect
leaf_frame <- leaf_frame[order(-leaf_frame$yval),] 

#Subsetting leaf frame to only keep useful values
leaf_frame <- leaf_frame[c("n","yval","sd","p_value","path")] 


return(leaf_frame)