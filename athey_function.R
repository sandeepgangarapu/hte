#install.packages("devtools")
#install_github("susanathey/causalTree")
#install_github("swager/randomForestCI")
library(causalTree)
library(devtools)
library(randomForestCI)
library(dplyr)
setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\git_code\\hte')
source("supplementary_functions.R")
source('findit.R')

athey <- function(data, control_group_placeholder, treatment_group_placeholder,
                    treatment_variable, target_variable, covariates,
                    covariate_factors)
{
  data[covariate_factors] <- lapply(data[covariate_factors], factor)
  control_data <- data[data[treatment_variable]==control_group_placeholder,]
  control_data[treatment_variable] <- 0
  treatment_data <- data[data[treatment_variable]==treatment_group_placeholder,]
  treatment_data[treatment_variable] <- 1
  set.seed(12345)
  control_data['split']<- sample(as.numeric(row.names(control_data))%% 3)
  treatment_data['split']<- sample(as.numeric(row.names(treatment_data))%% 3)
  est_data <- rbind(control_data %>% filter(split == 1), treatment_data %>% filter(split == 1))
  train_data <- rbind(control_data %>% filter(split == 0), treatment_data %>% filter(split == 0))
  test_data <- rbind(control_data %>% filter(split == 2), treatment_data %>% filter(split == 2))
  f = sprintf("%s ~ %s",target_variable,paste(covariates,collapse = ' + '))
  tree <- honest.causalTree(as.formula(f),
                            data = train_data,treatment = train_data[[treatment_variable]],
                            est_data = est_data, est_treatment = est_data[[treatment_variable]],
                            split.Rule = "CT", split.Honest = T, cv.option="CT",
                            cv.Honest=T, HonestSampleSize = nrow(est_data))
  tree_cp <- tree$cptable[,1][which.min(tree$cptable[,4])]
  pruned_tree <- prune(tree, tree_cp)
  leaf_results <- get_leaf_results(test_data, pruned_tree, treatment_variable, target_variable)
  leaf_results <- leaf_results[order(as.vector(leaf_results$effect), decreasing = TRUE),] 
  return(leaf_results)
}
