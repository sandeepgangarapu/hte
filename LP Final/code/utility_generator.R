library(dplyr)
library(treeClust)
setwd('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
source('./code/berry_function.R')

# Reading the raw data
data <- read.csv('./data/collage_raw.csv')

# Filtering out the users who do not open email
data <- data %>% filter(emailopen_b == 1)

# Creating benefit and cost columns based on coupon purchases
collage_data <- data %>% mutate(benefit = referrer_money_spend+friend_money_spend) %>% mutate(cost = 10)

# Assiging different treatment conditions
control_placeholder <- '1'
egoistic_placeholder <- '2'
equitable_placeholder <- '3'
altruistic_placeholder <- '4'

# Benefit Treatment effect for altruistic model
alt_output <- berry2s(
  data = collage_data,
  control_group_placeholder = control_placeholder,
  treatment_group_placeholder = altruistic_placeholder,
  treatment_variable = "cell",
  target_variable = "benefit",
  covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
  covariate_factors = c("cell","satisfied","survey"))
write.csv(alt_output[[2]],'./files/alt_output.csv')
collage_data['alt_output'] <- alt_output[[2]]
collage_data['alt_leaf'] <- leaf.numbers(alt_tree)
collage_data <- collage_data %>% group_by(alt_leaf) %>% mutate(alt_cost = mean(cost)) %>% ungroup()



# # Benefit Treatment effect for egoistic model

ego_output <- berry2s(
  data = collage_data,
  control_group_placeholder = control_placeholder,
  treatment_group_placeholder = egoistic_placeholder,
  treatment_variable = "cell",
  target_variable = "benefit",
  covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
  covariate_factors = c("cell","satisfied","survey"))
write.csv(ego_output[[2]],'./files/ego_output.csv')
collage_data['ego_output'] <- ego_output[[2]]
collage_data['ego_leaf'] <- leaf.numbers(ego_output[[1]])
collage_data <- collage_data %>% group_by(ego_leaf) %>% mutate(ego_cost = mean(cost)) %>% ungroup()
# Benefit Treatment effect for equitable model

equi_output <- berry2s(
  data = collage_data,
  control_group_placeholder = control_placeholder,
  treatment_group_placeholder = equitable_placeholder,
  treatment_variable = "cell",
  target_variable = "benefit",
  covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
  covariate_factors = c("cell","satisfied","survey"))
write.csv(equi_output[[2]],'./files/equi_output.csv')
collage_data['equi_output'] <- equi_output[[2]]
collage_data['equi_leaf'] <- leaf.numbers(equi_output[[1]])
collage_data <- collage_data %>% group_by(equi_leaf) %>% mutate(equi_cost = mean(cost)) %>% ungroup()
write.csv(collage_data,'./files/utility_output.csv')


# 
# # Cost Treatment effect for altruistic model
# 
# alt_c_output <- berry2s(
#   data = collage_data,
#   control_group_placeholder = control_placeholder,
#   treatment_group_placeholder = altruistic_placeholder,
#   treatment_variable = "cell",
#   target_variable = "cost",
#   covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
#   covariate_factors = c("cell","satisfied","survey"))
# d <- data.frame(alt_c_output)
# write.csv(d,'./files/alt_c_output.csv')
# 
# # Cost Treatment effect for egoistic model
# 
# ego_c_output <- berry2s(
#   data = collage_data,
#   control_group_placeholder = control_placeholder,
#   treatment_group_placeholder = egoistic_placeholder,
#   treatment_variable = "cell",
#   target_variable = "cost",
#   covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
#   covariate_factors = c("cell","satisfied","survey"))
# e <- data.frame(ego_c_output)
# write.csv(e,'./files/ego_c_output.csv')
# 
# # Cost Treatment effect for equitable model
# 
# equi_c_output <- berry2s(
#   data = collage_data,
#   control_group_placeholder = control_placeholder,
#   treatment_group_placeholder = equitable_placeholder,
#   treatment_variable = "cell",
#   target_variable = "cost",
#   covariates = c("satisfied","NPS","lastday_purchase_all","num_purchase_all","money_spend_all","survey"),
#   covariate_factors = c("cell","satisfied","survey"))
# f <- data.frame(equi_c_output)
# write.csv(f,'./files/equi_c_output.csv')
