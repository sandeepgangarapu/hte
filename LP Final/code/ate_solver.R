setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
library(dplyr)

utility_data <- read.csv('./files/utility_output.csv')
set.seed(12345)
utility_data <- sample_n(utility_data, 1000)

benefit_m <- as.matrix(utility_data %>% select(alt_output,equi_output,ego_output))
cost_m <- as.matrix(utility_data %>% select(alt_cost,equi_cost,ego_cost))


ben_v <- c(mean(utility_data$alt_output), mean(utility_data$equi_output), mean(utility_data$ego_output))


cost_v <- c(mean(utility_data$alt_cost), mean(utility_data$equi_cost), mean(utility_data$ego_cost))

utility_v <- ben_v - cost_v

# In this scenario we choose the treatment that has the highest average treatment effect.
# In this case, it is altruistic treatment 

budget_v <- seq(10,130,5)
num_users_ate <- c((floor(budget_v/cost_v[1]))[1:12], rep(1000,13)) 
                   #min(floor(budget_v/cost_v[1]), nrow(utility_data))
total_utility_ate <- num_users_ate * utility_v[1]
cost_ate <- num_users_ate * cost_v[1]
left_budget <- budget_v - cost_ate

ate_df <- data.frame(budget_v,num_users_ate,total_utility_ate,cost_ate,left_budget)



