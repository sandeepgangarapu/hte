library(lpSolveAPI)
setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
library(dplyr)
library(ggplot2)


utility_data <- read.csv('./files/utility_output.csv')
set.seed(12345)
utility_data <- sample_n(utility_data, 1000)

benefit_m <- as.matrix(utility_data %>% select(alt_output,equi_output,ego_output))
cost_m <- as.matrix(utility_data %>% select(alt_cost,equi_cost,ego_cost))
cost_vec <- c(cost_m)
# Total no. of possible treatments
n_treatments <- ncol(benefit_m)

# Total no of users
n_users <- nrow(benefit_m)


# Total available budget
# budget <- 100

# Calculating total utility. Here I am considering the utility as total benefit-total cost
utility_m <- benefit_m - cost_m

utility_v <- apply(utility_m,1,max)
max_rank <- max.col(utility_m)
rank_order <- order(utility_v, decreasing = TRUE)

cost_v <- vector('double')
for (i in 1:length(utility_v)) {
  cost_v <- c(cost_v, cost_vec[(max_rank[i]-1)*length(utility_v)+i])
}



greedy_func_u <- function(budget) {
  total_utility_greedy = 0
  total_cost = 0
for (i in 1:length(rank_order)) {
  
  if (utility_v[rank_order[i]] < 0) {break}
  
  total_cost = total_cost+cost_v[rank_order[i]]
  print(cost_v[rank_order[i]])
  total_utility_greedy = total_utility_greedy + utility_v[rank_order[i]]
  print(total_cost)
  
  if (total_cost > budget) {
    total_cost = total_cost-cost_v[rank_order[i]]
    total_utility_greedy = total_utility_greedy - utility_v[rank_order[i]]
    break
  } 
}

budget_left = budget - total_cost
return (total_utility_greedy)

}

greedy_func_c <- function(budget) {
  total_utility_greedy = 0
  total_cost = 0
  for (i in 1:length(rank_order)) {
    
    if (utility_v[rank_order[i]] < 0) {break}
    
    total_cost = total_cost+cost_m[rank_order[i]]
    total_utility_greedy = total_utility_greedy + utility_v[rank_order[i]]
    
    
    if (total_cost > budget) {
      total_cost = total_cost-cost_m[rank_order[i]]
      total_utility_greedy = total_utility_greedy - utility_v[rank_order[i]]
      break
    } 
  }
  
  budget_left = budget - total_cost
  return (total_cost)
}

greedy_func_b <- function(budget) {
  total_utility_greedy = 0
  total_cost = 0
  for (i in 1:length(rank_order)) {
    
    if (utility_v[rank_order[i]] < 0) {break}
    
    total_cost = total_cost+cost_m[rank_order[i]]
    total_utility_greedy = total_utility_greedy + utility_v[rank_order[i]]
    
    
    if (total_cost > budget) {
      total_cost = total_cost-cost_m[rank_order[i]]
      total_utility_greedy = total_utility_greedy - utility_v[rank_order[i]]
      break
    } 
  }
  
  budget_left = budget - total_cost
  return (budget_left)
  
}

greedy_func_users <- function(budget) {
  total_utility_greedy = 0
  total_cost = 0
  for (i in 1:length(rank_order)) {
    
    if (utility_v[rank_order[i]] < 0) {break}
    
    total_cost = total_cost+cost_m[rank_order[i]]
    total_utility_greedy = total_utility_greedy + utility_v[rank_order[i]]
    
    
    if (total_cost > budget) {
      total_cost = total_cost-cost_m[rank_order[i]]
      total_utility_greedy = total_utility_greedy - utility_v[rank_order[i]]
      break
    } 
  }
  
  budget_left = budget - total_cost
  return (i)
  
}

budget_v <- seq(10,130,5)


greedy_utility <- sapply(budget_v, greedy_func_u)
greedy_users <- sapply(budget_v, greedy_func_users)
greedy_cost <- sapply(budget_v, greedy_func_c)
greedy_budget <- sapply(budget_v, greedy_func_b)

df1 <- data.frame(budget_v, greedy_utility)
df2 <- data.frame(budget_v, greedy_users)
df3 <- data.frame(budget_v, greedy_cost)
df4 <- data.frame(budget_v, greedy_budget)

gp1 <- ggplot(df1, aes(budget_v, greedy_utility)) + geom_line()
gp2 <- ggplot(df2, aes(budget_v, greedy_users)) + geom_line()
gp3 <- ggplot(df3, aes(budget_v, greedy_cost)) + geom_line()
gp4 <- ggplot(df4, aes(budget_v, greedy_budget)) + geom_line()


lp_data <- read.csv('./files/lp_graph_output.csv')
dp1 <- data.frame(lp_data$budget_v, lp_data$total_utility_lp)
dp2 <- data.frame(lp_data$budget_v, lp_data$users_served)
dp3 <- data.frame(lp_data$budget_v, lp_data$total_cost_lp)
dp4 <- data.frame(lp_data$budget_v, lp_data$budget_left)

ggplot() +
  geom_line(data=df1, aes(budget_v, greedy_utility, colour="Greedy Algorithm")) +
  geom_line(data=dp1, aes(lp_data.budget_v, lp_data.total_utility_lp, colour="LP Optimization")) +
  geom_line(data=ate_df, aes(budget_v, total_utility_ate, colour="ATE")) +
  labs(x = "Budget in $", y = "Utility in $", title ="Graph of Budget Vs. Utility") +
  ggsave("plot_utility.png", width = 8, height = 5)

ggplot() +
  geom_line(data=df1, aes(budget_v, greedy_utility, colour="Greedy Algorithm") ) +
  geom_line(data=dp1,aes(lp_data.budget_v, lp_data.total_utility_lp, colour="LP Optimization")) +
  labs(x = "Budget in $", y = "Utility in $", title ="Graph of Budget Vs. Utility") +
  ggsave("plot_utility_2.png", width = 8, height = 5)


ggplot() +
  geom_line(data=df2, aes(budget_v, greedy_users, colour="Greedy Algorithm") ) +
  geom_line(data=dp2,aes(lp_data.budget_v, lp_data.users_served, colour="LP Optimization")) +
  geom_line(data=ate_df,aes(budget_v, num_users_ate, colour="ATE")) +
  labs(x = "Budget in $", y = "No. of users served", title ="Graph of Budget Vs. No. of users") +
  ggsave("plot_users.png", width = 8, height = 5)


ggplot() +
  geom_line(data=df3, aes(budget_v, greedy_cost, colour="Greedy Algorithm") ) +
  geom_line(data=dp3,aes(lp_data.budget_v, lp_data.total_cost_lp, colour="LP Optimization")) +
  geom_line(data=ate_df,aes(budget_v, cost_ate, colour="ATE")) +
  labs(x = "Budget in $", y = "Cost of treatment in $", title ="Graph of Budget Vs. Cost of treatment") +
  ggsave("plot_cost.png", width = 8, height = 5)

ggplot() +
  geom_line(data=df4, aes(budget_v, greedy_budget, colour="Greedy Algorithm") ) +
  geom_line(data=dp4,aes(lp_data.budget_v, lp_data.budget_left, colour="LP Optimization")) +
  geom_line(data=ate_df,aes(budget_v, left_budget, colour="ATE")) +
  labs(x = "Budget in $", y = "Left over budget in $", title ="Graph of Budget Vs. Left over budget") +
  ggsave("plot_budget_left.png", width = 8, height = 5)


