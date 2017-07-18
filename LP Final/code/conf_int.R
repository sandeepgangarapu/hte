library(dplyr)
library(ggplot2)
setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
lp_data <- read.csv('./files/lp_graph_output.csv')
greedy_data <- read.csv('./files/greedy_graph_output.csv')
ate_data <- read.csv('./files/ate_graph_output.csv')

lp_data <- lp_data %>% filter(budget_v != 'budget_v') 
greedy_data <- greedy_data %>% filter(budget_v != 'budget_v') 
ate_data <- ate_data %>% filter(budget_v != 'budget_v') 

cols = c(1, 2, 3, 4, 5)
lp_data[,cols] = apply(lp_data[,cols], 2, function(x) as.numeric(as.character(x)))
greedy_data[,cols] = apply(greedy_data[,cols], 2, function(x) as.numeric(as.character(x)))
ate_data[,cols] = apply(ate_data[,cols], 2, function(x) as.numeric(as.character(x)))

lp_data <- lp_data %>% group_by(budget_v) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n()))) %>% ungroup()
greedy_data <- greedy_data %>% group_by(budget_v) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n()))) %>% ungroup()
ate_data <- ate_data %>% group_by(budget_v) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n()))) %>% ungroup()

ggplot() +
  geom_line(data=greedy_data, aes(budget_v, total_utility_greedy_mean, colour="Greedy Algorithm")) +
  geom_ribbon(data=greedy_data, aes(x=budget_v, y=total_utility_greedy_mean,ymin=total_utility_greedy_mean+total_utility_greedy_sd, ymax=total_utility_greedy_mean-total_utility_greedy_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=lp_data, aes(budget_v, total_utility_lp_mean, colour="HTE_MAX")) +
  geom_ribbon(data=lp_data, aes(x=budget_v, y=total_utility_lp_mean,ymin=total_utility_lp_mean+total_utility_lp_sd, ymax=total_utility_lp_mean-total_utility_lp_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=ate_data, aes(budget_v, total_utility_ate_mean, colour="ATE")) +
  geom_ribbon(data=ate_data, aes(x=budget_v, y=total_utility_ate_mean,ymin=total_utility_ate_mean+total_utility_ate_sd, ymax=total_utility_ate_mean-total_utility_ate_sd), alpha=0.2, fill = "grey70") +
  labs(x = "Budget in $", y = "Utility in $", title ="Graph of Budget Vs. Utility", color='Allocation Methods') +
  ggsave("./plots/plots_ci/plot_utility.png", width = 8, height = 5) 


ggplot() +
  geom_line(data=greedy_data, aes(budget_v, users_served_mean, colour="Greedy Algorithm")) +
  geom_ribbon(data=greedy_data, aes(x=budget_v, y=users_served_mean,ymin=users_served_mean+users_served_sd, ymax=users_served_mean-users_served_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=lp_data, aes(budget_v, users_served_mean, colour="HTE_MAX")) +
  geom_ribbon(data=lp_data, aes(x=budget_v, y=users_served_mean,ymin=users_served_mean+users_served_sd, ymax=users_served_mean-users_served_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=ate_data, aes(budget_v, num_users_ate_mean, colour="ATE")) +
  geom_ribbon(data=ate_data, aes(x=budget_v, y=num_users_ate_mean,ymin=num_users_ate_mean+num_users_ate_sd, ymax=num_users_ate_mean-num_users_ate_sd), alpha=0.2, fill = "grey70") +
  labs(x = "Budget in $", y = "No. of Users served", title ="Graph of Budget Vs. No. of Users served", color='Allocation Methods') +
  ggsave("./plots/plots_ci/plot_user.png", width = 8, height = 5) 


ggplot() +
  geom_line(data=greedy_data, aes(budget_v, total_cost_greedy_mean, colour="Greedy Algorithm")) +
  geom_ribbon(data=greedy_data, aes(x=budget_v, y=total_cost_greedy_mean,ymin=total_cost_greedy_mean+total_cost_greedy_sd, ymax=total_cost_greedy_mean-total_cost_greedy_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=lp_data, aes(budget_v, total_cost_lp_mean, colour="HTE_MAX")) +
  geom_ribbon(data=lp_data, aes(x=budget_v, y=total_cost_lp_mean,ymin=total_cost_lp_mean+total_cost_lp_sd, ymax=total_cost_lp_mean-total_cost_lp_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=ate_data, aes(budget_v, cost_ate_mean, colour="ATE")) +
  geom_ribbon(data=ate_data, aes(x=budget_v, y=cost_ate_mean,ymin=cost_ate_mean+cost_ate_sd, ymax=cost_ate_mean-cost_ate_sd), alpha=0.2, fill = "grey70") +
  labs(x = "Budget in $", y = "Cost of Treatment", title ="Graph of Budget Vs. Cost of Treatment", color='Allocation Methods') +
  ggsave("./plots/plots_ci/plot_cost.png", width = 8, height = 5)


ggplot() +
  geom_line(data=greedy_data, aes(budget_v, budget_left_mean, colour="Greedy Algorithm")) +
  geom_ribbon(data=greedy_data, aes(x=budget_v, y=budget_left_mean,ymin=budget_left_mean+budget_left_sd, ymax=budget_left_mean-budget_left_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=lp_data, aes(budget_v, budget_left_mean, colour="HTE_MAX")) +
  geom_ribbon(data=lp_data, aes(x=budget_v, y=budget_left_mean,ymin=budget_left_mean+budget_left_sd, ymax=budget_left_mean-budget_left_sd), alpha=0.2, fill = "grey70") +
  geom_line(data=ate_data, aes(budget_v, left_budget_mean, colour="ATE")) +
  geom_ribbon(data=ate_data, aes(x=budget_v, y=left_budget_mean,ymin=left_budget_mean+left_budget_sd, ymax=left_budget_mean-left_budget_sd), alpha=0.2, fill = "grey70") +
  labs(x = "Budget in $", y = "Left over budget in $", title ="Graph of Budget Vs. Left over budget in $", color='Allocation Methods') +
  ggsave("./plots/plots_ci/plot_left_budget.png", width = 8, height = 5)