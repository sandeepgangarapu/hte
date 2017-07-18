setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
library(dplyr)
library(ggplot2)


utility_data_1 <- read.csv('./files/utility_output.csv')

for (j in 1:126) {
set.seed(j)
utility_data <- sample_n(utility_data_1, 1000)
benefit_m <- as.matrix(utility_data %>% select(alt_output,equi_output,ego_output))
cost_m <- as.matrix(utility_data %>% select(alt_cost,equi_cost,ego_cost))
cost_vec <- c(cost_m)
# Total no. of possible treatments
n_treatments <- ncol(benefit_m)

# Total no of users
n_users <- nrow(benefit_m)


# Calculating total utility. Here I am considering the utility as total benefit-total cost
utility_m <- benefit_m - cost_m

utility_v <- apply(utility_m,1,max)
max_rank <- max.col(utility_m)
rank_order <- order(utility_v, decreasing = TRUE)

cost_v <- vector('double')
for (i in 1:length(utility_v)) {
  cost_v <- c(cost_v, cost_vec[(max_rank[i]-1)*length(utility_v)+i])
}

greedy_func <- function(budget) {
  total_utility_greedy = 0
  total_cost = 0
  for (i in 1:length(rank_order)) {
    
    if (utility_v[rank_order[i]] < 0) {break}
    
    total_cost = total_cost+cost_v[rank_order[i]]
    total_utility_greedy = total_utility_greedy + utility_v[rank_order[i]]

    if (total_cost > budget) {
      total_cost = total_cost-cost_v[rank_order[i]]
      total_utility_greedy = total_utility_greedy - utility_v[rank_order[i]]
      break
    } 
  }
  
  budget_left = budget - total_cost
  return (c(i, total_utility_greedy, total_cost, budget_left))
}

budget_v <- seq(10,130,5)

greedy_solution <- sapply(budget_v, greedy_func)
transpose_dataframe <- data.frame(t(greedy_solution))
colnames(transpose_dataframe) <- c('users_served', 'total_utility_greedy', 'total_cost_greedy', 'budget_left')
transpose_dataframe$budget_v <- budget_v
write.table(transpose_dataframe, './files/greedy_graph_output.csv', sep = ",", col.names = T, append = T, row.names = FALSE)
}