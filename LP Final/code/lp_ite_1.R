library(lpSolveAPI)
setwd('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
library(dplyr)

# Benefit(Treatment effect) matrix for all users, I have users as row indices and treatments as column indices

utility_data_1 <- read.csv('./files/utility_output.csv')
for (i in 14:15) {
  set.seed(i)
  utility_data <- sample_n(utility_data_1, 1000)
  benefit_m <- as.matrix(utility_data %>% select(alt_output,equi_output,ego_output))
  cost_m <- as.matrix(utility_data %>% select(alt_cost,equi_cost,ego_cost))
  
  
  # Total no. of possible treatments
  n_treatments <- ncol(benefit_m)
  
  # Total no of users
  n_users <- nrow(benefit_m)
  
  # Total no of decisions to be made, which user should be assigned to which treatment
  n_decisions <- n_users*n_treatments
  
  # Total available budget
  # budget <- 1000
  
  
  
  # Calculating total utility. Here I am considering the utility as total benefit-total cost
  utility_m <- benefit_m - cost_m
  utility_v <- c(utility_m)
  
  
  lp_func <- function (custom_budget) {
    # Initializing the linear programming function
    lprec<-make.lp(0,n_decisions)
    
    # Initializing the objective function
    # The objective function here is the maximization of (total benefit - total cost) i.e. total utility
    set.objfn(lprec, utility_v)
    
    # Adding constarints
    # 1: Each user can be assigned to only one treatment.
    # 2. Total cost cannot exceed budget.
    # 3. Each user can only be assigned to one treatment only once
    
    # Adding constraint 1
    # 1: Each user can be assigned to maximum one treatment.
    rep.col<-function(x,n){
      matrix(rep(x,each=n), ncol=n, byrow=TRUE)
    }
    I <- diag(n_users)
    for(i in 1:n_users){
      add.constraint(lprec, c(rep.col(I[,i],n_treatments)), "<=", 1)
    }
    
    # Adding constraint 2
    # 2. Total cost cannot exceed budget.
    add.constraint(lprec, c(cost_m), "<=", custom_budget)
    
    # Adding constraint 3
    # 3. set the type of decision variables to binary
    set.type(lprec,1:n_decisions,"binary")
    
    # set the LP solver preference to maximization (instead of minimization)
    lp.control(lprec,sense='max')
    
    # set time limit 
    lp.control(lprec, timeout = 60)
    
    # Visually check the model, write the model to a text file
    write.lp(lprec,'./files/lpmodel_opt.txt',type='lp')
    
    # solve the model
    # The output '0' means that there is a solution
    solve(lprec)
    
    # Final result
    # This matrix shows which user is allocated what treatment under the set constraints 
    # We can see that many users are not allocated any treatment because of constrained budget.
    assign_matrix <- matrix(get.variables(lprec),nrow=n_users)
    
    # save the results in a csv
    write.csv(matrix(get.variables(lprec),nrow=n_users),"./files/lp_opt_solution.csv")
    
    
    utility_assign_matrix <- utility_m * assign_matrix
    
    cost_assign_matrix <- cost_m * assign_matrix
    
    users_served <- sum(c(assign_matrix))
    total_utility_lp <- sum(c(utility_assign_matrix))
    total_cost_lp <- sum(c(cost_assign_matrix))
    budget_left = custom_budget - total_cost_lp
    return (c(users_served, total_utility_lp, total_cost_lp, budget_left))
  }
  
  #lp_func(30)
  budget_v <- seq(10,130,5)
  #budget_v <- 10
  
  lp_solution <- sapply(budget_v, lp_func)
  transpose_dataframe <- data.frame(t(lp_solution))
  colnames(transpose_dataframe) <- c('users_served', 'total_utility_lp', 'total_cost_lp', 'budget_left')
  transpose_dataframe$budget_v <- budget_v
  write.table(transpose_dataframe, './files/lp_graph_output_1.csv', sep = ",", col.names = T, append = T, row.names = FALSE)
}


