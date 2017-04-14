describe_paths <-function(path)
{
  path_dict = list()
  #TODO: Do not assume the data is all categorical and look also for < and >
  selection <- strsplit(path[-1],"=",T)
  for(s in selection)
    path_dict[s[1]] = strsplit(s[2],",",T)
  
  return(list(att_vals=path_dict))
}

get_treatment_effect<-function(data, alternative="greater", treatment_var='treatment',outcome_var='outcome'){
  treat = (data[[treatment_var]] == 1)
  
  treat_values <- data[[outcome_var]][treat]
  cont_values <- data[[outcome_var]][!treat]
  
  test <- t.test(treat_values, cont_values, alternative = alternative)
  
  treat_mean <- test$estimate[1]
  cont_mean <- test$estimate[2]
  
  return(list(effect=(treat_mean - cont_mean), p_value=test$p.value))
}

get_leaf_results<-function(data, tree, treatment_var='treatment', outcome_var='outcome',
                           t.test.alternative="greater"){
  
  #hack to get the leaf nodes for each data point. Replace each nodes yval--which is used as prediction
  #for data ponints that fall into that node--with the node label, so that it is what instead is returned.
  temp_tree <- tree
  temp_tree$frame$yval <- as.factor(rownames(temp_tree$frame))
  point_nodes <- as.vector(predict(temp_tree, newdata=data))
  
  #get the treatment_effect and p-value for each leaf node
  scores = by(data, point_nodes, get_treatment_effect, t.test.alternative, treatment_var=treatment_var,
              outcome_var=outcome_var)
  
  frame <- tree$frame
  leaves <- row.names(frame)[frame$var == '<leaf>']
  paths <- path.rpart(tree,leaves, print.it = FALSE)
  att_values_lists <- lapply(paths, describe_paths)
  #TODO: ensure that the names are the same (sets are equalor setdiff both ways is empty)
  #keys <- unique(c(names(lst1), names(lst2)))
  #list.of.lists[order(sapply(list.of.lists,'[[',1))]
  #sapply(list.of.lists,function(x) x[[1]] )
  results <- Map(c,scores, att_values_lists)
  
  return(results)
}

get_profile_results<-function(data, individual_estimates, test_lower_tail=FALSE){
  p.values <- pnorm(individual_estimates[['estimate']]/sqrt(individual_estimates[['variance']]),
                    lower.tail = test_lower_tail)
  
  #keep all the non-duplicated records
  unique_indx <- !duplicated(data)
  
  unique_profiles = data[unique_indx,]
  unique_profiles[,c('effect', 'p_value')] <- cbind(individual_estimates[unique_indx,c('estimate')],p.values[unique_indx])
  
  return(unique_profiles)
}