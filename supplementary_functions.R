#TODO:
#1. Generalize dedup function


# This function deduplicates the paths to leaf nodes
#TODO:1
deduplication <- function(paths)
{
  new_paths <- list()
  for (i in 1:length(paths)){
    a <- strsplit(paths[[i]], ',')
    lpag<- NULL; lpal<- NULL; msag<- NULL; msal<- NULL; npsg<- NULL; npsl<- NULL; npag<- NULL; npal<- NULL; sat<- NULL; sur<- NULL;
    for (j in 1:length(a)){
      #print(a[[j]])
      if(grepl('lastday_purchase_all>', a[[j]])) {lpag = a[[j]]} #else {lpag = NULL}
      if(grepl('lastday_purchase_all<', a[[j]])) {lpal = a[[j]]} #else {lpal = NULL}
      if(grepl('money_spend_all>', a[[j]])) {msag = a[[j]]} #else {msag = NULL}
      if(grepl('money_spend_all<', a[[j]])) {msal = a[[j]]} #else {msal = NULL}
      if(grepl('NPS>', a[[j]])) {npsg = a[[j]]} #else {npsg = NULL}
      if(grepl('NPS<', a[[j]])) {npsl = a[[j]]} #else {npsl = NULL}
      if(grepl('num_purchase_all>', a[[j]])) {npag = a[[j]]} #else {npag = NULL}
      if(grepl('num_purchase_all<', a[[j]])) {npal = a[[j]]} #else {npal = NULL}
      if(grepl('satisfied', a[[j]])) {sat = a[[j]]} #else {sat = NULL}
      if(grepl('survey', a[[j]])) {sur = a[[j]]} #else {sur = NULL}
    }
    new_paths[[i]] <- c(lpag, lpal, msag, msal, npsg, npsl, npag, npal, sat, sur)
  }
  return(new_paths)
}



treatment_effects <- function(tree){
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
}
