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

positions <- function(paths)
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
    new_paths[[i]] <- list(lpag=lpag, lpal=lpal, msag=msag,
                           msal=msal, npsg=npsg, npsl=npsl,
                           npag=npag, npal=npal, sat=sat, sur=sur)
  }
  return(new_paths)
}

dummy <- function(data, covar, val, sub_path){
  covar_data <- data.frame(col=data[,covar])
  plot_covar <- ggplot(covar_data, aes(col))+geom_histogram(bins=50)+geom_vline(xintercept = as.numeric(val), color = 'green')+ labs(x = covar)
  print(plot_covar)
  percentile <- ecdf(covar_data$col)
  percentile_position <- percentile(as.numeric(val))*100
  print(c(sub_path, percentile_position))
  print("mean")
  print(mean(covar_data$col))
  print("median")
  print(median(covar_data$col))
}



grapher <- function(data, path) {
  output <- list()
  split_path <- unlist(strsplit(as.character(path[[1]]), ','))
  for(i in 1:length(split_path)){
    sub_path <- split_path[i]
    if(grepl("<=",sub_path)){
      covar <- gsub("^(.*?)<=.*", "\\1", sub_path)
      val <- gsub("^.*<=(.*?)", "\\1", sub_path)
      dummy(data, covar, val, sub_path)
    } else if (grepl(">=",sub_path)){
      covar <- gsub("^(.*?)>=.*", "\\1", sub_path)
      val <- gsub("^.*>=(.*?)", "\\1", sub_path)
      dummy(data, covar, val, sub_path)
    } else if (grepl("=",sub_path)){
      covar <- gsub("^(.*?)=.*", "\\1", sub_path)
      val <- gsub("^.*=(.*?)", "\\1", sub_path)
      dummy(data, covar, val, sub_path)
  } else if (grepl(">",sub_path)){
    covar <- gsub("^(.*?)>.*", "\\1", sub_path)
    val <- gsub("^.*>(.*?)", "\\1", sub_path)
    dummy(data, covar, val, sub_path)
} else if (grepl("<",sub_path)){
  covar <- gsub("^(.*?)<.*", "\\1", sub_path)
  val <- gsub("^.*<(.*?)", "\\1", sub_path)
  dummy(data, covar, val, sub_path)
}
  }
}
