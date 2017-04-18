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


path <- "lastday_purchase_all< 42.5,money_spend_all< 4.685"
a <- unlist(strsplit(path[[1]], ','))
for(i in 1:length(a)){
  if(grepl("<",a[i])){
    b <- gsub("^(.*?)<.*", "\\1", a[i])
    c <- gsub("^.*<(.*?)", "\\1", a[i])
    d <- collage_data[,b]
    print(qplot(d, geom = 'histogram'))
    print(b)
    print(c)
  }
}
