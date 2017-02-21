data <- read.csv('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\data.csv')
summary(data)
str(data)

library(dplyr)
library(sqldf)

for(i in 1:nrow(data)) {
  if (data[i,'cell'] == 1) {
    data[i, 'treatment_group'] <- 'control'
  } else if (data[i,'cell'] == 2)  {
    data[i, 'treatment_group'] <- 'egoistic'
  } else if (data[i,'cell'] == 3)  {
    data[i, 'treatment_group'] <- 'equitable'
  } else data[i, 'treatment_group'] <- 'altruistic'
}

data2 <- sqldf('select cell, sum(number_referrals), count(cell) from data group by cell')
