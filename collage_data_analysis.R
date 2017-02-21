data <- read.csv('C:\\Users\\ganga020\\Google Drive\\Ed Research\\Heterogenous treatment effects\\data.csv')
summary(data)
str(data)

library(dplyr)
library(sqldf)

#adding a new variable 'treatment_group' to dataframe data
for(i in 1:nrow(data)) {
  if (data[i,'cell'] == 1) {
    data[i, 'treatment_group'] <- 'control'
  } else if (data[i,'cell'] == 2)  {
    data[i, 'treatment_group'] <- 'egoistic'
  } else if (data[i,'cell'] == 3)  {
    data[i, 'treatment_group'] <- 'equitable'
  } else data[i, 'treatment_group'] <- 'altruistic'
}

#summary of treatment groups and their referrals
sqldf('select treatment_group, sum(number_referrals), count(treatment_group),  
      sum(number_referrals)/count(treatment_group) from data group by treatment_group')

#subsetting data to contain just 'altruistic group' so as to do heterogeneity
altruistic_data <- sqldf('select * from data where cell = 4')


