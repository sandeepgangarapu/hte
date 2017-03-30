data <- read.csv('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\data.csv')
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

#average effect of all groups
avg_effect <- data %>% group_by(treatment_group) %>% summarise(avg = mean(number_referrals))

#average treatment effect of altruistic group
avg_effect[avg_effect$treatment_group=='altruistic','avg'] - avg_effect[avg_effect$treatment_group=='control','avg']

#subsetting data to contain just 'altruistic group and control group' so as to do heterogeneity
altruistic_data <- data %>% filter(cell==c(4,1))

#Subsetting variables that matter
alt_data_filter <- altruistic_data[,c("cell","satisfied","NPS","lastday_purchase_all","num_purchase_all",
                                      "money_spend_all","survey", "number_referrals", "con_rate")]

write.csv(alt_data_filter, 'C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\collage_treatment_effect.csv', row.names = FALSE)
