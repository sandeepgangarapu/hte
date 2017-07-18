setwd('C:\\Users\\Sandeep Kumar\\Google Drive\\Ed Research\\Heterogenous treatment effects\\LP Final')
library(dplyr)
library(ggplot2)
library(reshape2)
utility_data <- read.csv('./files/utility_output.csv')


benefit <- utility_data %>% select(alt_output,equi_output,ego_output)

mean <- c(mean(benefit$alt_output),mean(benefit$equi_output),mean(benefit$ego_output))
standard_deviation <- c(sd(benefit$alt_output),sd(benefit$equi_output),sd(benefit$ego_output))
median <- c(median(benefit$alt_output),median(benefit$equi_output),median(benefit$ego_output))
third_quartile <- c(quantile(benefit$alt_output,0.75),quantile(benefit$equi_output,0.75),quantile(benefit$ego_output,0.75))
first_quartile <- c(quantile(benefit$alt_output,0.25),quantile(benefit$equi_output,0.25),quantile(benefit$ego_output,0.25))

ben_stats <- rbind(mean,standard_deviation,median,third_quartile,first_quartile)

benefit <- melt(benefit)
benefit <- benefit %>% mutate(Treatment_type = ifelse(
  variable=='alt_output','Altruistic',ifelse(
    variable=='equi_output','Equitable','Egoistic')))
ggplot(benefit, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Benefit in $', title='Boxplot of Benefit') +
  ggsave("./plots/stats/plot_benefit.png", width = 8, height = 5) 




cost <- utility_data %>% select(alt_cost,equi_cost,ego_cost)

mean <- c(mean(cost$alt_cost),mean(cost$equi_cost),mean(cost$ego_cost))
standard_deviation <- c(sd(cost$alt_cost),sd(cost$equi_cost),sd(cost$ego_cost))
median <- c(median(cost$alt_cost),median(cost$equi_cost),median(cost$ego_cost))
third_quartile <- c(quantile(cost$alt_cost,0.75),quantile(cost$equi_cost,0.75),quantile(cost$ego_cost,0.75))
first_quartile <- c(quantile(cost$alt_cost,0.25),quantile(cost$equi_cost,0.25),quantile(cost$ego_cost,0.25))

cost_stats <- rbind(mean,standard_deviation,median,third_quartile,first_quartile)

cost <- melt(cost)
cost <- cost %>% mutate(Treatment_type = ifelse(
  variable=='alt_cost','Altruistic',ifelse(
    variable=='equi_cost','Equitable','Egoistic')))
ggplot(cost, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Cost in $', title='Boxplot of Cost') +
  ggsave("./plots/stats/plot_cost.png", width = 8, height = 5) 

utility <- utility_data %>% select(alt_cost,equi_cost,ego_cost,alt_output,equi_output,ego_output,ben_c,ben_cost) %>%
  mutate(alt_utility = alt_output-alt_cost) %>%
  mutate(equi_utility = equi_output-equi_cost) %>%
  mutate(ego_utility = ego_output-ego_cost) %>%
  mutate(con_utility = ben_c-ben_cost) %>%
  select(alt_utility,equi_utility,ego_utility,con_utility)

mean <- c(mean(utility$alt_utility),mean(utility$equi_utility),mean(utility$ego_utility),mean(utility$con_utility))
standard_deviation <- c(sd(utility$alt_utility),sd(utility$equi_utility),sd(utility$ego_utility))
median <- c(median(utility$alt_utility),median(utility$equi_utility),median(utility$ego_utility))
third_quartile <- c(quantile(utility$alt_utility,0.75),quantile(utility$equi_utility,0.75),quantile(utility$ego_utility,0.75))
first_quartile <- c(quantile(utility$alt_utility,0.25),quantile(utility$equi_utility,0.25),quantile(utility$ego_utility,0.25))


uti_mean <- data.frame(Treatment = c('Altruistic','Equitable','Egoistic', 'Control'), mean)
ggplot(uti_mean,aes(x=reorder(Treatment,-mean), y=mean)) + geom_bar(width=0.5,stat = 'identity') +
  labs(y='Average Utility', x= 'Treatment Condition',title='Plot of Avg. Utility for different Treatment conditions') +
  ggsave("./plots/stats_p/plot_ate.png", width = 8, height = 5) 
  
utility_stats <- rbind(mean,standard_deviation,median,third_quartile,first_quartile)

max <- c(0.4754024,0.1458753,0.2185998)
min <- c(0.4142375,0.1062017,0.1721807)
mid <- c(0.4448199,0.1260385,0.1953903)
con_plot <- data.frame(Treatment = c('Altruistic','Equitable','Egoistic'), mid, min, max)
ggplot(con_plot,aes(x=reorder(Treatment,-mid), y=mid)) + geom_bar(width=0.5,stat = 'identity') +
  geom_errorbar(mapping=aes(x=reorder(Treatment,-mid), ymin=max, ymax=min),width=0.2, size=1) + 
  labs(y='Marginal Utility', x= 'Treatment Condition',title='Plot of Marginal Utility for different Treatment conditions') +
  ggsave("./plots/stats_p/plot_conf.png", width = 8, height = 5) 


t.test(utility$alt_utility,utility$con_utility, paired = TRUE)

utility <- melt(utility)
utility <- utility %>% mutate(Treatment_type = ifelse(
  variable=='alt_utility','Altruistic',ifelse(
    variable=='equi_utility','Equitable','Egoistic')))
ggplot(utility, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Utility in $', title='Boxplot of Utility') +
  ggsave("./plots/stats/plot_utility.png", width = 8, height = 5) 

benefit <- utility_data %>% select(alt_output,equi_output,ego_output) %>%
  filter(alt_output < quantile(utility_data$alt_output,0.95)) %>%
  #filter(alt_output > quantile(utility_data$alt_output,0.01)) %>%
  filter(equi_output < quantile(utility_data$equi_output,0.95)) %>%
  #filter(equi_output > quantile(utility_data$equi_output,0.01)) %>%
  filter(ego_output < quantile(utility_data$ego_output,0.95)) #%>%
  #filter(ego_output > quantile(utility_data$ego_output,0.01))  
  
benefit <- melt(benefit)
benefit <- benefit %>% mutate(Treatment_type = ifelse(
  variable=='alt_output','Altruistic',ifelse(
    variable=='equi_output','Equitable','Egoistic')))
ggplot(benefit, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Benefit in $', title='Boxplot of Benefit') +
  ggsave("./plots/stats_p/plot_benefit.png", width = 8, height = 5) 


cost <- utility_data %>% select(alt_cost,equi_cost,ego_cost) %>%
  filter(alt_cost < quantile(utility_data$alt_cost,0.99)) %>%
  filter(alt_cost > quantile(utility_data$alt_cost,0.01)) %>%
  filter(equi_cost < quantile(utility_data$equi_cost,0.99)) %>%
  filter(equi_cost > quantile(utility_data$equi_cost,0.01)) %>%
  filter(ego_cost < quantile(utility_data$ego_cost,0.99)) %>%
  filter(ego_cost > quantile(utility_data$ego_cost,0.01))
cost <- melt(cost)
cost <- cost %>% mutate(Treatment_type = ifelse(
  variable=='alt_cost','Altruistic',ifelse(
    variable=='equi_cost','Equitable','Egoistic')))
ggplot(cost, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Cost in $', title='Boxplot of Cost') +
  ggsave("./plots/stats_p/plot_cost.png", width = 8, height = 5) 


utility <- utility_data %>% select(alt_cost,equi_cost,ego_cost,alt_output,equi_output,ego_output) %>%
  mutate(alt_utility = alt_output-alt_cost) %>%
  mutate(ego_utility = ego_output-ego_cost) %>%
  mutate(equi_utility = equi_output-equi_cost) %>%
  select(alt_utility,ego_utility,equi_utility)

utility <- utility %>%
  filter(alt_utility < quantile(utility$alt_utility,0.99)) %>%
  filter(alt_utility > quantile(utility$alt_utility,0.01)) %>%
  filter(equi_utility < quantile(utility$equi_utility,0.99)) %>%
  filter(equi_utility > quantile(utility$equi_utility,0.01)) %>%
  filter(ego_utility < quantile(utility$ego_utility,0.99)) %>%
  filter(ego_utility > quantile(utility$ego_utility,0.01))


utility <- melt(utility)
utility <- utility %>% mutate(Treatment_type = ifelse(
  variable=='alt_utility','Altruistic',ifelse(
    variable=='equi_utility','Equitable','Egoistic')))
ggplot(utility, aes(x=Treatment_type,y=value, color=Treatment_type)) + geom_boxplot() +
  labs(y='Utility in $', title='Boxplot of Utility') +
  ggsave("./plots/stats_p/plot_utility.png", width = 8, height = 5) 


stats <- rbind(ben_stats, cost_stats, utility_stats)


write.csv(stats, './files/summary_stats.csv')
