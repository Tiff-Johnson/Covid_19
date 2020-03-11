library(tidyverse)
library(ggplot2)
library(tidyr)
library(here)
library(dplyr)
library(ggthemes)
library(ggpubr)

confirmed_cases <- time_series_19_covid_Confirmed

sum(confirmed_cases$"1/22/20",na.rm=TRUE)
# 555

sum(confirmed_cases$"1/23/20",na.rm=TRUE)
#653

sum(confirmed_cases$"1/24/20",na.rm=TRUE)
#941

sum(confirmed_cases$"1/25/20",na.rm=TRUE)
# 1434

sum(confirmed_cases$"1/26/20",na.rm=TRUE)
# 2118

sum(confirmed_cases$"1/27/20",na.rm=TRUE)
# 2927

sum(confirmed_cases$"1/28/20",na.rm=TRUE)
# 5578

sum(confirmed_cases$"1/29/20",na.rm=TRUE)
# 6166

sum(confirmed_cases$"1/30/20",na.rm=TRUE)
# 8234

sum(confirmed_cases$"1/31/20",na.rm=TRUE)
# 9927

sum(confirmed_cases$"2/1/20",na.rm=TRUE)
# 12038

sum(confirmed_cases$"2/2/20",na.rm=TRUE)
# 16787

sum(confirmed_cases$"2/3/20",na.rm=TRUE)
# 19881

sum(confirmed_cases$"2/4/20",na.rm=TRUE)
# 23892

sum(confirmed_cases$"2/5/20",na.rm=TRUE)
# 27636

sum(confirmed_cases$"2/6/20",na.rm=TRUE)
# 30818

sum(confirmed_cases$"2/7/20",na.rm=TRUE)
# 34392

sum(confirmed_cases$"2/8/20",na.rm=TRUE)
# 37121

sum(confirmed_cases$"2/9/20",na.rm=TRUE)
# 40151

sum(confirmed_cases$"2/10/20",na.rm=TRUE)
# 42763

sum(confirmed_cases$"2/11/20",na.rm=TRUE)
# 44803

sum(confirmed_cases$"2/12/20",na.rm=TRUE)
# 45222

sum(confirmed_cases$"2/13/20",na.rm=TRUE)
# 60370

sum(confirmed_cases$"2/14/20",na.rm=TRUE)
# 66887

sum(confirmed_cases$"2/15/20",na.rm=TRUE)
# 69032

sum(confirmed_cases$"2/16/20",na.rm=TRUE)
# 71226

sum(confirmed_cases$"2/17/20",na.rm=TRUE)
# 73260

sum(confirmed_cases$"2/18/20",na.rm=TRUE)
# 75138

sum(confirmed_cases$"2/19/20",na.rm=TRUE)
# 75641

sum(confirmed_cases$"2/20/20",na.rm=TRUE)
# 76199

sum(confirmed_cases$"2/21/20",na.rm=TRUE)
# 76843

sum(confirmed_cases$"2/22/20",na.rm=TRUE)
# 78599

sum(confirmed_cases$"2/23/20",na.rm=TRUE)
# 78985

sum(confirmed_cases$"2/24/20",na.rm=TRUE)
# 79570

sum(confirmed_cases$"2/25/20",na.rm=TRUE)
# 80415

sum(confirmed_cases$"2/26/20",na.rm=TRUE)
# 81397

sum(confirmed_cases$"2/27/20",na.rm=TRUE)
# 82756

sum(confirmed_cases$"2/28/20",na.rm=TRUE)
# 84122

sum(confirmed_cases$"2/29/20",na.rm=TRUE)
# 86013

sum(confirmed_cases$"3/1/20",na.rm=TRUE)
# 88371

sum(confirmed_cases$"3/2/20",na.rm=TRUE)
# 90309

sum(confirmed_cases$"3/3/20",na.rm=TRUE)
# 92843

sum(confirmed_cases$"3/4/20",na.rm=TRUE)
# 95123

sum(confirmed_cases$"3/5/20",na.rm=TRUE)
# 97885

sum(confirmed_cases$"3/6/20",na.rm=TRUE)
# 101799

sum(confirmed_cases$"3/7/20",na.rm=TRUE)
# 105835

sum(confirmed_cases$"3/8/20",na.rm=TRUE)
# 109836

a <- (115855)

trial3 <- (555/a)

rate <- c(555/a, 653/a, 941/a, 1434/a, 2118/a, 2927/a, 5578/a, 6166/a, 8234/a, 9927/a, 12038/a, 16787/a, 19881/a, 23892/a, 27636/a, 30818/a, 34392/a, 37121/a, 40151/a, 42763/a, 44803/a, 45222/a, 60370/a, 66887/a, 69032/a, 71226/a, 73260/a, 75138/a, 75641/a, 76199/a, 76843/a, 78599/a, 78985/a, 79570/a, 80415/a, 81397/a, 82756/a, 84122/a, 86013/a, 88371/a, 90309/a, 92843/a, 95123/a, 97885/a, 101799/a, 105835/a, 109836/a)

rate_date <- data.frame(date, rate)

rate_date_sep <- rate_date %>% 
  separate(col = date, into = c("day", "month", "year"), sep = "-")

trial4 <- confirmed_cases %>% 
  gather(5:51, key = "date", value = "cases")

trial4a <- trial4 %>% 
  rename(country_region = "Country/Region")

trial5 <- trial4a %>% 
  select(2,5,6) %>% 
  group_by(date, country_region) %>% 
  summarise(total_cases = sum(cases))


trial5 <- trial5 %>% 
  arrange(total_cases)

d_c_r <- data.frame(trial5, rate)



trial7 <- trial6 %>% 
  group_by(month) %>% 
  summarise(avg_rate = mean(rate))

ggplot(data=d_c_r, aes(x=date, y=rate)) +
  geom_point()+
  geom_smooth(method = "lm")
theme_pubr()
labs(x = "month", y = "rate")


ggplot(data = d_c_r , aes(x = month, y = rate )) +
  geom_path(aes(group = total_cases)) +
  labs(x = "month", y = "rate")



ggplot(data = d_c_r, aes(x = date)) +
  geom_density(colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Infection Rate",
       x = "Date",
       y = "Rate") + theme_pubr()



ggplot(data = trial7, aes(x = month, y = avg_rate)) + #inputting chick weight data
  geom_point() +  # creating point graph
  geom_smooth(method = "lm") # linking this with a line for each chick


ggplot(data = d_c_r, aes(x = date, y = total_cases)) +
  geom_line(aes(group = 1))+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme_pubr()+
  
 group_country <- trial4a %>% 
  group_by(country_region) %>% 
  summarise(sum_cases = sum(cases))
  
  ggplot(data = d_c_r, aes(x = date, y = rate)) +
  geom_line(aes(group = 1))+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20") +
  ylim(0.003, 1)+
  theme_pubr()+
  labs(title = "Infection Rate",
       x = "Date",
       y = "Rate (total cases per day/total cases)")

  ggplot(data = trial5, aes(x = date, y = total_cases, group = country_region)) +
    geom_point() +
    geom_line(aes(group = 1))+
    theme(legend.position = "none")+
    xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")


























