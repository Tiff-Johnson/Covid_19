library(tidyverse)
library(ggplot2)
library(tidyr)
library(here)
library(dplyr)
library(ggthemes)
library(ggpubr)

confirmed_cases <- time_series_19_covid_Confirmed  



# infection rate per country

confirmed_cases_long <- confirmed_cases %>% 
  gather(5:51, key = "date", value = "cases")

confirmed_cases_long_rename <- confirmed_cases_long %>% 
  rename(country_region = "Country/Region")

date_country_cases <- confirmed_cases_long_rename %>% 
  select(2,5,6) %>% 
  group_by(date, country_region) %>% 
  summarise(total_cases = sum(cases))

date_country_cases <- date_country_case %>% 
  arrange(total_cases)



ggplot(data = date_country_cases, aes(x = date, y = total_cases, colour = country_region)) +
  geom_point() +
  geom_line(aes(group = country_region))+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme(legend.position = "none")+
  theme_pubr()+
  theme(legend.title = element_text(color = "blue", size = 9),
        legend.text = element_text(color = "red", size = 8),
        legend.position = "top right")

# Global infection rate

sum(confirmed_cases$"1/22/20",na.rm=TRUE) %>% 
  #555
  
  sum(confirmed_cases$"1/23/20",na.rm=TRUE) %>% 
  # 653
  
  sum(confirmed_cases$"1/24/20",na.rm=TRUE) %>% 
  #941
  
  sum(confirmed_cases$"1/25/20",na.rm=TRUE) %>% 
  # 1434
  
  sum(confirmed_cases$"1/26/20",na.rm=TRUE) %>% 
  # 2118
  
  sum(confirmed_cases$"1/27/20",na.rm=TRUE) %>% 
  # 2927
  
  sum(confirmed_cases$"1/28/20",na.rm=TRUE) %>% 
  # 5578
  
  sum(confirmed_cases$"1/29/20",na.rm=TRUE) %>% 
  # 6166
  
  sum(confirmed_cases$"1/30/20",na.rm=TRUE) %>% 
  # 8234
  
  sum(confirmed_cases$"1/31/20",na.rm=TRUE) %>% 
  # 9927
  
  sum(confirmed_cases$"2/1/20",na.rm=TRUE) %>% 
  # 12038
  
  sum(confirmed_cases$"2/2/20",na.rm=TRUE) %>% 
  # 16787
  
  sum(confirmed_cases$"2/3/20",na.rm=TRUE) %>% 
  # 19881
  
  sum(confirmed_cases$"2/4/20",na.rm=TRUE) %>% 
  # 23892
  
  sum(confirmed_cases$"2/5/20",na.rm=TRUE) %>% 
  # 27636
  
  sum(confirmed_cases$"2/6/20",na.rm=TRUE) %>% 
  # 30818
  
  sum(confirmed_cases$"2/7/20",na.rm=TRUE) %>% 
  # 34392
  
  sum(confirmed_cases$"2/8/20",na.rm=TRUE) %>% 
  # 37121
  
  sum(confirmed_cases$"2/9/20",na.rm=TRUE) %>% 
  # 40151
  
  sum(confirmed_cases$"2/10/20",na.rm=TRUE) %>% 
  # 42763
  
  sum(confirmed_cases$"2/11/20",na.rm=TRUE) %>% 
  # 44803
  
  sum(confirmed_cases$"2/12/20",na.rm=TRUE) %>% 
  # 45222
  
  sum(confirmed_cases$"2/13/20",na.rm=TRUE) %>% 
  # 60370
  
  sum(confirmed_cases$"2/14/20",na.rm=TRUE) %>% 
  # 66887
  
  sum(confirmed_cases$"2/15/20",na.rm=TRUE) %>% 
  # 69032
  
  sum(confirmed_cases$"2/16/20",na.rm=TRUE) %>% 
  # 71226
  
  sum(confirmed_cases$"2/17/20",na.rm=TRUE) %>% 
  # 73260
  
  sum(confirmed_cases$"2/18/20",na.rm=TRUE) %>% 
  # 75138
  
  sum(confirmed_cases$"2/19/20",na.rm=TRUE) %>% 
  # 75641
  
  sum(confirmed_cases$"2/20/20",na.rm=TRUE) %>% 
  # 76199
  
  sum(confirmed_cases$"2/21/20",na.rm=TRUE) %>% 
  # 76843
  
  sum(confirmed_cases$"2/22/20",na.rm=TRUE) %>% 
  # 78599
  
  sum(confirmed_cases$"2/23/20",na.rm=TRUE) %>% 
  # 78985
  
  sum(confirmed_cases$"2/24/20",na.rm=TRUE) %>% 
  # 79570
  
  sum(confirmed_cases$"2/25/20",na.rm=TRUE) %>% 
  # 80415
  
  sum(confirmed_cases$"2/26/20",na.rm=TRUE) %>% 
  # 81397
  
  sum(confirmed_cases$"2/27/20",na.rm=TRUE) %>% 
  # 82756
  
  sum(confirmed_cases$"2/28/20",na.rm=TRUE) %>% 
  # 84122
  
  sum(confirmed_cases$"2/29/20",na.rm=TRUE) %>% 
  # 86013
  
  sum(confirmed_cases$"3/1/20",na.rm=TRUE) %>% 
  # 88371
  
  sum(confirmed_cases$"3/2/20",na.rm=TRUE) %>% 
  # 90309
  
  sum(confirmed_cases$"3/3/20",na.rm=TRUE) %>% 
  # 92843
  
  sum(confirmed_cases$"3/4/20",na.rm=TRUE) %>% 
  # 95123
  
  sum(confirmed_cases$"3/5/20",na.rm=TRUE) %>% 
  # 97885
  
  sum(confirmed_cases$"3/6/20",na.rm=TRUE) %>% 
  # 101799
  
  sum(confirmed_cases$"3/7/20",na.rm=TRUE) %>% 
  # 105835
  
  sum(confirmed_cases$"3/8/20",na.rm=TRUE) 
# 109836

a <- (115855)

rate <- c(555/a, 653/a, 941/a, 1434/a, 2118/a, 2927/a, 5578/a, 6166/a, 8234/a, 9927/a, 12038/a, 16787/a, 19881/a, 23892/a, 27636/a, 30818/a, 34392/a, 37121/a, 40151/a, 42763/a, 44803/a, 45222/a, 60370/a, 66887/a, 69032/a, 71226/a, 73260/a, 75138/a, 75641/a, 76199/a, 76843/a, 78599/a, 78985/a, 79570/a, 80415/a, 81397/a, 82756/a, 84122/a, 86013/a, 88371/a, 90309/a, 92843/a, 95123/a, 97885/a, 101799/a, 105835/a, 109836/a)


date <- d_c_r %>% 
  select(1)


rate_date <- data.frame(date, rate)

ggplot(data = rate_date, aes(x = date, y = rate)) +
  geom_line(aes(group = 1))+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Global Infection Rate",
       x = "Date",
       y = "Rate (%)")


# confirmed cases per country


group_country <- confirmed_cases_long_rename %>% 
  group_by(country_region) %>% 
  summarise(sum_cases = sum(cases))


country1 <- group_country %>% 
  slice(1:18)

country2 <- group_country %>% 
  slice(18:35)

country3 <- group_country %>% 
  slice(16:53)

country4 <- group_country %>% 
  slice(54:71)

country5 <- group_country %>% 
  slice(71:89)

country6 <- group_country %>% 
  slice(90:108)



bar1 <- country1 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(title = "Total Confirmed Cases per Country",
       subtitle = "catergorised per country",
       x = "Country", 
       y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar1

bar2 <- country2 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(x = "Country",
       y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar2

bar3 <- country3 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(
    x = "Country", 
    y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar3

bar4 <- country4 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs (x = 
          y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar4


bar5 <- country5 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs( 
    x = "Country", 
    y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar5

bar6 <- country6 %>% 
  ggplot(aes(x = country_region, y = sum_cases))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(
    x = "Country", 
    y = "Confrimed Cases") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar6

ggarrange(bar1, bar2, bar3, bar4, bar5, bar6, 
          ncol = 3, nrow = 3, 
          common.legend = TRUE) 


# Deaths

# death rate per country

Deaths <- time_series_19_covid_Deaths

Deaths_long <- Deaths %>% 
  gather(5:51, key = "date", value = "deaths")

Deaths_long_rename <- Deaths_long %>% 
  rename(country_region = "Country/Region")

date_country_deaths <- Deaths_long_rename %>% 
  select(2,5,6) %>% 
  group_by(date, country_region) %>% 
  summarise(total_deaths = sum(deaths))

date_country_deaths <- date_country_deaths %>% 
  arrange(date)


ggplot(data = date_country_deaths, aes(x = date, y = total_deaths, colour = country_region)) +
  geom_point() +
  geom_line(aes(group = country_region))+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme(legend.position = "none")+
  theme_pubr()+
  theme(legend.title = element_text(color = "blue", size = 9),
        legend.text = element_text(color = "red", size = 8),
        legend.position = "top right")


# global death rate

Deaths_sum_rate <- Deaths_long %>% 
  group_by(date) %>% 
  summarise(total_deaths = sum(deaths)) 

b <- (3803)

rate_deaths <- c(17/b, 18/b, 26/b, 42/b, 56/b, 82/b, 131/b, 133/b, 171/b, 213/b, 259/b, 362/b, 426/b, 492/b, 564/b, 634/b, 719/b, 806/b, 906/b, 1013/b, 1113/b, 1118/b, 1371/b, 1523/b, 1666/b, 1770/b, 1868/b, 2007/b, 2122/b, 2247/b, 2251/b, 2458/b, 2469/b, 2629/b, 2708/b, 2770/b, 2814/b, 2872/b, 2941/b, 2996/b, 3085/b, 3160/b, 3254/b, 3348/b, 3460/b, 3558/b, 3803/b)

date <- rate_date %>% 
  select(1)

rate_deaths_date <- data.frame(date, rate_deaths)

ggplot(data = rate_deaths_date, aes(x = date, y = rate_deaths)) +
  geom_line(aes(group = 1))+
  xlim("22-1-20", "5-2-20", "10-2-20", "15-2-20", "29-2-20", "8-3-20")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Global Death Rate",
       x = "Date",
       y = "Rate (%)")




# Deaths per country


group_country_deaths <- Deaths_long_rename %>% 
  group_by(country_region) %>% 
  summarise(sum_deaths = sum(deaths))


country1d <- group_country_deaths %>% 
  slice(1:18)

country2d <- group_country_deaths %>% 
  slice(18:35)

country3d <- group_country_deaths %>% 
  slice(16:53)

country4d <- group_country_deaths %>% 
  slice(54:71)

country5d <- group_country_deaths %>% 
  slice(71:89)

country6d <- group_country_deaths %>% 
  slice(90:108)



bar1d <- country1d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(title = "Total Deaths per Country",
       x = "Country", 
       y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar1d

bar2d <- country2d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(x = "Country",
       y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar2d

bar3d <- country3d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(
    x = "Country", 
    y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar3d

bar4d <- country4d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs (x = "Country",
        y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar4d


bar5d <- country5d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs( 
    x = "Country", 
    y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar5d

bar6d <- country6d %>% 
  ggplot(aes(x = country_region, y = sum_deaths))+
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  theme_bw() + 
  labs(
    x = "Country", 
    y = "Deaths") +
  theme_pubclean()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

bar6d

ggarrange(bar1d, bar2d, bar3d, bar4d, bar5d, bar6d, 
          ncol = 3, nrow = 3, 
          common.legend = TRUE) 



# deaths per cases

d_c_r <- data.frame(confirmed_cases_long_rename, rate)

d_c_r2 <- d_c_r %>% 
  select(5,6) %>% 
  group_by(date) %>% 
  summarise(cases = sum(cases))


d_c_r3 <- d_c_r2 %>% 
  arrange(cases)

deaths1 <- Deaths_sum_rate %>% 
  arrange(total_deaths)

cases <- d_c_r3 %>% 
  select(2)

deaths <- deaths1 %>% 
  select(2)

cases_deaths <- data.frame(cases, deaths)



ggplot(data = cases_deaths, aes(x = cases, y = total_deaths)) +
  geom_point(stat = "identity", position = "identity",colour = "purple", size = 0.9) +
  geom_smooth(method = "lm", size = 1, colour= "black")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Deaths per Case",
       x = "Cases",
       y = "Deaths")

age <- c("80+ years old", "70-79 years old", "60-69 years old", "50-59 yearsold", "40-49 years old", "30-39 years old", "20-29 years old", "10-19years old", "0-9 years old")
death_rate_confirmed_cases <- c("21.9%")
death_rate_all_cases <- c("14.8%", "8.0%", "3.6%", "1.3%", "0.4%", "0.2%", "0.2%", "0.2%", "no fatalities")

death_rate_age <- data.frame(age, death_rate_confirmed_cases, death_rate_all_cases)

aget <- data.frame(age)
death_rate_confirmed_casest <- data.frame(death_rate_confirmed_cases)
death_rate_all_casest <- data.frame(death_rate_all_cases)

age_death <- merge(aget, death_rate_confirmed_casest, by = 0, all = TRUE, na.rm = TRUE) [-1] 

age_death <- merge(age_death, death_rate_all_casest,  by = 0, all = TRUE, na.rm = TRUE) [-1] 

# Age Graph

write.csv(age_death, file="C:/Users/PC/Documents/Covid_19/file.csv")








