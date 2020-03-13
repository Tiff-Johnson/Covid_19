# Creating figure, maps and tables for the ebola and coronavirus disease assignment
# Author: Micaela Williams
# Date: 28 Febraury 2020
###############################################################################################################

# load packages
library(tidyverse) # package activates make of the R functions
library(ggpubr) # provides some easy-to-use functions for creating and customizing “ggplot2” based publication ready plots.
library(tidyr)
library(here)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(readr)
library(maps)

# load in data 
ebola <- read_csv("Ebola/Ebola.csv") # load in data that is in a csv file within the folder 
ebola_rate <- read_csv("Ebola/Ebola_total.csv")
confirmed_cases <- read_csv("COVID_19-master/time_series_19-covid-Confirmed.csv")
Deaths <- read_csv("csse_covid_19_series/time_series_19-covid-Deaths.csv")

#explore data
head(ebola) # shows the first six rows in the dataset
head(ebola,10)  # shows the first ten  in the dataset
tail(ebola) #  last six rows in the dataset
colnames(ebola) # displaying the variable names of each column 
summary(ebola) # displays a summary of the data such as mean, median,1st and 3rd quartile for each variable
dim(ebola) #shows the dimensions of the data (observations and variables)
nrow(ebola) # displays the amount of rows in the datatset
ncol(ebola) # displays the amount of columns in the dataset
glimpse(ebola) # views the dasaset in the console
str(ebola) # shows the structre of the data, and shows the type of data presented in each variable

# Spread of disease as a function of time and country

ebola <- ebola %>% 
  separate(col = Date, into = c("year", "month", "day"), sep = "-") %>% # splits the date column with three variables into three separate variable for year, month and date
  arrange(year)  # arranges the year in chronalogical order

# graph 
ebol_spread <- ebola %>% 
  ggplot(aes(x = year, y = Cases, fill = Country))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = c(0,2000,4000,6000,8000,10000,12000,14000,16000)) + # shows the intervals for the graph
  theme_bw() + # adding a border around the plot
  labs(title = "Cases of the Ebola virus",
       subtitle = "catergorised per country",
       x = "Years", # applying heading, subtitle, and the x and y axis titles
       y = "Cases") + 
  theme_pubclean() #theme_pubclean creates a clean theme without axis lines, to direct more attention to the data
ebol_spread # view the image

# infection rate per country for COVID-19
# tidying the dataset
confirmed_cases_long <- confirmed_cases %>% 
  gather(5:51, key = "date", value = "cases")

confirmed_cases_long_rename <- confirmed_cases_long %>% 
  rename(country_region = "Country/Region")

date_country_cases <- confirmed_cases_long_rename %>% 
  select(2,5,6) %>% 
  group_by(date, country_region) %>% 
  summarise(total_cases = sum(cases))

date_country_cases <- date_country_cases %>% 
  arrange(total_cases)

# plotting the graph
covid_spread <- ggplot(data = date_country_cases, aes(x = date, y = total_cases, colour = country_region)) +
  geom_point() +
  geom_line(aes(group = country_region))+
  xlim("1/22/20", "2/5/20", "2/19/20", "2/26/20", "3/8/20")+
  theme_pubr()+
  theme(legend.title = element_text(color = "blue", size = 9),
        legend.text = element_text(color = "red", size = 8),
        legend.position = "top right") +
  labs(title = "Spread of COVID-19",
       subtitle = "Globally",
       x = "Dates", # applying heading, subtitle, and the x and y axis titles
       y = "Cases")
covid_spread # view the graph

spread <- ggarrange(ebol_spread, covid_spread, ncol = 2, labels = "AUTO") #places all plots in one plotting pane
spread # view plots

# death cases for ebola 
# graph 
ebol_death <- ebola %>% 
  ggplot(aes(x = Cases, y = Deaths, colour = Country))+
  geom_point(stat = "identity", position = "identity") +
  geom_line(aes(group = Country))+
  theme_bw() + # adding a border around the plot
  labs(title = "Deaths of the Ebola virus",
       subtitle = "catergorised per country",
       x = "Cases", # applying heading, subtitle, and the x and y axis titles
       y = "Deaths") + 
  theme_pubclean() #theme_pubclean creates a clean theme without axis lines, to direct more attention to the data
ebol_death # view the image

# deaths per cases for COVID-19

# getting the rate
a <- (115855) # sum of cases 
rate <- c(555/a, 653/a, 941/a, 1434/a, 2118/a, 2927/a, 5578/a, 6166/a, 8234/a, 9927/a, 12038/a, 16787/a, 19881/a, 23892/a, 27636/a, 30818/a, 34392/a, 37121/a, 40151/a, 42763/a, 44803/a, 45222/a, 60370/a, 66887/a, 69032/a, 71226/a, 73260/a, 75138/a, 75641/a, 76199/a, 76843/a, 78599/a, 78985/a, 79570/a, 80415/a, 81397/a, 82756/a, 84122/a, 86013/a, 88371/a, 90309/a, 92843/a, 95123/a, 97885/a, 101799/a, 105835/a, 109836/a)

d_c_r <- data.frame(confirmed_cases_long_rename, rate) # dataset creates with two sperate dataframes

d_c_r <- d_c_r %>% 
  select(5,6) %>% # select two columns only
  group_by(date) %>% # group the dataset by date
  summarise(cases = sum(cases)) # get the total (sum) of COVID-19 cases

d_c_r <- d_c_r %>% 
  arrange(cases) # arranges the cases in chronological order 

deaths <- Deaths_sum_rate %>%  
  arrange(total_deaths) # arrange the total cases in chronoligical order 

cases <- d_c_r %>% 
  select(2) # select the cases column 

deaths <- deaths %>% 
  select(2) # select the deaths column

covid_cases_deaths <- data.frame(cases, deaths) # creating a dataset with cases and deaths of COVID-19

# plot the graph 
covid_deaths <- ggplot(data = covid_cases_deaths, aes(x = cases, y = total_deaths)) +
  geom_point(stat = "identity", position = "identity",colour = "purple", size = 0.9) +
  geom_smooth(method = "lm", size = 1, colour= "black")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Deaths per Case of COVID-19",
       x = "Cases",
       y = "Deaths")
covid_deaths # view the graph

disease_deaths <- ggarrange(ebol_death, covid_deaths, ncol = 2, labels = "AUTO") #places all plots in one plotting pane
disease_deaths # view plots

# figures showing the rate of infections for the diseases

# plotting graph for Ebola virus
infrate_ebola <- ebola %>% 
  ggplot(aes(x = year, y = Cases))+
  geom_bar(stat = "identity", position = "identity", fill = "cyan", colour = "cyan") +
  theme_bw() + # adding a border around the plot
  labs(title = "Infection rate of the Ebola virus",
       subtitle = "West Africa",
       x = "Year", # applying heading, subtitle, and the x and y axis titles
       y = "Rate of infection") + 
  theme_pubclean() #theme_pubclean creates a clean theme without axis lines, to direct more attention to the data
infrate_ebola # view the image

# infection rate  of COVID-19 (the totals are seen after the "#" in the annotation)

sum(confirmed_cases$"1/22/20",na.rm=TRUE) %>% #555  # getting the sum of each date (per column) for the confirmed cases
  sum(confirmed_cases$"1/23/20",na.rm=TRUE) %>% #653
  sum(confirmed_cases$"1/24/20",na.rm=TRUE) %>% #941
  sum(confirmed_cases$"1/25/20",na.rm=TRUE) %>% # 1434
  sum(confirmed_cases$"1/26/20",na.rm=TRUE) %>% # 2118
  sum(confirmed_cases$"1/27/20",na.rm=TRUE) %>% # 2927
  sum(confirmed_cases$"1/28/20",na.rm=TRUE) %>% # 557
  sum(confirmed_cases$"1/29/20",na.rm=TRUE) %>% # 6166
  sum(confirmed_cases$"1/30/20",na.rm=TRUE) %>% # 8234
  sum(confirmed_cases$"1/31/20",na.rm=TRUE) %>% # 9927
  sum(confirmed_cases$"2/1/20",na.rm=TRUE) %>% # 12038
  sum(confirmed_cases$"2/2/20",na.rm=TRUE) %>% # 1678
  sum(confirmed_cases$"2/3/20",na.rm=TRUE) %>% # 19881
  sum(confirmed_cases$"2/4/20",na.rm=TRUE) %>% # 23892
  sum(confirmed_cases$"2/5/20",na.rm=TRUE) %>% # 27636
  sum(confirmed_cases$"2/6/20",na.rm=TRUE) %>% # 30818
  sum(confirmed_cases$"2/7/20",na.rm=TRUE) %>% # 34392
  sum(confirmed_cases$"2/8/20",na.rm=TRUE) %>% # 37121
  sum(confirmed_cases$"2/9/20",na.rm=TRUE) %>% # 40151
  sum(confirmed_cases$"2/10/20",na.rm=TRUE) %>% # 42763
  sum(confirmed_cases$"2/11/20",na.rm=TRUE) %>% # 44803
  sum(confirmed_cases$"2/12/20",na.rm=TRUE) %>% # 45222
  sum(confirmed_cases$"2/13/20",na.rm=TRUE) %>% # 60370
  sum(confirmed_cases$"2/14/20",na.rm=TRUE) %>% # 66887
  sum(confirmed_cases$"2/15/20",na.rm=TRUE) %>% # 69032
  sum(confirmed_cases$"2/16/20",na.rm=TRUE) %>% # 71226
  sum(confirmed_cases$"2/17/20",na.rm=TRUE) %>% # 73260
  sum(confirmed_cases$"2/18/20",na.rm=TRUE) %>% # 75138
  sum(confirmed_cases$"2/19/20",na.rm=TRUE) %>% # 75641
  sum(confirmed_cases$"2/20/20",na.rm=TRUE) %>% # 76199
  sum(confirmed_cases$"2/21/20",na.rm=TRUE) %>% # 76843
  sum(confirmed_cases$"2/22/20",na.rm=TRUE) %>% # 78599
  sum(confirmed_cases$"2/23/20",na.rm=TRUE) %>% # 78985
  sum(confirmed_cases$"2/24/20",na.rm=TRUE) %>% # 79570
  sum(confirmed_cases$"2/25/20",na.rm=TRUE) %>% # 80415
  sum(confirmed_cases$"2/26/20",na.rm=TRUE) %>% # 81397
  sum(confirmed_cases$"2/27/20",na.rm=TRUE) %>% # 82756
  sum(confirmed_cases$"2/28/20",na.rm=TRUE) %>% # 84122
  sum(confirmed_cases$"2/29/20",na.rm=TRUE) %>% # 86013
  sum(confirmed_cases$"3/1/20",na.rm=TRUE) %>% # 88371
  sum(confirmed_cases$"3/2/20",na.rm=TRUE) %>% # 90309
  sum(confirmed_cases$"3/3/20",na.rm=TRUE) %>% # 92843
  sum(confirmed_cases$"3/4/20",na.rm=TRUE) %>% # 95123
  sum(confirmed_cases$"3/5/20",na.rm=TRUE) %>% # 97885
  sum(confirmed_cases$"3/6/20",na.rm=TRUE) %>% # 101799
  sum(confirmed_cases$"3/7/20",na.rm=TRUE) %>% # 105835
  sum(confirmed_cases$"3/8/20",na.rm=TRUE) # 109836

date <- d_c_r %>% # extracting the date from the rate_date dataset
  select(1)
rate_date <- data.frame(date, rate) # creating a dataset with the date and rate of cases 

# plot the graph of COVID-19 rate globally 
covid_infec_rate <- ggplot(data = rate_date, aes(x = date, y = rate)) +
  geom_line(aes(group = 1), colour = "brown")+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Infection rate of COVID-19",
       x = "Date",
       y = "Rate of infection")
covid_infec_rate # view the plot

dis_infrate <- ggarrange(infrate_ebola, covid_infec_rate, ncol = 2, labels = "AUTO") #places all plots in one plotting pane
dis_infrate # view plots

# death rate of of ebola virus

# plotting graph
drate_ebola <- ebola %>% 
  ggplot(aes(x = year, y = Deaths))+
  geom_bar(stat = "identity", position = "identity",colour = "purple", fill = "purple") +
  theme_bw() + # adding a border around the plot
  labs(title = "Death rate of the Ebola virus",
       subtitle = "West Africa",
       x = "Years", # applying heading, subtitle, and the x and y axis titles
       y = "Rate of death") + 
  theme_pubclean() #theme_pubclean creates a clean theme without axis lines, to direct more attention to the data
drate_ebola # view the image

# death rate of COVID-19

Deaths_sum_rate <- Deaths_long %>% 
  group_by(date) %>% # group the dataset by the date
  summarise(total_deaths = sum(deaths)) # get the sum (total) of the deaths

b <- (3803) # value of the sum of death for COVID-19

rate_deaths <- c(17/b, 18/b, 26/b, 42/b, 56/b, 82/b, 131/b, 133/b, 171/b, 213/b, 259/b, 362/b, 426/b, 492/b, 564/b, 634/b, 719/b, 806/b, 906/b, 1013/b, 1113/b, 1118/b, 1371/b, 1523/b, 1666/b, 1770/b, 1868/b, 2007/b, 2122/b, 2247/b, 2251/b, 2458/b, 2469/b, 2629/b, 2708/b, 2770/b, 2814/b, 2872/b, 2941/b, 2996/b, 3085/b, 3160/b, 3254/b, 3348/b, 3460/b, 3558/b, 3803/b)

covid_rdeaths_date <- data.frame(date, rate_deaths) # creating a dataframe which includes the date and the rate of deaths 

covid_rdeath <- ggplot(data = covid_rdeaths_date, aes(x = date, y = rate_deaths)) +
  geom_line(aes(group = 1), colour = "blue")+
  xlim("1/22/20", "2/5/20", "2/10/20", "2/15/20", "2/29/20", "3/8/20")+
  theme_bw()+
  theme_pubclean()+
  labs(title = "Death rate of COVID-19",
       x = "Date",
       y = "Rate of death")
covid_rdeath # view the graph

dis_rdeath <- ggarrange(drate_ebola, covid_rdeath, ncol = 2, labels = "AUTO") #places all plots in one plotting pane
dis_rdeath # view the image

# distribution map of COVID-19

library(remotes) 
remotes::install_github("GuangchuangYu/nCov2019")
3
library(nCov2019)

x <- get_nCov2019(lang='en')

require(nCov2019)

x = get_nCov2019(lang='en')
plot(x)

# age table for COVID-19

age <- c("80+ years old", "70-79 years old", "60-69 years old", "50-59 yearsold", "40-49 years old", "30-39 years old", "20-29 years old", "10-19years old", "0-9 years old")
death_rate_all_cases <- c("14.8%", "8.0%", "3.6%", "1.3%", "0.4%", "0.2%", "0.2%", "0.2%", "no fatalities")

covid_age_death <- data.frame(age,death_rate_all_cases) # creating a dataset of the ages and death rates

write.csv(covid_age_death, file="C:/Users/User/Desktop/Honours 2020 biostats/Intro_R_2020/Micaela_Williams/covid_age.csv")

# age table for Ebola outbreak

age_bracket <- c("61 + years old", "35-60 years old", "18-34 years old", "5-17 years old", "0-5 years old")
case_fatality <- c("25.0%", "23.7%", "22.8%", "21.3%", "27.0%")

ebola_age_death <- data.frame(age_bracket,case_fatality) # creating a dataset of the ages and death rates

write.csv(ebola_age_death, file="C:/Users/User/Desktop/Honours 2020 biostats/Intro_R_2020/Micaela_Williams/ebola_age.csv")

citation() # R
citation(package = "tidyverse")
citation(package = "ggpubr")