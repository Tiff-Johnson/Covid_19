library(tidyverse)
library(ggplot2)
library(tidyr)
library(here)
library(dplyr)
library(plyr)

confirmed_cases <- time_series_19_covid_Confirmed  

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

a <- (109836)

trial3 <- (555/a)

rate <- c(555/a, 653/a, 941/a, 1434/a, 2118/a, 2927/a, 5578/a, 6166/a, 8234/a, 9927/a, 12038/a, 16787/a, 19881/a, 23892/a, 27636/a, 30818/a, 34392/a, 37121/a, 40151/a, 42763/a, 44803/a, 45222/a, 60370/a, 66887/a, 69032/a, 71226/a, 73260/a, 75138/a, 75641/a, 76199/a, 76843/a, 78599/a, 78985/a, 79570/a, 80415/a, 81397/a, 82756/a, 84122/a, 86013/a, 88371/a, 90309/a, 92843/a, 95123/a, 97885/a, 101799/a, 105835/a, 109836/a)
date <- (c("22-1-20", "23-1-20", "24-1-20", "25-1-20", "26-1-20", "27-1-20", "28-1-20", "29-1-20", "30-1-20", "31-1-20", "1-2-20", "2-2-20", "3-2-20", "4-2-20", "5-2-20", "6-2-20", "7-2-20", "8-2-20", "9-2-20", "10-2-20", "11-2-20", "12-2-20", "13-2-20", "14-2-20", "15-2-20", "16-2-20", "17-2-20", "18-2-20", "19-2-20", "20-2-20", "21-2-20", "22-2-20", "23-2-20", "24-2-20", "25-2-20", "26-2-20", "27-2-20", "28-2-20", "29-2-20", "1-3-20", "2-3-20", "3-3-20", "4-3-20", "5-3-20", "6-3-20", "7-3-20", "8-3-20"))
  
rate_date <- data.frame(date, rate)
  

ggplot(data = rate_date, aes(x = date, y = rate)) +
  geom_line() +
  labs(x = "date", y = "rate")

ggplot(data = rate_date, aes(x = date, y = rate)) +
  geom_point()
  geom_line(aes(stat = "identity", position = "identity", colour = "purple", size = 0.5)) +
  labs(x = "date", y = "rate")







  























