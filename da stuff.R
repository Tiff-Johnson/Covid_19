library(tidyverse)
library(ggplot)
library(tidyr)
library(here)
library(dplyr)

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

sum(555+653+941+1434+2118+2927+5578+6166+8234+9927+12038+16787+19881+23892+27636+30818+34392+37121+40151+42763+44803+45222+60370+66887+69032+71226+73260+75138+75641+76199+76843+78599+78985+79570+80415+81397+82756+84122+86013+88371+90309+92843+95123+97885+101799+105835+109836)
# 2492491

rate <- (555/2492491)

trial1 <- confirmed_cases %>% 
  filter(c(5 - 51))

trial2 <- confirmed_cases %>%
  gather(c(5 - 51, key = "date", value = "rate"))

confirmed_cases2 <- mutate(confirmed_cases)































