library(tidyverse)
library(lubridate)

aa_weather <- read_csv("https://github.com/riffomonas/generalR_data/blob/master/noaa/USC00200230.csv?raw=true",
                       col_type=cols(TOBS = col_double())) %>%
  select(DATE, TMAX, TMIN, TOBS) %>%
  mutate(year = year(DATE),
         month=month(DATE),
         day = day(DATE))
may_nineth <- aa_weather %>% filter(month == 5 & day == 9)
may_nineth %>% 
  summarise(ave_high=mean(TMAX),
            lci_high=quantile(TMAX,prob=0.025),
            uci_high=quantile(TMAX,prob=0.975),
            ave_low=mean(TMIN),
            ave_obs=mean(TOBS),
            lci_low=quantile(TMIN,probs = 0.025),
            uci_low=quantile(TMIN,probs = 0.975),
            n=n())
may_nineth %>% summarise(historic_low=min(TMIN))
may_nineth %>% summarise(historic_high=max(TMAX))
may_nineth %>% arrange(TMIN)
may_nineth %>% arrange(desc(TMAX))
may_nineth %>% top_n(TMAX,n=3)
may_nineth %>% top_n(TMAX,n=-3) %>%  #negative number is the last 
  arrange(TMAX)


aa_weather %>% 
  #filter(month == 6 & day == 29) %>%
  group_by(month,day) %>%
  summarise(ave_high=mean(TMAX),
            lci_high=quantile(TMAX,prob=0.025,na.rm = T),
            uci_high=quantile(TMAX,prob=0.975,na.rm = T),
            ave_low=mean(TMIN),
            ave_obs=mean(TOBS),
            lci_low=quantile(TMIN,probs = 0.025,na.rm = T),
            uci_low=quantile(TMIN,probs = 0.975,na.rm = T),
            n=n())

# 1
aa_weather %>% 
  filter(month == 6 & day == 29) %>%
  top_n(TMAX,n=1)

aa_weather %>% 
  filter(year >=1991 & month ==6 & day == 29) %>%
  top_n(TMAX,n=1)

# 2
aa_weather %>%
  filter(year==1991) %>%
  summarise(hottest=max(TMAX),
            coldest=min(TMAX))
# 3
aa_weather %>%
  filter(year >= 1892 & year <= 2019) %>%
  group_by(year) %>%
  summarise(ave_T=mean(TMAX)) %>%
  filter(year==1991)

aa_weather %>%
  filter(year >= 1892 & year <= 2019) %>%
  group_by(year) %>%
  summarise(ave_T=mean(TMAX)) %>%
  top_n(ave_T,n=1)
 
aa_weather %>%
  filter(year >= 1892 & year <= 2019) %>%
  group_by(year) %>%
  summarise(ave_T=mean(TMAX)) %>%
  top_n(ave_T,n=-1)  

# 4
month_T <- aa_weather %>%
  group_by(year,month) %>%
  summarise(ave_T=mean(TMAX),
            lci_high=quantile(TMAX,prob=0.25,na.rm = T),
            uci_high=quantile(TMAX,prob=0.95,na.rm = T)) 

month_T %>% filter(year==1991)

TukeyHSD(Observed.anova)
plot(TukeyHSD(Observed.anova))

