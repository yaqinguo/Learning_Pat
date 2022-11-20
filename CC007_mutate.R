library(tidyverse)
library(lubridate)

aa_weather <- read_csv("https://github.com/riffomonas/generalR_data/blob/master/noaa/USC00200230.csv?raw=true",
                       col_type=cols(TOBS = col_double())) %>%
  select(DATE, TMAX, TMIN, TOBS) %>%
  mutate(year = year(DATE),
         month=month(DATE),
         day = day(DATE))
aa_weather
#bouns about logical data
my_vector <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
my_vector
as.numeric(my_vector) 
sum(my_vector) #how many true
mean(my_vector) #how much percent of true in my data
sum(my_vector)/length(my_vector)
#1. Determine whether a low temperature for each day was below freezing
#2. Aggregate the data by month and day
#3. Determine the fraction of years that a month/day paring had a frost
#4. Find the day (in May) where there 's below a 5 or 10% risk of another frost
aa_weather %>%
  mutate(below_freezing = TMIN < 0) %>%
  group_by(month,day) %>%
  summarise(frac_below_freezing = mean(below_freezing,na.rm = T)*100) %>%
  filter(month == 5) %>%
  print(n=31)

#1
aa_weather %>%
  mutate(below_freezing = TMIN < 0) %>%
  group_by(month,day) %>%
  summarise(frac_below_freezing = mean(below_freezing,na.rm = T)*100) %>%
  filter(month == 10) %>%
  print(n=31)
#2
aa_weather %>%
  mutate(F_T = 9/5 * TMAX + 32,high_temperature = F_T > 90) %>%
  group_by(year) %>%
  summarise(total_hot_days = sum(high_temperature,na.rm = T)) %>%
  filter(year > 1891 & year < 2020) %>%
  summarise(ave_t_higher = mean(total_hot_days,na.rm = T))
# 3
aa_weather %>%
  mutate(F_high_temperature = 9/5 * TMAX + 32, F_low_temperature = 9/5 * TMIN + 32,
         grass_growing = F_high_temperature + F_low_temperature > 100) %>%
  group_by(month,day) %>%
  summarise(frac_growing_days = mean(grass_growing,na.rm = T)) %>%
  filter(month == 5) %>%
  print(n=31)

  
  