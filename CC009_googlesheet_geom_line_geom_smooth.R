library(tidyverse)
library(googlesheets4)
library(ggplot2)
gs4_deauth() #we don't need to have google count
sheep_numbers <- read_sheet("https://docs.google.com/spreadsheets/d/1_quMjJRBHDLQSmWQouzzyi1DOejAtCZnAeesdVyRWiQ/edit#gid=1467293328",
           range="A:B",col_types = "Dd",na="NA")
ggplot(sheep_numbers,aes(x=Date,y=Total)) +
  geom_line(size=0.2,color="gray") +
  geom_smooth(color="dodgerblue",span=1,size=2,se=F) +
  theme_classic()

library(lubridate)
sheep_numbers %>%
  mutate(year=year(Date),
         month=month(Date),
         day=day(Date)) %>%
  group_by(year) %>%
  summarise(annual_total=sum(Total,na.rm = T)) %>%
  filter(year !=2015 & year !=2021) %>%
  ggplot(aes(x=year,y=annual_total)) +
  geom_line() 

sheep_weights <- read_sheet("https://docs.google.com/spreadsheets/d/1TVl_h7oUZ-J0Q5LTSz2dWkAxPQ70su2bDY2MEWkXbSg/edit#gid=1856279751",
           sheet = "weights",na="NA",col_type = "cDcd") 
sheep_weights %>%
  filter(eartag_n=="1211") %>%
  ggplot(aes(x=date,y=weight)) +
  geom_line()+
  theme_classic()
           