#what i learned
#1. rename snytax newname="oldname"


library(tidyverse)
library(googlesheets4)
library(lubridate)
gs4_deauth()
google_sheet <- "https://docs.google.com/spreadsheets/d/1_quMjJRBHDLQSmWQouzzyi1DOejAtCZnAeesdVyRWiQ/edit#gid=1467293328"
auction_data <- read_sheet(google_sheet,
           sheet = "numbers_and_prices",
           range = "A:J",
           col_types = "Ddcccccccc",
           na="NA") %>%
  rename_all(tolower) %>% #change all capital letter to lower case 
  rename(aged_sheep = "aged sheep",
         feeder_lambs = "feeder lambs",
         hair_lambs = "hair lambs",
         new_crop = "new crop",
         small="40-85",
         medium="85-105",
         large="106-130",
         extra_large=">131")
large_prices <- auction_data %>%
separate(large,sep="-",into=c("min","max"),
         remove = T,convert = T) %>% 
  select(date,min,max) 
 
large_prices %>%
  mutate(midpoint=(min+max)/2) %>%
  ggplot(aes(x=day,y=midpoint)) +
  geom_line(color="gray")
  geom_smooth(span=0.1,se=F)
large_prices %>%
  ggplot(aes(x=date,ymin=min,ymax=max))+
  geom_ribbon(fill="dodgerblue",color="black",size=0.2)+
  labs(title = "large lambs reliably are most valued in April and May",
       subtitle = "Prices of 106-130 pound lambs from 2015 to 2021",
       caption = "Data reported by United producers in Manchester, MI",
       x="Date",
       y="Price ($/100 pounds)")+
  theme_light()
#1
Aged_sheep <- auction_data %>%
  select(date,aged_sheep) %>%
  separate(aged_sheep,sep="-",into=c("min","max"),
           convert=T) %>%
  mutate(midpoint=(min+max)/2)
Aged_sheep %>%
  ggplot(aes(x=date,y=midpoint))+
  geom_line(color="gray")+
  geom_smooth(span=0.1,se=F)+
  labs(title = "aged_sheep reliably are most valued at the beggining of year",
       subtitle = "Prices of aged_sheep from 2015 to 2021",
       caption ="Data reported by United producers in Manchester, MI",
       x="date",
       y="Price ($/100 pounds)")+
  theme_light()
Aged_sheep %>%
  ggplot(aes(x=date,ymin=min,ymax=max))+
  geom_ribbon(fill="blue",color="red",size=0.1)+
  labs(title = "aged_sheep reliably are most valued at the beggining of year",
       subtitle = "Prices of aged_sheep from 2015 to 2021",
       caption ="Data reported by United producers in Manchester, MI",
       x="date",
       y="Price ($/100 pounds)")+
  theme_light()
large_prices %>%
  mutate(range=max-min) %>%
  ggplot(aes(x=date,y=range))+
  geom_line(color="gray")+
  geom_smooth(span=0.1,se=F)
medium_large <- auction_data %>%
  select(date,medium,large)%>%
  separate(medium,sep="-",
           into=c("medium_min_price","medium_max_price"),convert=T) %>%
  mutate(midpoint_medium=(medium_max_price+medium_min_price)/2) %>%
  separate(large,sep="-",
           into=c("large_min_price","large_max_price"),convert=T) %>%
  mutate(midpoint_large=(large_max_price+large_min_price)/2) %>%
  select(date,midpoint_medium,midpoint_large)
medium_large %>%
  mutate(difference=(midpoint_large*125-midpoint_medium*95)/100) %>%
  ggplot(aes(x=date,y=difference))+
  geom_line(color="gray")+
  geom_smooth(span=0.1,se=F)+
  theme_light()
