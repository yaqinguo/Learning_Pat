library(tidyverse)
library(googlesheets4)
gs4_deauth()
auction_data <- read_sheet("https://docs.google.com/spreadsheets/d/1_quMjJRBHDLQSmWQouzzyi1DOejAtCZnAeesdVyRWiQ/edit#gid=1467293328",
                           sheet="numbers_and_prices",
                           range="A:J",
                           col_type = "Ddcccccccc",
                           na="NA") %>%
  rename_all(tolower) %>%
  rename("aged_sheep" = "aged sheep",
         "feeder_lambs" = "feeder lambs",
         "hair_lambs" = "hair lambs",
         "new_crop" = "new crop",
         "small" = "40-85",
         "medium" = "85-105",
         "large" = "106-130",
         "extra_large" = ">131")

tidy_auction_data <- auction_data %>%
  pivot_longer(cols=-c("date", "total"), names_to="classes", values_to = "price_range") %>%
  separate(price_range, into=c("min", "max"), sep="-", convert=TRUE) %>%
  mutate(midpoint = (min+max)/2)

# 1. find previous N weeks to go into our rolling average (4 weeks)
# 2. represent those prices in seperate columns
# 3. calculate the average across the current and previous weeks's prices
# 4. plot data with the rolling average on top of weekly data

large_rolling_price_1 <- tidy_auction_data %>%
  filter(classes=="large") %>%
  mutate(lag0=midpoint,
         lag1=lag(midpoint,n=1,order_by = date),
         lag2=lag(midpoint,n=2,order_by = date),
         lag3=lag(midpoint,n=3,order_by = date)) %>%
  mutate(average_rolling=(lag0+lag1+lag2+lag3)/4) %>%
  select(date,midpoint,average_rolling)
  
large_rolling_price_2 <- tidy_auction_data %>%
  filter(classes=="large") %>%
  select(date,classes,midpoint) %>%
  mutate(lag0=midpoint,
         lag1=lag(midpoint,n=1,order_by = date),
         lag2=lag(midpoint,n=2,order_by = date),
         lag3=lag(midpoint,n=3,order_by = date)) %>%
  group_by(date) %>%
  summarise(midpoint=midpoint,average_rolling=mean((c(lag0,lag1,lag2,lag3))))

large_rolling_price_2 %>%
  pivot_longer(cols = -date,names_to="classes",values_to="prices") %>%
  ggplot(aes(x=date,y=prices,color=classes)) +
  geom_line() +
  scale_color_manual(values = c("blue","gray"))
#1
large_rolling_week3 <- tidy_auction_data %>%
  filter(classes=="large") %>%
  select(date,classes,midpoint) %>%
  mutate(lag0=midpoint,
         lag1=lag(midpoint,n=1,order_by = date),
         lag2=lag(midpoint,n=2,order_by = date)) %>%
  group_by(date) %>%
  summarise(midpoint=midpoint,average_rolling=mean((c(lag0,lag1,lag2))))

large_rolling_week3 %>%
  pivot_longer(cols = -date,names_to="classes",values_to="prices") %>%
  ggplot(aes(x=date,y=prices,color=classes)) +
  geom_line() +
  scale_color_manual(values = c("blue","gray"))

large_rolling_week5 <- tidy_auction_data %>%
  filter(classes=="large") %>%
  select(date,classes,midpoint) %>%
  mutate(lag0=midpoint,
         lag1=lag(midpoint,n=1,order_by = date),
         lag2=lag(midpoint,n=2,order_by = date),
         lag3=lag(midpoint,n=3,order_by = date),
         lag4=lag(midpoint,n=4,order_by = date),
         ) %>%
  group_by(date) %>%
  summarise(midpoint=midpoint,average_rolling=mean((c(lag0,lag1,lag2,lag3,lag4))))

large_rolling_week5 %>%
  pivot_longer(cols = -date,names_to="classes",values_to="prices") %>%
  ggplot(aes(x=date,y=prices,color=classes)) +
  geom_line() +
  scale_color_manual(values = c("blue","gray"))

#2
rolling_average <- tidy_auction_data %>% 
  group_by(classes) %>%
  mutate(lag0=midpoint,
         lag1=lag(midpoint,1,order_by = date),
         lag2=lag(midpoint,2,order_by = date),
         lag3=lag(midpoint,3,order_by = date)) %>%
   mutate(average_rolling=(lag0+lag1+lag2+lag3)/4) %>%
   select(date,classes,midpoint,average_rolling)
  # group_by(date)%>%
  # summarise(classes=classes,midpoint=midpoint,average_rolling=mean(c(lag0,lag1,lag2,lag3)),.groups="drop")
  
rolling_average %>%
  filter(classes=="small"|classes=="medium"|classes=="large"|classes=="extra_large") %>% 
  ggplot(aes(x=date,y=average_rolling,color=classes))+
  geom_line()
#3
#align is the function from which you are going to start to calculate
#if align="right", it ends with reference date and works backwards in time
#if align="left", it starts with reference date and works forward in time
library(zoo)
rolling <- tidy_auction_data %>%
  group_by(classes)%>%
  mutate(average_rolling=rollmean(midpoint,4,na.pad = T,align = "right"))%>%
  select(date,classes,midpoint,average_rolling)
rolling %>%
  filter(classes=="small"|classes=="medium"|classes=="large"|classes=="extra_large") %>%
  ggplot(aes(x=date,y=average_rolling,color=classes))+
  geom_line()























