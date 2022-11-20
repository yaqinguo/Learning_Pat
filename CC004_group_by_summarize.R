library(tidyverse)

candy_data <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv",
                       col_types="clllllllllddd")
candy_data %>%
  filter(chocolate) %>%
  group_by(bar) %>%
  summarise(mean_price=mean(pricepercent),sd=sd(pricepercent),n=n())

candy_data %>%
  group_by(chocolate,bar) %>%
  summarise(mean_price=mean(pricepercent),sd=sd(pricepercent),n=n()) %>%
  ungroup()
#1
candy_data %>%
  filter(winpercent > 75) %>%
  count(chocolate)

#2
candy_data %>%
  group_by(fruity) %>%
  summarise(mean_price=mean(pricepercent),sd=sd(pricepercent),n=n())

#3
candy_data %>%
  group_by(winpercent > 50) %>%
  summarise(mean_price=mean(pricepercent),sd=sd(pricepercent),n=n())

candy_data %>%
  group_by(winpercent >80 , sugarpercent <= 0.5) %>%
  summarise(mean_price=mean(pricepercent),sd=sd(pricepercent),n=n()) %>% ungroup()
candy_data %>%
  filter(winpercent >80 & sugarpercent <= 0.5)
