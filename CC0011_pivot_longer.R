# what i learned
# tidy data using pivot longer
#allow us to take wide data frame and make it long
# each column represents a different variable and each row represents different observations
#snytax pivot_long(data,cols,name_to,values_to)
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
# concatenate columns for differnet classes of sheep
# seperate min and max values into different columns
# calculate the midpoint price
# plot

auction_data %>%
  pivot_longer(cols = c("small","medium","large"),names_to="classes",
               values_to="price_range") 
tidy_auction_data <- auction_data %>%
  pivot_longer(cols = -c("date","total"),names_to="classes",
               values_to="price_range") %>%
  separate(price_range,sep="-",into=c("min","max"),convert=T,remove=T) %>%
  mutate(midpoint=(min+max)/2) 
tidy_auction_data %>%
  filter(classes=="small"|classes=="medium"|classes=="large"|classes=="extra_large") %>%
  ggplot(aes(x=date,y=midpoint,color=classes)) +
  #geom_line() +
  geom_smooth(span=0.1,se=F)+
  labs(title = "small lambs have the highest price,but all lambs peak in spring",
       caption = "Data reported by United producers in Manchester, MI",
       x="Date",
       y="Price ($/100 ibs)")+
  theme_light()
#1 
holiday_data <- read_sheet(google_sheet,
                           sheet = "holidays",
                           col_types = "cDDDDDDD") %>%
  pivot_longer(-holiday,names_to="year",values_to="date")

#3
tidy_auction_data %>%
  filter(classes=="small"|classes=="new_crop"|classes=="feeder_lambs") %>%
  ggplot(aes(x=date,y=midpoint,color=classes)) +
  #geom_line()+
  geom_smooth(span=0.1,se=F)
  
  









