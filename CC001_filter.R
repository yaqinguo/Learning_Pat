library(tidyverse)
library(ggplot2)
candy_data <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv",
                       col_types="clllllllllddd")
summary(candy_data)
#exercise1
candy_data %>% #spit out the data frame that we read in  above for our pipeline
  filter(pluribus) %>% #return those rows that contain bite-sized candies
  pivot_longer(
    cols=c(chocolate,fruity,caramel,peanutyalmondy,nougat,crispedricewafer,hard,bar,pluribus),
    names_to="type",
    values_to="answer") %>% #concatenate all of the characteristic columns into two columns
  filter(answer) %>% #remove those rows where a characteristic ha been FALSE
  group_by(type) %>% #arrange the data by characteristics,later on will be calculate the data according to this group
  summarise(total=sum(answer)) #return the number of candies with each characteristic
#exercise2
pluribus_data <- candy_data %>% #spit out the data frame that we read in  above for our pipeline
  filter(pluribus&(chocolate | fruity )) %>% 
  pivot_longer(cols=c(chocolate,fruity),names_to="type",values_to="answer") %>%
  filter(answer)
pluribus_data %>%
  group_by(type) %>%
  summarise(median=median(winpercent),IQR=IQR(winpercent),N=n())
pluribus_data %>%
  ggplot(aes(x=type,y=winpercent,color=hard))+
  geom_jitter(width = 0.1)+
  theme_classic()+
  scale_x_discrete(breaks=c("chocolate","fruity"),labels=c("chocolate","fruity"))+
  labs(y="Contest won (%)",x=NULL,title = "Bite-sized candies containing cholocate are preferred to candy without")+
  coord_cartesian(ylim=c(0,100))
#exercise
chocolate_data <- candy_data %>%
  filter(chocolate & (pluribus | bar)) %>%
  pivot_longer(cols=c(pluribus,bar),names_to="type",values_to="answer") %>%
  filter(answer) 

chocolate_data %>%
  group_by(type) %>%
  summarise(median=median(pricepercent),IQR=IQR(pricepercent),N=n())

chocolate_data %>%
  ggplot(aes(x=type,y=pricepercent,color=peanutyalmondy))+
  geom_jitter(width = 0.1)+
  scale_x_discrete(breaks=c("bar","pluribus"),labels=c("Bar","Bite-sized"))+
  coord_cartesian(ylim=c(0,1))+
  labs(y="Price(fraction)",x=NULL,title = "Cholocate candy bars tend to be more expensive than bite-sized chocolate candies")+
  theme_classic()
