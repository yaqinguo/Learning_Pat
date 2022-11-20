library(tidyverse)
library(lubridate)
now()
breeding_start <- date("2020-04-25")
lamb_start <- breeding_start + days(147)
breed_date <- date("2020-04-07")
due_date <- breed_date + months(3) + weeks(3) + days(3)
due_date 
due_date - breed_date
difftime(due_date,breed_date,units = "day")


due_date <- date("2021-03-01")
due_date - days(283)
#1
difftime("2020-05-30","2020-04-25")
#2
due_date_farrow <- date("2021-01-10")
breed_date <- due_date_farrow - months(3) - weeks(3) - days(3) 
breed_date 

#3
due_date_lamb <- date("2020-10-03")
due_date_next <- due_date_lamb + months(8)
diff <- difftime(due_date_next,due_date_lamb,units = "week") 
recvery_period <- diff - 9
recvery_period
# cow average days 283 days
breed_date <- now()
due_date <- breed_date + days(283)
gestation_date <- breed_date + months(7)+weeks(7)+days(7)
difftime(due_date,gestation_date)
#sheep average days 147 days
breed_date <- now()
due_date <- breed_date + days(147)
gestation_date <- breed_date + weeks(21)
difftime(due_date,gestation_date)




