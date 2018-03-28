##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights
?flights

## Question 3

##number of cancelled flights per day

flights_3 <- flights %>% 
  mutate(cancel=is.na(air_time)) %>% 
  group_by(year,month,day) %>% 
  summarize(n_cancel=n(),p_cancel=mean(cancel),ave_delay=mean(arr_delay+dep_delay,na.rm=T))

plot(flights_3$n) 
cor(flights_3$p_cancel,flights_3$ave_delay) 

# The correlation between proportion of cancelled flights and average delay is 0.58. 

## Question 4

##find the carrier has the worst delays
flights_4a <- flights %>% 
  group_by(carrier) %>% 
  summarize(car_delay=mean(arr_delay+dep_delay,na.rm=T)) %>% 
  arrange(desc(car_delay))

# "F9": Frontier Airlines Inc. has the worst delay in terms of average of delays.

flights_4b <- flights %>% 
  group_by(carrier,dest) %>% 
  summarize(car_delay=mean(arr_delay+dep_delay,na.rm=T)) %>% 
  arrange(desc(car_delay)) 

# "UA" in destination "STL" has teh worst delay in terms of average of delays.

## Question 5
## not sure about the meaning of "before the first delay of greater than 1 hour"
flights_5 <- flights %>% 
  filter ?

## Question 6
filghts_6 <- flights %>% 
  filter(!is.na(arr_delay),!is.na(dep_delay)) %>% 
  group_by(tailnum) %>% 
  summarize(on_time = max(max(dep_delay),max(arr_delay))) %>% 
  arrange(desc(on_time))

# "N384HA" is the plane has the worst on-time record 

## Question 7

flights_7 <- flights %>% 
  filter(!is.na(dep_delay)) %>% 
  mutate(delay=(dep_delay >0)) %>% 
  group_by(hour) %>% 
  summarize(p_delay=mean(delay)) %>% 
  arrange(p_delay)

# 6am is the best time of day

## Question 8

flights_8a <- flights %>% 
  group_by(dest) %>% 
  summarize(total_delay=sum(dep_delay+arr_delay,na.rm=T))

flights_8b <- flights %>% 
  mutate(delay=(arr_delay >0)) %>% 
  group_by(dest,flight) %>% 
  summarize(p_delay=mean(delay,na.rm=T))

## Question 9
flights_9a <- flights %>% 
  filter(!is.na(air_time),!is.na(distance)) %>% 
  group_by(dest,flight) %>% 
  summarize(speed=max(distance/air_time)) %>% 
  arrange(desc(speed)) %>% 
  select(dest,flight,speed)
  
# Haven't find suspiciously fast flights

flights_9b <- flights %>% 
  filter(!is.na(air_time)) %>% 
  group_by(origin, dest,flight) %>% 
  summarize(air_delay=mean(air_time)-min(air_time)) %>% 
  arrange(desc(air_delay))

# flight 4667 for EWR to MSP was most delayed in the air.
