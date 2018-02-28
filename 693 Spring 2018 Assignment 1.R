##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights

## Find all flights satify following restrictions

#==a. had an arrival delay of two or more hours

flights_a <- flights %>% 
  filter(arr_delay >= 120)

#==b. flew to houston

flights_b <- flights %>% 
  filter(dest == "IAH" | dest == "HOU")

#==c. were operated by united, american, or delta

flights_c <- flights %>% 
  filter(carrier == "UA" | carrier == "AA" | carrier == "DL")

#==d. departed in summer (july, august, and september)

flights_d <- flights %>% 
  filter(month == 7 | month == 8 | month == 9)

#==e. arrived more than two hours late, but didn't leave late

flights_e <- flights %>% 
  filter(arr_delay > 120) %>% 
  filter(dep_delay <= 0)

#==f. were delayed by at least an hour, but made up over 30 minutes in flight

flights_f <- flights %>% 
  mutate(change=dep_delay - arr_delay) %>% 
  filter(dep_delay >= 60) %>% 
  filter(change > 30)

#==g. departed between midnight and 6 a.m. (inclusive)

flights_g <- flights %>% 
  filter(dep_time <= 600)

#==h. sort to find the most delayed flights.
#== find the most depart delayed flights
flights %>% 
  arrange(desc(dep_delay))

#== find the most arrive delayed flights
flights %>% 
  arrange(desc(arr_delay))

#=== find the flights that left earliest

flights %>% 
  arrange(dep_delay)

flights_h <- flights %>% 
  filter(dep_delay == -43)

#==i. sort to find the fastest flights

flights_i <- flights %>% 
  mutate(speed=distance/air_time) %>% 
  arrange(desc(speed))

#==j. find the flights traveled the longest

flights_j1 <- flights %>% 
  filter(distance == max(flights$distance))

#=== find the fligths traveled the shortest

flights_j2 <- flights %>% 
  filter(distance == min(flights$distance))

