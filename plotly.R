###DATA CLeaning 


#For the smart meter data, the primary interest is the variable `powerallphases` which is the sum of real power over all power phases consumed in the household. The plugs data provides appliance-level consumption. 

#Read all data from 06 (smart meter data), only need the first column (* powerallphases: Sum of real power over all phases)

#one row per second -> change it to daily (better to do the visualization)

library(tidyverse)
library(readr)
library(purrr)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(plotly)
library(altair)


setwd('~/ANLY503/anly503-fall2020-a5-nwu1017/eco/05')

household_5_1 <- data.frame(read.csv(file = "2012-12-24.csv",header = FALSE, sep = ",")[,1]) 
names(household_5_1)[1] <- "smartmeter"
household_5_1 <-household_5_1 %>%
  mutate(date = ymd("2012-12-24")) %>%
  mutate(hour = rep(1:24, each = 3600)) 

household_5_2 <- data.frame(read.csv(file = "2012-12-25.csv",header = FALSE, sep = ",")[,1]) 
names(household_5_2)[1] <- "smartmeter"
household_5_2 <-household_5_2 %>%
  mutate(date = ymd("2012-12-25")) %>%
  mutate(hour = rep(1:24, each = 3600)) 

household_5_3 <- data.frame(read.csv(file = "2012-12-31.csv",header = FALSE, sep = ",")[,1]) 
names(household_5_3)[1] <- "smartmeter"
household_5_3 <-household_5_3 %>%
  mutate(date = ymd("2012-12-31")) %>%
  mutate(hour = rep(1:24, each = 3600)) 

household_5_4 <- data.frame(read.csv(file = "2013-01-01.csv",header = FALSE, sep = ",")[,1]) 
names(household_5_4)[1] <- "smartmeter"
household_5_4 <-household_5_4 %>%
  mutate(date = ymd("2013-01-01")) %>%
  mutate(hour = rep(1:24, each = 3600)) 


household_5_1_hour <- household_5_1%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2012-12-24"))

household_5_2_hour <- household_5_2%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2012-12-25"))

household_5_3_hour <- household_5_3%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2012-12-31"))

household_5_4_hour <- household_5_4%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2013-01-01"))

household_5 <- rbind( household_5_2_hour, household_5_4_hour)

write_csv(household_5,"household_5.csv")

household_5_p<- household_5 %>%
  ggplot(aes(hour, sum, color = as.factor(date))) +
  geom_line() +
  labs(title ="Total Energy Consumption by Household No.5", x = "Hour of the Day", y = "Smart Meters Value", caption = "Data Source: the Electricity Consumption and Occupancy data") +
  scale_color_manual(values = c("red","lightblue"), labels= c("Christmas Day","New Year"), name = "Occasion") +
  theme_clean() 

ggplotly(household_5_p)




setwd('~/ANLY503/anly503-fall2020-a5-nwu1017/a5/eco/04_sm_csv/04')


household_4_2 <- data.frame(read.csv(file = "2012-12-25.csv",header = FALSE, sep = ",")[,1]) 
names(household_4_2)[1] <- "smartmeter"
household_4_2 <-household_4_2 %>%
  mutate(date = ymd("2012-12-25")) %>%
  mutate(hour = rep(1:24, each = 3600)) 


household_4_4 <- data.frame(read.csv(file = "2013-01-01.csv",header = FALSE, sep = ",")[,1]) 
names(household_4_4)[1] <- "smartmeter"
household_4_4 <-household_4_4 %>%
  mutate(date = ymd("2013-01-01")) %>%
  mutate(hour = rep(1:24, each = 3600)) 


household_4_2_hour <- household_4_2%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2012-12-25"))

household_4_4_hour <- household_4_4%>%
  group_by(hour) %>%
  summarize(sum = sum(smartmeter)) %>%
  mutate(date = ymd("2013-01-01"))

household_4 <- rbind( household_4_2_hour, household_4_4_hour)

write_csv(household_4,"household_4.csv")

household_4_p<- household_4 %>%
  ggplot(aes(hour, sum, color = as.factor(date))) +
  geom_line() +
  labs(title ="Total Energy Consumption by Household No.4", x = "Hour of the Day", y = "Smart Meters Value", caption = "Data Source: the Electricity Consumption and Occupancy data") +
  scale_color_manual(values = c("red","lightblue"), labels= c("Christmas Day","New Year"), name = "Occasion") +
  theme_clean() 

ggplotly(household_4_p)

#Can't run Altair on R as it crushed my R studio each time
  