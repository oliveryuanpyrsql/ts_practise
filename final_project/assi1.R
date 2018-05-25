###### load packages ######

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)

###### data import and format ######

rawdata <- read.csv("Daily_Births.csv")

rawdata$Date <- ymd(rawdata$Date)
rawdata$monthly <- substring(rawdata$Date,1,7)

###### aggregate data by month and create time series object ######

data_m <- rawdata %>% 
  select(2,3) %>% 
  group_by(monthly) %>% 
  dplyr::summarise(count = sum(Num_Births)) %>% 
  arrange(monthly)

birthts <- ts(data_m$count, frequency = 12, start = c(1977,1))
  
###### plot ######

plot.ts(birthts)

###### decomposing ######

### cyclical

cycle(birthts)

plot(aggregate(birthts, FUN = sum))

### trend seasonality and random

decomposedRes <- decompose(birthts)

plot(decomposedRes)

### white noise test

ltest <- Box.test(birthts,type = "Ljung-Box")

### autocorrelations

acf(birthts)

### partial autocorrelations

pacf(birthts)
