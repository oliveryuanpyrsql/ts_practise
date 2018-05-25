###### load packages ######

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)

###### data import and format ######

rawdata <- read.csv("overtime_sale.csv")

data_m <- rawdata %>% 
  select(1,2) %>% 
  group_by(monthly) %>% 
  dplyr::summarise(sale = sum(privatesale))

###### aggregate data by month and create time series object ######

lts <- ts(data_m$sale, frequency = 12, start = c(2015,10))

###### plot ######

plot.ts(lts)

###### decomposing ######

### cyclical

cycle(lts)

plot(aggregate(lts, FUN = sum))

### trend seasonality and random

decomposedRes <- decompose(lts)

plot(decomposedRes)

### white noise test

ltest <- Box.test(lts,type = "Ljung-Box")

ltest

### autocorrelations

acf(lts)

### partial autocorrelations

pacf(lts)
