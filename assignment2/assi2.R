###### load packages ######

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)
library(tseries)

###### data import ######

list.filename <- list.files(pattern = ".csv$")
list.filename

list.data <- list()

for (i in 1:length(list.filename))
{
  list.data[[i]] <- read.csv(list.filename[i])
}


###### choose series to check ######

seriesno <- 1 # input which series you want to look at
  
raw_df <- as.data.frame(list.data[seriesno])

###### data preparation ######

raw_df$Date <- dmy(raw_df$Date)
raw_df$Date <- substring(raw_df$Date,1,7)
raw_df <- raw_df %>% 
  group_by(Date) %>% 
  dplyr::summarise(Y = sum(Y))

startyear <- as.numeric(substring(raw_df[1,1],1,4))
startmonth <- as.numeric(substring(raw_df[1,1],6,7))


df_ts <- ts(raw_df$Y, frequency = 12, start = c(startyear, startmonth))
  
###### plot ######

plot.ts(df_ts)

###### decomposing ######

### cyclical

cycle(df_ts)

plot(aggregate(df_ts, FUN = sum))

### trend seasonality and random

decomposedRes <- decompose(df_ts)

plot(decomposedRes)

### white noise test ###

box_test <- Box.test(df_ts,type = "Ljung-Box")

box_test

### autocorrelations ###

acf(df_ts)

### partial autocorrelations ###

pacf(df_ts)

### ADF test ###

adf_test <- adf.test(df_ts)

adf_test


### fit an ARIMA model ###

fitmodel <- auto.arima(df_ts)

fitmodel

### evaluate model and iterate ###

tsdisplay(residuals(fitmodel))

### first difference

a_series <- diff(raw_df$Y, lag = 1)

a_ts <- ts(a_series)

plot.ts(a_ts)
