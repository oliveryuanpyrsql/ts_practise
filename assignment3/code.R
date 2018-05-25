############ load packages

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)
library(tseries)

############ import data

raw_data <- read.csv("mt_steel.csv")

############ create time series object

df_ts <- ts(raw_data$EXPORT, frequency = 1, start = 1937)


########### plot data

plot.ts(df_ts)

decomposeRes <- decompose(df_ts)

########### box test

testRes <- Box.test(df_ts)

testRes

########### acf and pacf

acf(df_ts)

pacf(df_ts)

############ auto arima

model <- auto.arima(df_ts)

fcast <- plot(forecast(model,h =3))

tsdisplay(residuals(model), lag.max = 25, main = "(0,0,1) Model Residual")

accuracy(model)

adf.test(model$residuals)

