############ load packages

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)
library(tseries)
library(fpp2)

############ import data

raw_data <- read.csv("concertsales1.csv")
raw_data$newdate <- dmy(raw_data$DATE)
raw_data$monthly <- substring(raw_data$newdate,1,7)

df_data <- raw_data %>% 
  select(2,4) %>% 
  group_by(monthly) %>% 
  dplyr::summarise(monthlysale = sum(sales))

############ create time series object

df_ts <- ts(df_data$monthlysale, frequency = 12, start = c(2005,1))


########### plot data

plot.ts(df_ts)

decomposeRes <- decompose(df_ts)
plot(decomposeRes)


########### box test

testRes <- Box.test(df_ts)

testRes

########### acf and pacf

acf(df_ts)

pacf(df_ts)

############ exponential smooth model comparison ##############

#df_ts <- window(df_ts, end = c(2011,12))
#df.test <- window(df_ts, start = c(2012,1))


########### SES

## model test

ses.concert <- ses(df_ts, alpha = .2, h = 12)
autoplot(ses.concert)

concert.dif <- diff(df_ts)
autoplot(concert.dif)

ses.concert.dif <- ses(concert.dif, alpha = .2, h = 12)
autoplot(ses.concert.dif)

accuracy(ses.concert.dif)

## find the best alpha

# identify optimal alpha parameter

alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(concert.dif, alpha = alpha[i], h = 100)
  RMSE[i] <- accuracy(fit)[1,2]
}

# convert to a data frame and idenitify min alpha value
alpha.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, RMSE == min(RMSE))

# plot RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")

# refit model with alpha = .05
ses.concert.opt <- ses(concert.dif, alpha = .05, h = 12)

# performance eval
accuracy(ses.concert.opt) # accuracy for SES



##############  Holt's method

holt_method <- ets(df_ts, model = "AAN")

holt.fcast <- forecast(holt_method, h = 12)

accuracy(holt.fcast)

############# seasonal exp smoothing

seasonal_exp <- ets(df_ts, model = "ANA")

season.fcast <- forecast(seasonal_exp, h = 12)

accuracy(seasonal_exp)

############ HoltWinter's additive

ht_add <- ets(df_ts, model = "AAA")

add.fcast <- forecast(ht_add, h = 12)

accuracy(ht_add)

############ HoltWinter's multi

ht_multi <- ets(df_ts, model = "MAM")

multi.fcast <- forecast(ht_multi, h = 12)

accuracy(ht_multi)

########### model comparison

modcomp <- rbind(as.data.frame(accuracy(ses.concert.opt)),
                 as.data.frame(accuracy(holt.fcast)),
                 as.data.frame(accuracy(season.fcast)),
                 as.data.frame(accuracy(add.fcast)),
                 as.data.frame(accuracy(multi.fcast)))


rownames(modcomp) <- c("SES", "Linear","Seasonal Exponential","Holt's Additive","Holt's Multiplicative")


AIC <- c(1666.567, 1556.043, 1602.883, 1565.678, 1543.458)
#BIC <- c(1672.125, 1569.980, 1644.695, 1613.066, 1590.845)
modcomp$AIC <- AIC
#modcomp$BIC <- BIC

