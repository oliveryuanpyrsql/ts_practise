############ load packages

library(tidyverse)
library(lubridate)
library(forecast)
library(sarima)
library(tseries)
library(fpp2)

############ import data and merge two datasets #############

trips <- read.csv("austin_bikeshare_trips_fin.csv")
stations <- read.csv("austin_bikeshare_stations.csv")

mg_trip <- trips %>% 
  inner_join(stations, by=c("start_station_name" = "name")) %>% 
  filter(status == "active")
mg_trip$newdate <- dmy_hms(mg_trip$start_time)

############ data preparation ################

### handle date missing values and create weekly index

mg_trip$newyear <- lubridate::year(mg_trip$newdate)
mg_trip$ymd <- ymd(substring(mg_trip$newdate,1,10))
mg_trip$week <- lubridate::isoweek(mg_trip$ymd)

mg_trip$week <- ifelse(mg_trip$week - 10 >= 0,mg_trip$week,paste("0",mg_trip$week, sep =""))

mg_trip$wk_index <- paste(mg_trip$newyear,mg_trip$week, sep = "-")

### locate top 3 and bot 3 station

st_rank <- mg_trip %>% 
  select(start_station_name, trip_id) %>% 
  group_by(start_station_name) %>% 
  dplyr::summarise(n_trip = n()) %>% 
  arrange(desc(n_trip))

top1_st <- st_rank[1,1]
top2_st <- st_rank[2,1]
top3_st <- st_rank[3,1]

bot1_st <- st_rank[nrow(st_rank),1]
bot2_st <- st_rank[nrow(st_rank)-1,1]
bot3_st <- st_rank[nrow(st_rank)-2,1]

### separate dataset and aggregate data for each series

## top1

top1 <- mg_trip %>% 
  filter(start_station_name == top1_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

top1 <- top1[6:nrow(top1),]

top1_date <- as.data.frame(seq(as.Date("2014/03/03"),as.Date("2017/07/31"), by = "day"))
names(top1_date) <- "ymd"
top1_date$ymd <- ymd(top1_date$ymd)

top1_lst <- left_join(top1_date,top1, by = "ymd")

top1_lst[is.na(top1_lst)] <- median(top1_lst$n_trip,na.rm = T)

top1_lst <- top1_lst[1:(nrow(top1_lst)-1),]

top1_lst$group <- rep(1:(nrow(top1_lst)/7), each = 7)

top1_lst <- top1_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

top1_ts <- ts(top1_lst$count, frequency = 52)

## top2

top2 <- mg_trip %>% 
  filter(start_station_name == top2_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

top2 <- top2[3:nrow(top2),]

top2_date <- as.data.frame(seq(as.Date("2013/12/23"),as.Date("2017/07/31"), by = "day"))

names(top2_date) <- "ymd"
top2_date$ymd <- ymd(top2_date$ymd)

top2_lst <- left_join(top2_date,top2,by = "ymd")

top2_lst[is.na(top2_lst)] <- median(top2_lst$n_trip,na.rm = T)

top2_lst <- top2_lst[1:(nrow(top2_lst)-1),]

top2_lst$group <- rep(1:(nrow(top2_lst)/7), each = 7)

top2_lst <- top2_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

top2_ts <- ts(top2_lst$count, frequency = 52)

## top3

top3 <- mg_trip %>% 
  filter(start_station_name == top3_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

top3 <- top3[3:nrow(top3),]

top3_date <- as.data.frame(seq(as.Date("2013/12/23"),as.Date("2017/07/31"), by = "day"))

names(top3_date) <- "ymd"
top3_date$ymd <- ymd(top3_date$ymd)

top3_lst <- left_join(top3_date,top3,by = "ymd")

top3_lst[is.na(top3_lst)] <- median(top3_lst$n_trip,na.rm = T)

top3_lst <- top3_lst[1:(nrow(top3_lst)-1),]

top3_lst$group <- rep(1:(nrow(top3_lst)/7), each = 7)

top3_lst <- top3_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

top3_ts <- ts(top3_lst$count, frequency = 52)

## bot 1

bot1 <- mg_trip %>% 
  filter(start_station_name == bot1_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

bot1 <- bot1[4:nrow(bot1),]

bot1_date <- as.data.frame(seq(as.Date("2017/07/10"),as.Date("2017/07/31"), by = "day"))

names(bot1_date) <- "ymd"

bot1_date$ymd <- ymd(bot1_date$ymd)

bot1_lst <- left_join(bot1_date,bot1,by = "ymd")

bot1_lst[is.na(bot1_lst)] <- median(bot1_lst$n_trip,na.rm = T)

bot1_lst <- bot1_lst[1:(nrow(bot1_lst)-1),]

bot1_lst$group <- rep(1:(nrow(bot1_lst)/7), each = 7)

bot1_lst <- bot1_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

bot1_ts <- ts(bot1_lst$count, frequency = 52)

## bot2

bot2 <- mg_trip %>% 
  filter(start_station_name == bot2_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

bot2 <- bot2[4:nrow(bot2),]

bot2_date <- as.data.frame(seq(as.Date("2017/07/10"),as.Date("2017/07/31"), by = "day"))

names(bot2_date) <- "ymd"

bot2_date$ymd <- ymd(bot2_date$ymd)

bot2_lst <- left_join(bot2_date,bot2,by = "ymd")

bot2_lst[is.na(bot2_lst)] <- median(bot2_lst$n_trip,na.rm = T)

bot2_lst <- bot2_lst[1:(nrow(bot2_lst)-1),]

bot2_lst$group <- rep(1:(nrow(bot2_lst)/7), each = 7)

bot2_lst <- bot2_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

bot2_ts <- ts(bot2_lst$count, frequency = 52)

## bot3

bot3 <- mg_trip %>% 
  filter(start_station_name == bot3_st$start_station_name) %>% 
  select(ymd, trip_id) %>% 
  group_by(ymd) %>% 
  dplyr::summarise(n_trip = n())

bot3 <- bot3[5:nrow(bot3),]

bot3_date <- as.data.frame(seq(as.Date("2017/07/10"),as.Date("2017/07/31"), by = "day"))

names(bot3_date) <- "ymd"

bot3_date$ymd <- ymd(bot3_date$ymd)

bot3_lst <- left_join(bot3_date,bot3,by = "ymd")

bot3_lst[is.na(bot3_lst)] <- median(bot3_lst$n_trip,na.rm = T)

bot3_lst <- bot3_lst[1:(nrow(bot3_lst)-1),]

bot3_lst$group <- rep(1:(nrow(bot3_lst)/7), each = 7)

bot3_lst <- bot3_lst %>% 
  group_by(group) %>% 
  dplyr::summarise(count = sum(n_trip))

bot3_ts <- ts(bot3_lst$count, frequency = 52)

###### model build for series 1,2 & 3

### model for top1

plot.ts(top1_ts)

decomposedRes_top1 <- decompose(top1_ts)

plot(decomposedRes_top1)

box_test_1 <- Box.test(top1_ts,type = "Ljung-Box")

box_test_1

acf(top1_ts)

pacf(top1_ts)

adf_test_1 <- adf.test(top1_ts)

adf_test_1

## arima

top1_arima <- auto.arima(top1_ts)

top1_arima

tsdisplay(residuals(top1_arima))

top1_arima_fcast <- forecast(top1_arima, h = 6)

accuracy(top1_arima_fcast)

## exponential smoothing

ese.top1 <- ses(top1_ts, h =6)
autoplot(ese.top1)
accuracy(ese.top1)

exp.top1 <- ets(top1_ts,model = "ZZZ")
exp.top1.fcast <- forecast(exp.top1,h = 6)
accuracy(exp.top1.fcast)

## model for top2

plot.ts(top2_ts)

decomposedRes_top2 <- decompose(top2_ts)

plot(decomposedRes_top2)

box_test_2 <- Box.test(top2_ts,type = "Ljung-Box")

box_test_2

acf(top2_ts)

pacf(top2_ts)

adf_test_2 <- adf.test(top2_ts)

adf_test_2

## arima

top2_arima <- auto.arima(top2_ts)

top2_arima

tsdisplay(residuals(top2_arima))

top2_arima_fcast <- forecast(top2_arima, h = 6)

accuracy(top2_arima_fcast)

## exponential smoothing

ese.top2 <- ses(top2_ts, h =6)
autoplot(ese.top2)
accuracy(ese.top2)

exp.top2 <- ets(top2_ts,model = "ZZZ")
exp.top2.fcast <- forecast(exp.top2,h = 6)
accuracy(exp.top2.fcast)

### model for top 3

plot.ts(top3_ts)

decomposedRes_top3 <- decompose(top3_ts)

plot(decomposedRes_top3)

box_test_3 <- Box.test(top3_ts,type = "Ljung-Box")

box_test_3

acf(top3_ts)

pacf(top3_ts)

adf_test_3 <- adf.test(top3_ts)

adf_test_3

## arima

top3_arima <- auto.arima(top3_ts)

top3_arima

tsdisplay(residuals(top3_arima))

top3_arima_fcast <- forecast(top3_arima, h = 6)

accuracy(top3_arima_fcast)

## exponential smoothing

ese.top3 <- ses(top3_ts, h =6)
autoplot(ese.top3)
accuracy(ese.top3)

exp.top3 <- ets(top3_ts,model = "ZZZ")
exp.top3.fcast <- forecast(exp.top3,h = 6)
accuracy(exp.top3.fcast)

###### plot for bot 3 series

ts.plot(bot1_ts)

ts.plot(bot2_ts)

ts.plot(bot3_ts)

##### overall trend

mg_trip$newmonth <- substring(mg_trip$ymd,1,7)

overall <- mg_trip %>% 
  select(newmonth, trip_id) %>% 
  group_by(newmonth) %>% 
  dplyr::summarise(n_trip = n())

all_ts <- ts(overall$n_trip, frequency = 12, start=c(2013,12))  

ts.plot(all_ts)  

decomposedRes_all <- decompose(all_ts)

plot(decomposedRes_all)

adf_test_all <- adf.test(all_ts)

adf_test_all

mg_trip$newhour <- hour(mg_trip$newdate)

daytime <- mg_trip %>% 
  select(newhour,trip_id) %>% 
  group_by(newhour) %>% 
  dplyr::summarise(count = n())
  
ggplot(data = daytime)+
  geom_line(aes(x = newhour, y = count,group = 1))



top1_lst$type <- "Top1"
top2_lst$type <- "Top2"
top3_lst$type <- "Top3"

bot1$type <- "Bot1"
bot2$type <- "Bot2"
bot3$type <- "Bot3"

top.all <- as.data.frame(rbind(top1_lst,top2_lst,top3_lst))

bot.all <- as.data.frame(rbind(bot1,bot2,bot3))

names(top.all)[1] <- "week"
names(bot.all)[1] <- "day"

ggplot(data = top.all)+
  geom_line(aes(x = week, y = count, group = type, color = type))+
  ggtitle("Weekly Trend of Bike Usage in the top 3 stations")

ggplot(data = bot.all)+
  geom_line(aes(x = day, y = n_trip, group = type, color = type))+
  ggtitle("Daily Trend of Bike Usage in the Bot 3 stations")

ggplot(data = overall)+
  geom_line(aes(x = newmonth, y = n_trip, group = 1))+
  theme(axis.text.x = element_text(angle = -45, hjust = -0.05))+
  ggtitle("Monthly Trend of Bike Usage in All Stations")+
  xlab("Month")+
  ylab("Count")

ggplot(data = daytime)+
  geom_line(aes(x = newhour, y = count,group = 1))+
  ggtitle("Bike Usage Daytime Trend")+
  xlab("Hour")

