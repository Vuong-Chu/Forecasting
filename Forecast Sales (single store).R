library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(xts)
library(zoo)
library(seasonal)
library(imputeTS)

data <- read.csv("train.csv",header=TRUE)

## Process training set
data1 <- data %>% group_by(store_id, date) %>% 
  summarise(Amount = sum(amount))

data1$date <- as.Date(data1$date, "%m/%d/%Y")

data2 <- data1 %>% dplyr:: filter(store_id == 0) ## Forecast sales for Store_ID = 0

ts <- seq(ymd("2016-12-14"), ymd("2018-07-31"), by="day")

df <- data.frame(date=ts)
data2 <- data2[,-1]

data_with_missing_times <- full_join(df,data2)

data_with_missing_times <-data_with_missing_times[6:nrow(data_with_missing_times),] %>% group_by(date) %>% mutate_each(funs(ifelse(is.na(.),0,.)))

data_with_missing_times$Amount <- log(data_with_missing_times$Amount)

tsdata <- xts(data_with_missing_times$Amount, order.by=as.POSIXlt(data_with_missing_times$date,tryFormats = c("%y %m %d")))

tsdata <- na.remove(ma(tsdata, 7))

tsdata <- ts(tsdata, c(2016,12,22), frequency = 365)

autoplot(tsdata)+ggtitle("Card Sales")+xlab("Date")+ylab("Thousands") ## Data Plot

train.data <- subset(tsdata,end=length(tsdata) - 50)
test.data <- subset(tsdata,start=length(tsdata) - 50)

fit1<-nnetar(train.data,lambda = "auto")
fit2 <- auto.arima(train.data, lambda = "auto")
fit3 <- ets(train.data,lambda = "auto")
Benchmark <- arima(train.data,order=c (1, 0, 0)) %>% forecast(h=51)
result1 <- forecast(fit1,h=51)
result2 <- forecast(fit2,h=51)
result3 <- forecast(fit3,h=51)
combination <- (result1$mean + result2$mean + result3$mean)/3
accuracy(combination,test.data)
forecast.data <- ts(c(result1$x,combination),c(2016,12,22), frequency = 365)

autoplot(tsdata, series="Original") +
  autolayer(forecast.data, series="Forecast") +
  scale_colour_manual(
    values=c(`Forecast`="red",`Original`="blue"))
