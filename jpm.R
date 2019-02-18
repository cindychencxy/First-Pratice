library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(corrplot)
library(gridExtra)
library(forecast)
library(tseries)
library(TSA)
library(tibble)
library(TTR)
library(xts)
library(dygraphs)
library(zoo)
library(rmarkdown)
library(fpp)


#Loading data
jpm<-read.csv('~/Downloads/JPM.csv')

#Data Overview
colSums(is.na(jpm))
summary(jpm)

#Data Cleaning
jpm$Date<-as.Date(jpm$Date,format="%Y-%m-%d")
summary(jpm)
str(jpm)

#Data visualization
options(repr.plot.width=12,repr.plot.height=12)
Open=ggplot(jpm,aes(Open))+
  geom_histogram(bins=50,aes(y=..density..),col="red",fill="red",alpha=0.3)+
  geom_density()+
  xlim(c(0,150))
High=ggplot(jpm,aes(High))+
  geom_histogram(bins=50,aes(y=..density..),col="blue",fill="blue",alpha=0.3)+
  geom_density()+
  xlim(c(0,150))
Low=ggplot(jpm,aes(Low))+
  geom_histogram(bins=50,aes(y=..density..),col="green",fill="green",alpha=0.3)+
  geom_density()+
  xlim(c(0,150))
Close=ggplot(jpm,aes(Close))+
  geom_histogram(bins=50,aes(y=..density..),col="yellow",fill="yellow",alpha=0.3)+
  geom_density()+
  xlim(c(0,150))
grid.arrange(Open,High,Low,Close,nrow=2,ncol=2)

#Time Series
xts_high<-xts(jpm$High,order.by=jpm$Date)
xts_low<-xts(jpm$Low,order.by=jpm$Date)
attr(xts_high,'frequency')<-length(xts_high)/12
attr(xts_low,'frequency')<-length(xts_low)/12
xts_high_low<-cbind(xts_high,xts_low)


dygraph(xts_high_low,main="JP Morgan High&Low Share Price from 2014 to 2019 (Daily)")%>%
  dySeries("xts_high",label="High")%>%
  dySeries("xts_low",label="Low")%>%
  dyRangeSelector(height=20)


dygraph(xts_high_low,main="JP Morgan High&Low Share Price from 2014 to 2019 (Daily)")%>%
  dySeries("xts_high",label="High")%>%
  dySeries("xts_low",label="Low")%>%
  dyOptions(stackedGraph = TRUE)%>%
  dyRangeSelector(height=20)


#Stationary
attr(xts_high,'frequency')<-length(xts_high)/5

adf.test(xtshigh,alternative="stationary",k=0)

#Decomposing Time Series
tshigh=as.ts(xts_high,start=c(2014))
ts_components_add<-decompose(tshigh,type="additive")
ts_components_mul<-decompose(tshigh,type="multiplicative")
plot(ts_components_add,col="red")
plot(ts_components_mul,col="blue")

#Differencing a Time Series
xtsdiff_high<-diff(xts_high,differences=1)
tsdiff_high<-diff(tshigh,differences=1)
plot.xts(xtsdiff_high,col="blue")

adf.test(tsdiff_high,alternative="stationary",k=0)


#ARIMA Model
Acf(xtsdiff_high,lag.max=60)
Acf(xtsdiff_high,lag.max=60,plot=FALSE)

Pacf(xtsdiff_high,lag.max=60)
Pacf(xtsdiff_high,lag.max=60,plot=FALSE)

tsarima500<-auto.arima(head(xts_high,-500))
print(tsarima500)
autoplot(tsarima500)
summary(tsarima500)

tsarima250<-auto.arima(head(xts_high,-250))
print(tsarima250)
autoplot(tsarima250)
summary(tsarima250)
#Forecasting&Evaluation
forecast_500<-forecast(tsarima500,h=500)
autoplot(forecast_500)
accuracy(forecast_500,head(tail(xts_high,500),500))

ggplot(data.frame(residuals=forecast_500$residuals),aes(residuals))+
  geom_histogram(bins=50,aes(y=..density..),col="red",fill="red",alpha=0.3)+
  geom_density()

checkresiduals(tsforecast500)


forecast_250<-forecast(tsarima250,h=250)
autoplot(forecast_250)
accuracy(forecast_250,head(tail(xts_high,250),250))

ggplot(data.frame(residuals=forecast_250$residuals),aes(residuals))+
  geom_histogram(bins=50,aes(y=..density..),col="red",fill="red",alpha=0.3)+
  geom_density()

checkresiduals()

#Evaluation Visualization
plot(forecast_250)
lines(ts(xts_high))
