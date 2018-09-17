# load data
setwd("~/Desktop/Grad2/TimeSeriesAnal/HW/week2")
library(openxlsx)
library(nortest)
library(arima)
library(sarima)
require(ggplot2)
library(forecast)
data1 = read.xlsx("MA_simulation.xlsx", sheet = 2, colNames = TRUE)
colnames(data1) = c("t", "at", "yt.1", "yt.2","yt.3", "yt.4", "yt.withat.prob2")
t = data1$t; at = data1$at; yt.1 = data1$yt.1; yt.2 = data1$yt.2; yt.3 = data1$yt.3; yt.4 = data1$yt.4
phi.1 = c(-0.8, 0.8, 1, 1.05)

# 시도표
plot(data1$t,data1$yt.1,type='l',xlab="x",ylab="y(t)",main="phi: -0.8")
acf(yt.1)
pacf(yt.1)

plot(data1$t,data1$yt.2,type='l',xlab="x",ylab="y(t)",main="phi: 0.8")
acf(yt.2)
pacf(yt.2)

plot(data1$t,data1$yt.3,type='l',xlab="x",ylab="y(t)",main="phi: 1")
acf(yt.3)
pacf(yt.3)

plot(data1$t,data1$yt.4,type='l',xlab="x",ylab="y(t)",main="phi: 1.05")
acf(yt.4)
pacf(yt.4)

# 2.
acf(data1$yt.withat.prob2, 50)

# 3.
data2 = read.xlsx("Week1_KOSPI.xlsx", sheet = 3, colNames = TRUE)
date.2 = data2$DateID; RV.10 = data2$RV_10
data.fit.fixed = RV.10[1:1723]
data.month.12 = RV.10[1724:1741]

## AR(2)
#install.packages("forecast")
#arima.fit.1 = Arima(data.fit, order=c(1,0,0))
#forecast(arima.fit.1, h=18) # arima 모형 fitting 함수 (Data에 시계열 자료를 넣고 order= c(AR order, the degree of differencing, MA order) 정해줌)

ar1.1 = arima.fit$coef[1]
sigma.a = ar.ols(data.fit.fixed, order=1, demean=F, intercept=T)$var.pred
mu1 = arima.fit$coef[2]
ar1.0 = ar.ols(data.fit.fixed, order=1, demean=F, intercept=T)$x.intercept  #mu1*(1-ar1.1)

## predict
data.predict.1 = rep(0,18)
for(i in 1:18){
  data.fit = RV.10[1:(1722+i)]
  arima.fit.1 = Arima(data.fit, order=c(1,0,0))
  print(forecast(arima.fit.1, h=1))
  #print(c(forecast(arima.fit.1, h=1)$mean[1], 1722+i))
  data.predict.1[i] =forecast(arima.fit.1, h=1)$mean[1]
}

###predict_value vs real_value
x = 1:18
df.plot1.test <- data.frame(x,data.month.12,data.predict.1)
ggplot(df.plot1, aes(x)) +                    # basic graphical object
  geom_line(aes(y=data.month.12), lty = 1) +  # first layer
  geom_line(aes(y=data.predict.1),lty = 2)  # second layer

### reisidual
plot(data.month.12-data.predict.1, type = 'l')

### mae&mse of AR(1)
mae.ar1 = mean(abs(data.month.12-data.predict.1))
mse.ar1 = mean((data.month.12-data.predict.1)^2)

## AR(5)
arima.fit.5 = Arima(data.fit.fixed, order=c(5,0,0))
ar.ols(data.fit.fixed, order=1, demean=F, intercept=T)

## predict
data.predict.5 = rep(0,18)
for(i in 1:18){
  data.fit = RV.10[1:(1722+i)]
  arima.fit = Arima(data.fit, order=c(5,0,0))
  #print(forecast(arima.fit.1, h=1)$mean[1])
  data.predict.5[i] =forecast(arima.fit, h=1)$mean[1]
}

###predict_value & real_value
df.plot5 <- data.frame(x,data.month.12,data.predict.5)
ggplot(df.plot5, aes(x)) +                    # basic graphical object
  geom_line(aes(y=data.month.12), lty = 1) +  # first layer
  geom_line(aes(y=data.predict.5),lty = 2)  # second layer

### reisidual
plot(data.month.12-data.predict.5, type = 'l')

### mae&mse of AR(5) model
mae.ar5 = mean(abs(data.month.12-data.predict.5))
mse.ar5 = mean((data.month.12-data.predict.5)^2)



##############################
install.packages("forecast")
# arima 모형 fitting 함수 
#Data에 시계열 자료를 넣고 order= c(AR order, the degree of differencing, MA order) 
arima.fit = Arima(Data, order=c(1,0,0))
forecast(arima.fit, h=20)    # h = period of prediction 

