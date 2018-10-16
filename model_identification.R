# load data
setwd("~/Desktop/Grad2/TimeSeriesAnal/HW/week3")
library(openxlsx)
library(arima)
library(sarima)
library(forecast)
library(highfrequency)

data2 = read.xlsx("일별 Won_Dollar_환율_2006_2012.xlsx", sheet = 1, colNames = TRUE)
t = data2$Date2; RVt = data2$RVt; Pt = data2$Pt; pt = data2$pt; rt = data2$rt
pacf(RVt)
pacf(rt[-1])

## find aic
myaicftn <- function(yt, i){
  arima.fit = Arima(yt, order=c(i,0,0), method="ML")
  arima.aic = arima.fit$aic
  return(arima.aic)
}
p = seq(0,9,1)
aic_RVt = rep(0,10); aic_rt = rep(0,10)

for(i in 0:9){
  aic_RVt[i+1] = myaicftn(RVt, i)
  aic_rt[i+1] = myaicftn(rt, i)
}
plot(p, aic_RVt, type = "b", main = "AIC(p) of RVt", xlab = "p", ylab = "AIC(p)")
plot(p, aic_rt, type = "b", main = "AIC(p) of rt", xlab = "p", ylab = "AIC(p)")

# find bic
mybicftn <- function(yt, i){
  arima.fit = Arima(yt, order=c(i,0,0), method="ML")
  arima.bic = arima.fit$bic
  return(arima.bic)
}

bic_RVt = rep(0,10); bic_rt = rep(0,10)

for(i in 0:9){
  bic_RVt[i+1] = mybicftn(RVt, i)
  bic_rt[i+1] = mybicftn(rt, i)
}
plot(p, bic_RVt, type = "b", main = "bic(p) of RVt", xlab = "p", ylab = "bic(p)")
plot(p, bic_rt, type = "b", main = "bic(p) of rt", xlab = "p", ylab = "bic(p)")
result_df = data.frame(p, aic_RVt, bic_RVt, aic_rt, bic_rt)

p = seq(0,9,1)
aic_RVt = rep(0,10); aic_rt = rep(0,10)

for(i in 0:9){
  aic_RVt[i+1] = myaicftn(RVt, i)
  aic_rt[i+1] = myaicftn(rt, i)
}

##########
len.t1 = 1516
len.t2 = 1517
len.t3 = 1763
past.RVt = RVt[1:len.t1]   # RVt:1 ~ 1515까지
future.RVt = RVt[len.t1+1: length(RVt)]

# t = 1517예측

arima.fit.RVt.1 = Arima(past.RVt, order=c(7,0,0))
RVt[len.t1+1]
forecast(arima.fit.RVt.1, h=1)
RVt[len.t1+1]-forecast(arima.fit.RVt.1, h=1)$mean[1]

# t = 1518 예측
past.RVt.2 = RVt[1:len.t2]
arima.fit.RVt.2 = Arima(past.RVt.2, order=c(7,0,0))
RVt[len.t2+1]
forecast(arima.fit.RVt.2, h=1)
RVt[len.t2+1]-forecast(arima.fit.RVt.2, h=1)$mean[1]

# t = 1764 예측
past.RVt.3 = RVt[1:len.t3]
arima.fit.RVt.3 = Arima(past.RVt.3, order=c(7,0,0))
RVt[len.t3+1]
forecast(arima.fit.RVt.3, h=1)
RVt[len.t3+1]-forecast(arima.fit.RVt.3, h=1)$mean[1]

# t = 1517 ~ 1764 예측

# t = 1517예측
past.RVt.trans = xts(x = past.RVt, order.by = as.Date(t[1:len.t1]))
har.1 = harModel(past.RVt.trans, periods = c(1,5,22), RVest = c("rCov"), type = "HARRV")
rv1 = past.RVt[len.t1]; rv5 = mean(past.RVt[(len.t1-4):len.t1]); rv22 = mean(past.RVt[(len.t1-21):len.t1])
rv_set = c(1, rv1, rv5, rv22)
sum(har.1$coefficients * rv_set)
RVt[len.t1+1]- pred.1 

# t = 1518 예측
past.RVt.trans.2 = xts(x = past.RVt.2, order.by = as.Date(t[1:len.t2]))
har.2 = harModel(past.RVt.trans.2, periods = c(1,5,22), RVest = c("rCov"), type = "HARRV")
rv1 = past.RVt.2[len.t2]; rv5 = mean(past.RVt.2[(len.t2-4):len.t2]); rv22 = mean(past.RVt.2[(len.t2-21):len.t2])
rv_set = c(1, rv1, rv5, rv22)
sum(har.2$coefficients * rv_set)
RVt[len.t2+1]-pred.2


# t = 1764 예측
past.RVt.trans.3 = xts(x = past.RVt.3, order.by = as.Date(t[1:len.t3]))
har.3 = harModel(past.RVt.trans.3, periods = c(1,5,22), RVest = c("rCov"), type = "HARRV")
rv1 = past.RVt.3[len.t3]; rv5 = mean(past.RVt.3[(len.t3-4):len.t3]); rv22 = mean(past.RVt.3[(len.t3-21):len.t3])
rv_set = c(1, rv1, rv5, rv22)
sum(har.3$coefficients * rv_set)
RVt[len.t3+1]- pred.3 

error.ar = rep(0, 246); error.har = rep(0,246);
pred_len = 246
for(i in 1:pred_len){
  data = RVt[1:(len.t1+i)]
  n =len.t1+i
  arima.fit.RVt = Arima(data, order=c(7,0,0))
  error1 =RVt[n+1]-forecast(arima.fit.RVt, h=1)$mean[1]
  data.trans = xts(x = data, order.by = as.Date(t[1:n]))
  har = harModel(data.trans, periods = c(1,5,22), RVest = c("rCov"), type = "HARRV")
  rv1 = data[n]; rv5 = mean(data[(n-4):n]); rv22 = mean(data[(n-21):n])
  rv_set = c(1, rv1, rv5, rv22)
  error2 = RVt[n+1] - sum(har$coefficients * rv_set) 
  error.ar[i] = error1
  error.har[i] = error2
}

mae = c(mean(abs(error.ar)), mean(abs(error.har)))
mape = c(mean(abs(error.ar)*100/RVt[1518:1763]), mean(abs(error.har)*100/RVt[1518:1763]))  
rmse = c(sqrt(mean(error.ar^2)), sqrt(mean(error.har^2)))


