# load data
setwd("~/Desktop/Grad2/TimeSeriesAnal/HW/week3")
library(openxlsx)
library(nortest)
library(arima)
library(sarima)
require(ggplot2)
library(forecast)
library(moments)
library(fUnitRoots)
data1 = read.xlsx("MA_simulation.xlsx", sheet = 5, colNames = TRUE)
colnames(data1) = c("at", "t", "yt.wo.at", "yt.1","rho", "yt.2", "yt.3", "wt.1", "wt.2", "wt.3")
t = data1$t; at = data1$at; yt.1 = data1$yt.1; yt.2 = data1$yt.2; yt.3 = data1$yt.3
wt.1 = data1$wt.1; wt.2 = data1$wt.2; wt.3 = data1$wt.3
phi.1 = c(-0.8, 0.8, 1, 1.05)
wt.1 = wt.1[-1]; wt.2 = wt.2[-1]; wt.3 = wt.3[-1]

# 시도표
plot(data1$t,data1$yt.1,type='l',xlab="x",ylab="y(t)",main="phi: -0.8")
acf(yt.1)
pacf(yt.1)

plot(data1$t,data1$yt.2,type='l',xlab="x",ylab="y(t)",main="phi: 0")
acf(yt.2)
pacf(yt.2)

plot(data1$t,data1$yt.3,type='l',xlab="x",ylab="y(t)",main="phi: 0.8")
acf(yt.3)
pacf(yt.3)

plot(data1$t,data1$yt.4,type='l',xlab="x",ylab="y(t)",main="phi: 1.05")
acf(yt.4)
pacf(yt.4)

#1_4) wt의 sacf
acf(wt.1)
acf(wt.2)
acf(wt.3)

# 2.
data2 = read.xlsx("일별 Won_Dollar_환율_2006_2012.xlsx", sheet = 1, colNames = TRUE)
t = data2$Date2; RVt = data2$RVt; Pt = data2$Pt; pt = data2$pt; rt = data2$rt
plot(data2$Date2,data2$RVt,type='l',xlab="t",ylab="RVt",main="RV")
plot(data2$Date2,data2$pt,type='l',xlab="t",ylab="pt",main="exchange rate")
plot(data2$Date2,data2$rt,type='l',xlab="t",ylab="rt",main="log exchange rate")
summary(data2); skewness(data2, na.rm = TRUE); kurtosis(data2, na.rm = TRUE)
shapiro.test(pt); shapiro.test(rt[-1]); shapiro.test(RVt)

acf(pt)
acf(rt[-1])
acf(RVt)
acf(data1$yt.withat.prob2, 50)

# n+1예측
arima.fit.pt.2 = Arima(pt, order=c(1,0,0))
forecast(arima.fit.pt.2, h=1)
arima.fit.pt.5 = Arima(pt, order=c(5,0,0))
forecast(arima.fit.pt.5, h=1)
arima.fit.pt.22 = Arima(pt, order=c(22,0,0))
forecast(arima.fit.pt.22, h=1)

rt = rt[-1]
arima.fit.rt.2 = Arima(rt, order=c(1,0,0))
forecast(arima.fit.rt.2, h=1)
arima.fit.rt.5 = Arima(rt, order=c(5,0,0))
forecast(arima.fit.rt.5, h=1)
arima.fit.rt.22 = Arima(rt, order=c(22,0,0))
forecast(arima.fit.rt.22, h=1)

arima.fit.RVt.2 = Arima(RVt, order=c(1,0,0))
forecast(arima.fit.RVt.2, h=1)
arima.fit.RVt.5 = Arima(RVt, order=c(5,0,0))
forecast(arima.fit.RVt.5, h=1)
arima.fit.RVt.22 = Arima(RVt, order=c(22,0,0))
forecast(arima.fit.RVt.22, h=1)

# 3.
# ADF_unitroottest: adfTest(RVt, lags = 1, type = "c", description = NULL)
# type : "c" = 절편이 있는 상수모형 검정 
# type :  "nc" = 절편이 없는 상수모형 검정
# type : "ct" = 추세모형 검정 

## find aic
myaicftn <- function(yt, i){
  arima.fit = Arima(yt, order=c(i,0,0), method="ML")
  arima.aic = arima.fit$aic
  return(arima.aic)
}

## find adfstat. and p_value
unitrootftn <- function(yt, i){
  adf.test = adfTest(yt, lags = i-1, type = "c") 
  adf.stat = adf.test@test$statistic           # ADF 값
  adf.p.value = adf.test@test$p.value          # p-value
  return(c(adf.stat,adf.p.value))
}

p = seq(0,8,1)
aic_RVt = rep(0,9); aic_pt = rep(0,9); aic_rt = rep(0,9)
pval_RVt =rep(0,9); pval_pt =rep(0,9); pval_rt =rep(0,9)
adf_RVt = rep(0,9); adf_pt = rep(0,9); adf_rt = rep(0,9)

for(i in 0:8){
  aic_RVt[i+1] = myaicftn(RVt, i); aic_pt[i+1] = myaicftn(pt, i) 
}
for(i in 1:8){
  pval_RVt[i+1] = unitrootftn(RVt, i)[2]; pval_pt[i+1] = unitrootftn(pt, i)[2]
  adf_RVt[i+1] = unitrootftn(RVt, i)[1]; adf_pt[i+1] = unitrootftn(pt, i)[1]
}
for(i in 0:8){
  aic_rt[i+1] = myaicftn(rt, i)
}
for(i in 1:8){
  pval_rt[i+1] = unitrootftn(rt, i)[2]
  adf_rt[i+1] = unitrootftn(rt, i)[1]
}

df.unitroot.RVt = data.frame(p, adf_RVt, pval_RVt, aic_RVt)
df.unitroot.pt = data.frame(p, adf_pt, pval_pt, aic_pt)
df.unitroot.rt = data.frame(p, adf_rt, pval_rt, aic_rt)

