# A. Daily log returns

# load data
setwd("~/Desktop/Grad2/TimeSeriesAnal/HW/week1")
library(openxlsx)
library(nortest)
library(dplyr)
library(magrittr)
library(arima)
library(sarima)
data1 = read.xlsx("Week1_KOSPI.xlsx", sheet = 1, colNames = TRUE)
colnames(data1) = c("x1", "time", "pt", "rt")
num_k = 20
rt = data1$rt

# Set function
mySACF <- function(rt, k){
  rt.k = rt[(1+k):length(rt)]
  rt.0 = rt[1:(length(rt)-k)]
  gamma.0 = sum((rt-mean(rt))^2) 
  gamma.k = sum((rt.0-mean(rt.0))*(rt.k-mean(rt.k)))
  return(gamma.k/gamma.0+ 10^(-8))
}

rho_list = c()
for(i in 1:num_k){
  rho_list[i] = mySACF(rt, i)
}

std.SACF<- function(rt, k){
  rho_list_sub = rho_list[1:k-1]
  std = sqrt((1/length(rt))*(1+2*sum(rho_list_sub^2)))
  return(std)
}

SPACF.k.j <- function(k, j, rt){
  if(k == 1){
    return(mySACF(rt,1))
  }
  else{
    return(SPACF.k.j(k-1, j, rt) - mySPACF(rt,k)*SPACF.k.j(k-1, k-j, rt))
  }
}

mySPACF <- function(rt, k){
  if(k == 1){
    return(mySACF(rt, 1))
  }
  else{
    num.sum = 0; denom.sum = 0
    for(j in 1:(k-1)){
      num.sum =+ SPACF.k.j(k-1,j,rt)*mySACF(rt, k-j)
    }
    for(l in 1:(k-1)){
      denom.sum =+ SPACF.k.j(k-1,l,rt)*mySACF(rt, l)
    }
    return((mySACF(rt, k) - num.sum)/(1-denom.sum + 10^(-8)))
  }
}

std.SPACF = sqrt(1/length(rt))

# using packages
rho_rt = acf(rt, num_k)
phi_rt = pacf(rt, num_k)

#2. SACF, SPACF; dataframe
DF_data1<-data.frame('SACF' = rep(0, num_k), 'std.SACF' = rep(0,num_k), 'SPACF' = rep(0, num_k), 'std.SPACF' = rep(0, num_k))
for (i in 1:num_k){
  DF_data1[i,1] = rho_rt$acf[i+1]
  DF_data1[i,2] = std.SACF(rt, i)
  DF_data1[i,3] = phi_rt$acf[i]
  DF_data1[i,4] = std.SPACF
}

#3. white noise test
rt.wntest <- whiteNoiseTest(autocorrelations(rt,20), h0 = "iid", nlags = c(1:20), x = autocorrelations(rt,20))

#4. time_lag; 10
Pt.10.index = data1$pt[seq(1,length(rt),10)]
rt.10 = rep(0, length(Pt.10.index)-1)
for(i in 1:(length(Pt.10.index)-1)){
  rt.10[i] = log(Pt.10.index[i+1])-log(Pt.10.index[i])
}

# using my function
mySACF(rt.10, 1)
std.SACF(rt.10,1)
rt.10.wntest <- whiteNoiseTest(acf.10, h0 = "iid", nlags = c(1,2,3), x = acf.10)

# package; sarima
acf.10 = autocorrelations(rt.10, 10)
plot(acf.10)

# B. Prediction of realized volatilities
data2 = read.xlsx("Week1_KOSPI.xlsx", sheet = 2, colNames = FALSE)
colnames(data2) = c("yt")
num_k = 20
yt = data2$yt

std.SPACF.yt = sqrt(1/length(yt))
rho_yt = acf(yt, num_k)
phi_yt = pacf(yt, num_k)

DF_data2<-data.frame('SACF' = rep(0, num_k), 'std.SACF' = rep(0,num_k), 'SPACF' = rep(0, num_k), 'std.SPACF' = rep(0, num_k))
for (i in 1:num_k){
  DF_data2[i,1] = rho_yt$acf[i+1]
  DF_data2[i,2] = std.SACF(yt, i)
  DF_data2[i,3] = phi_yt$acf[i]
  DF_data2[i,4] = std.SPACF.yt
}

# lag_10
data3 = read.xlsx("Week1_KOSPI.xlsx", sheet = 3, colNames = TRUE)
colnames(data3) = c("date","yt.10")
yt.10 = data3$yt.10
acf.yt.10 = autocorrelations(yt.10, 10)
plot(acf.yt.10)
yt.10.wntest <- whiteNoiseTest(acf.yt.10, h0 = "iid", nlags = c(1,2,3), x = acf.yt.10)
