# load data
setwd("~/Desktop/Grad2/TimeSeriesAnal/HW/week1")
library(openxlsx)
library(nortest)
library(dplyr)
library(magrittr)
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


#2. SACF, SPACF; dataframe
DF_data1<-data.frame('SACF' = rep(0, num_k), 'std.SACF' = rep(0,num_k), 'SPACF' = rep(0, num_k), 'std.SPACF' = rep(0, num_k))
for (i in 1:num_k){
  DF_data1[i,1] = mySACF(rt, i)
  DF_data1[i,2] = std.SACF(rt, i)
  DF_data1[i,3] = mySPACF(rt, i)
  DF_data1[i,4] = std.SPACF
}

#4.time_lag; 10
Pt.10.index = data1$pt[seq(1,length(rt),10)]
rt.10 = rep(0, length(Pt.10.index)-1)
for(i in 1:(length(Pt.10.index)-1)){
  rt.10[i] = log(Pt.10.index[i+1])-log(Pt.10.index[i])
}
mySACF(rt.10, 1)
std.SACF(rt.10,1)
