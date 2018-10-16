
#Packges to Install

install.packages("tseries")
install.packages("DescTools")
install.packages("forecast")
install.packages("TTR")

#Libraries to be added 
library("tseries")
library("stats")
library("data.table")
library(TTR)
library("forecast")
library("DescTools")


## Step 1: Data on monthly demand for a product over 3 years
rm(list=ls(all=TRUE))
setwd("E:/GLIM-PGPBABI/Time-Series Forecasting")
data = read.csv("ProductDemand.csv", header = T)
attach(data)
plot(Demand, type = "l")
Demand.ts = ts(Demand, start = c(2013,1), frequency = 12)
plot(Demand.ts, type = "l")

## Step 2: Simple moving average
smademand= SMA(Demand.ts, n=2)
MAPESMA <- mean(abs(Demand.ts[2:36]-smademand[2:36])/abs(Demand.ts[2:36]))*100
MAPESMA

Theilsma = TheilU(Demand.ts[2:36],smademand[2:36])*100
Theilsma

## Step 3: Weighted moving average
wmademand= WMA(Demand.ts, n=2)
MAPEWMA <- mean(abs(Demand.ts[2:36]-wmademand[2:36])/abs(Demand.ts[2:36]))*100
MAPEWMA

Theilwma = TheilU(Demand.ts[2:36],wmademand[2:36])*100
Theilwma

## Step 4: Exponential Weighted moving average
emademand= EMA(Demand.ts, n=2)
MAPEEMA <- mean(abs(Demand.ts[2:36]-emademand[2:36])/abs(Demand.ts[2:36]))*100
MAPEEMA

Theilema = TheilU(Demand.ts[2:36],emademand[2:36])*100
Theilema

## Step 5: Single exponential smoothing
SES <-   HoltWinters(Demand.ts, 
                  beta=FALSE, 
                  gamma=FALSE,seasonal = "multiplicative")
SES
plot(SES)
SES.predict = forecast(SES, h=12)
accuracy(SES.predict)
SES.predict$fitted

Theilses = TheilU(Demand.ts[2:36],SES.predict$fitted[2:36])*100
Theilses

##Step 6: Double exponential smoothing
DES <-   HoltWinters(Demand.ts,gamma=FALSE,seasonal = "multiplicative")
DES
plot(DES)

DES.predict = forecast(DES, h=12)
accuracy(DES.predict)
DES.predict$fitted
Theildes = TheilU(Demand.ts[3:36],DES.predict$fitted[3:36])*100
Theildes

##Step 7: Triple exponential smoothing
TES <-   HoltWinters(Demand.ts) 
                     
TES$SSE
plot(TES)
TES.predict = forecast(TES, h=12)
accuracy(TES.predict)
TES.predict$fitted
Theiltes = TheilU(Demand.ts[13:36],TES.predict$fitted[13:36])*100
Theiltes

## Step 8: Calculate ACF & PACF
acf(Demand.ts)
pacf(Demand.ts)


##Step 9: ARMA
arma = Arima(Demand.ts, order=c(1,0,2), include.drift = FALSE)
arma
summary(arma)
arma.predict = forecast(arma,12)
arma$fitted

Theilarma = TheilU(Demand.ts[1:36],arma$fitted[1:36])*100
Theilarma

##Step 10: Autoarima
autoarma = auto.arima(Demand.ts)
autoarma
summary(autoarma)
autoarma.predict = forecast(autoarma,12)
autoarma$fitted

Theilautoarma = TheilU(Demand.ts[1:36],autoarma$fitted[1:36])*100
Theilautoarma

plot(Demand.ts, type="l", col="black")
lines(smademand, col="red", lwd=2)
lines(wmademand, col="blue")
lines(emademand, col="green")
plot(TES.predict, col="green")

#Final Output
##MAPE
##MAPESMA   7.746316
##MAPEWMA   5.164211
##MAPEEMA   5.54877
##SES       16.58058
##DES       15.47498
##TES       7.253226
##ARMA      16.40653
##AutoARIMA 4.848526


##Theil
#Theilsma      0.1019227
#Theilwma      0.06794844
#Theilema      0.06474324
#Theilses      0.1938525
#Theildes      0.1867359
#Theiltes      0.07576728
#Theilarma     0.1940906
#Theilautoarma 0.06842064
