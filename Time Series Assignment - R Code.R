library(xlsx)
mydf <- read.xlsx("MarketMixModel.xlsx",header=TRUE, sheetIndex = 1)
head(mydf)

summary(mydf)
plot(mydf[,1], type ="l")
# mid year drop, spike at year end

tmydf <- ts(mydf, start=c(2014, 1), end=c(2015, 12), frequency=12)
tmydf

plot(tmydf)
m = lm(tmydf~time(tmydf))
abline(m)
#increasing mean over time

#seasonal plots
library(forecast)
seasonplot(tmydf, col = c("red","blue","green"), main="Seasonal Variations")

#decompose time series
#dmydf = stl(tmydf[,1], s.window = 12)
#plot(dmydf)

dmydf = decompose(tmydf, type="multiplicative")
dmydf$seasonal
plot(dmydf)

#Seasonal Index
dmydf$figure

# seasonilty index by moving average
mydfma= ma(tmydf,order = 12, centre = T)
mademand=tmydf/mydfma
ma_demand=t(matrix(data=mademand, nrow=12))
seasonal_demand=colMeans(ma_demand,na.rm = T)
seasonal_demand

ts_calculations <- data.frame( DetrendedData =dmydf$x/dmydf$seasonal, SeasonalIndex = dmydf$seasonal)
ts_calculations

seasadj(dmydf)
plot(seasadj(dmydf))

library(tseries)
adf.test(na.omit(dmydf$random))
acf(na.omit(dmydf$random))
pacf(na.omit(dmydf$random))

#differencing since above was not stationary
diff1 = diff(dmydf$random, differences = 1)
adf.test(na.omit(diff1))
plot(diff1)

acf(na.omit(diff1))
pacf(na.omit(diff1))

#https://cran.r-project.org/web/packages/AnalyzeTS/AnalyzeTS.pdf
library(forecast)
library(AnalyzeTS)
actual = data.frame(mydf)

mydfma<- ma(tmydf, order=12) 
plot(mydfma)
result = abs((mydfma-actual[,1])/actual[,1])*100
colMeans(result,na.rm = TRUE) # MAPE value
resid = data.frame(mydfma)
av.res(actual,resid)

sma<-SMA(tmydf,12)
result = abs((sma-actual[,1])/actual[,1])*100
mean(result,na.rm = TRUE) # MAPE value

cma<-CMA(tmydf,12)
result = abs((cma-actual[,1])/actual[,1])*100
mean(result,na.rm = TRUE) # MAPE value

forecasted<-data.frame(sma,cma,mydfma)
av.res(Y=actual,F=forecasted,r=5)

# Arima model
auar = auto.arima(tmydf[,1])
resid = data.frame(auar$residuals)
av.res(Y=actual,E=resid)

#Exponential State Smoothing
s_ets  = ets(tmydf[,1])
resid = data.frame(s_ets$residuals)
av.res(Y=actual,E = resid)
plot(s_ets)

