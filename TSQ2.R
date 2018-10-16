
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
library("lmtest")

rm(list=ls(all=TRUE))
setwd("E:/GLIM-PGPBABI/Time-Series Forecasting")
Dataset=read.csv("TRPRatings.csv",header=TRUE)
attach(Dataset)
head(Dataset)

#Step 1: OLS using the LM Function
tvtrp=lm(TRP~ Episode,data = Dataset)
summary(tvtrp)


#Step 2: Add regression line 
plot(Dataset)
lines(Dataset$Episode, predict(tvtrp), type="l", col="red",lwd=5)

#Step 3: To find Auto-Correlation in the dataset, Durbin-Watson statistic and find hypothesis
require(lmtest)
dwtest(tvtrp)

# Step 4: Convert data into Time Series
par(mfrow = c(1, 1))
TRPRatings<-read.csv("TRPRatings.csv")
TRPRatings<-ts(TRPRatings[,2],frequency = 365)
TRPRatings
plot(TRPRatings, xlab="Episode", ylab = "TRPRatings")

# Step 5: Difference data to make it stationary
#ndiffs(TRPRatings)
plot(diff(TRPRatings),ylab="TRPRatings")

# Step 6: Plot ACF/PACF to identify candidates for AR, MA or ARMA models
par(mfrow = c(1,2))
acf(ts(diff((TRPRatings))),main="ACF TRPRatings")
pacf(ts(diff((TRPRatings))),main="PACF TRPRatings")

# Step 7: Build ARIMA model
TRPRatingsARIMA <- auto.arima(TRPRatings)
TRPRatingsARIMA

# Step 8: Check residuals to ensure they are white noise
par(mfrow=c(1,2))
acf(ts(TRPRatingsARIMA$residuals),main="ACF Residual")
pacf(ts(TRPRatingsARIMA$residuals),main="PACF Residual")
Box.test(TRPRatingsARIMA$residuals, lag=24, type="Ljung-Box")


# Step 9: Forecast TRP Ratings
par(mfrow = c(1, 1))
TRPRatingsForecast <- forecast(TRPRatingsARIMA, h=31)
plot(TRPRatingsForecast)
TRPRatingsForecast

# Step 10:Alternative method
pred <- predict(TRPRatingsARIMA, n.ahead=31)
pred

#Step 11 : Find Probability 
m1 <- mean(Dataset$TRP)
sd1 <- sd(Dataset$TRP)
pnorm(10, m1, sd1, lower.tail = FALSE)



