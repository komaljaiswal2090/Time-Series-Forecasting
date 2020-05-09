#------------------------------Begin----------------------------#

# Load Forecast and Zoo Libraries

library(forecast)
library(zoo)
Yellowstone.data<-read.csv("~/Desktop/Time Series/Project/Project.csv")
Yellowstone.data
Yellowstone.data$Recreation.Visits

# Create Time Series Data using ts() function
Yellowstone.ts <- ts(Yellowstone.data$Recreation.Visits, 
                     start = c(1986, 1), end = c(2016, 12), freq = 12)
Yellowstone.ts

#------------------Exploratory Data Analysis-------------------#

#*******Data Plot*******#
# Plot the time series data using plot() function
plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No. of Visits", xaxt = "n",
     ylim = c(5000, 1000000), main = "Visitors in Yellowstone National Park", col ="blue" )
axis(1, at = seq(1986, 2016, 1), labels = format(seq(1986, 2016,1)) )

#*******Time Series Components Plot*******#
# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
Yellowstone.stl <- stl(Yellowstone.ts, s.window = "periodic")
autoplot(Yellowstone.stl, main = "Yellowstone Visitors Times Series Components")

#*******Seasonal Plot*******#
# Create seasonal plot to see how seasons affect the visitors in 
# Yellowstone National Park
library(fma)
seasonplot(Yellowstone.ts, ylab="No. of Visitors", xlab="Months", 
           year.labels.left=TRUE, main="Visitors Seasonal Plot", col=1:20, pch=19)

#*******Autocorrelation Chart*******#
# Use acf() function to identify autocorrealtion and plot autocorrrelation
# for different lags (up to maximum of 12).
autocor <- Acf(Yellowstone.ts, lag.max = 12, main = "Autocorrelation for Yellowstone data")
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#*******Predictability Test*******#
## Test Predictability OF Yellowstone Visitors.
# Approach1
# Use Arima() function to fit AR(1) model for regression residulas.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
Yellowstone.ts.ar1<- Arima(Yellowstone.ts, order = c(1,0,0))
summary(Yellowstone.ts.ar1)

# Approach2
# Create differenced data using (lag-1)
diff.Yellowstone.ts <- diff(Yellowstone.ts, lag = 1)
diff.Yellowstone.ts

# Use Acf() function to identify autocorrealtion for the model residuals 
Acf(diff.Yellowstone.ts, lag.max = 12, 
    main = "Autocorrelation for Yellowstone Differenced Visitors")

#--------------------------Partioning---------------------------#

## Create Times Series Parition.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(ridership.ts) = 372.
# nvalid = 60 months for the last 5 years from Jan 2012 to Dec 2016
# nTrain = 312 months from Jan 1986 to Dec 2011
Yellowstone.nValid <- 60
Yellowstone.nTrain <- length(Yellowstone.ts) - Yellowstone.nValid
Yellowstone.train.ts <- window(Yellowstone.ts, start = c(1986, 1), end = c(1986, Yellowstone.nTrain))
Yellowstone.valid.ts <- window(Yellowstone.ts, start = c(1986, Yellowstone.nTrain + 1), 
                               end = c(1986, Yellowstone.nTrain + Yellowstone.nValid))
Yellowstone.train.ts
Yellowstone.valid.ts

#--------------------------Naive Forecast---------------------------#

# Naive Forecast 
Yellowstone.naive.pred <- naive(Yellowstone.ts, h = 12)
Yellowstone.naive.pred 

# Seasonal Naive Forecast 
Yellowstone.snaive.pred <- snaive(Yellowstone.ts, h = 12)
Yellowstone.snaive.pred 

# Comparing Accuracy for Seasonal Naive and Naive
round(accuracy(Yellowstone.naive.pred$fitted,Yellowstone.ts) ,3)
round(accuracy(Yellowstone.snaive.pred $fitted,Yellowstone.ts) ,3)

#-----------------------Exponential Smoothing------------------------#

#***********Holt Winter's Automated Selection ************#

# Holt Winter's Exponential Smoothing for training data.
HW.ZZZ <- ets(Yellowstone.train.ts, model= "ZZZ")
HW.ZZZ

# Forecast to make predictions using this HW's model with Validation Period
HW.ZZZ.Pred <- forecast(HW.ZZZ,h=Yellowstone.nValid,level=0)
HW.ZZZ.Pred

# Holt WInters exponential smoothing for historical data
Hist.HW.ZZZ <- ets(Yellowstone.ts,model= "ZZZ")
Hist.HW.ZZZ

# Forecast to make predictions using this HW's model for future 12 periods
Hist.HW.ZZZ.Pred <- forecast(Hist.HW.ZZZ,h=12,level=0)
Hist.HW.ZZZ.Pred

# Accuracy for Holt-Winter's model for the validation period.
round(accuracy(HW.ZZZ.Pred$mean,Yellowstone.valid.ts),3)

# Accuracy for Holt-Winter's model for Entire historical dataset
round(accuracy(Hist.HW.ZZZ.Pred$fitted,Yellowstone.ts),3)
round(accuracy((naive(Yellowstone.ts))$fitted,Yellowstone.ts),3)
round(accuracy((snaive(Yellowstone.ts))$fitted,Yellowstone.ts),3)

#--------------------------Regression Models---------------------------#


#*************** Regression with Seasonality***************#

#Regression model with seasonality for the Training Dataset.
Yellowstone.train.season <- tslm(Yellowstone.train.ts ~ season)
summary(Yellowstone.train.season)

#Create forecast for the validation period.
Yellowstone.train.season.pred <- forecast(Yellowstone.train.season ,h=Yellowstone.nValid,level=0)
Yellowstone.train.season.pred 

# plot for regression model with seasonality 
plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No of Visits", xaxt = "n",
     ylim = c(5000, 1200000), main = "Regression model with Seasonality", col ="blue",lwd=1)
axis(1, at = seq(1986, 2017, 1), labels = format(seq(1986, 2017,1)) )
lines(Yellowstone.trend.season.pred $fitted, col = "brown", lwd = 2,lty=2)
lines(Yellowstone.trend.season.pred $mean, col = "black", lty = 5, lwd = 1)
lines(c(2012, 2012), c(0, 1200000),lwd=2)
lines(c(2017,2017), c(0, 1200000),lwd=2)
text(1996.25, 1100000, "Training")
text(2014.5 ,1100000, "Validation")
arrows(1986, 1000000+20000, 2011, 1000000+20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25 , 1000000+20000, 2016.75, 1000000+20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#***************Quadratic Trend and Seasonality***************#

#Regression model with Quadratic trend and seasonality for Training Dataset.
Yellowstone.quad.train.trend.season <- tslm(Yellowstone.train.ts ~ trend + I(trend^2) +season)
summary(Yellowstone.quad.train.trend.season)

# Create forecast for the validation period.
Yellowstone.quad.train.trend.season.pred <- forecast(Yellowstone.quad.train.trend.season,h=Yellowstone.nValid,level=0)
Yellowstone.quad.train.trend.season.pred 

#Regression with quadratic trend and seasonality 
plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No of Visits", xaxt = "n",
     ylim = c(5000, 1200000), main = "Regression model with quadratic trend and seasonality", col ="blue",lwd=1)
axis(1, at = seq(1986, 2017, 1), labels = format(seq(1986, 2017,1)) )
lines(Yellowstone.quad.train.trend.season.pred $fitted, col = "brown", lwd = 2,lty=2)
lines(Yellowstone.quad.train.trend.season.pred $mean, col = "black", lty = 5, lwd = 1)
lines(c(2012, 2012), c(0, 1200000),lwd=2)
lines(c(2017,2017), c(0, 1200000),lwd=2)
text(1996.25, 1100000, "Training")
text(2014.5 ,1100000, "Validation")
arrows(1986, 1000000+20000, 2011, 1000000+20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2012.25 , 1000000+20000, 2016.75, 1000000+20000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

#Comparision of Regression Models for partitioned data 
round(accuracy(Yellowstone.season.pred$mean,Yellowstone.valid.ts ),3)
round(accuracy(Yellowstone.quad.train.trend.season.pred$mean,Yellowstone.valid.ts ),3)

#****************Regression for Entire Data Set*************#

# Regression with seasonality.
Yellowstone.season <- tslm(Yellowstone.ts ~ season)
summary(Yellowstone.season)

# Create forecast for the 12 periods into the future.
Yellowstone.season.pred <- forecast(Yellowstone.season,h=12,level=0)
Yellowstone.season.pred

#plot for Regression model with seasonality for entire data

plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No of Visits", xaxt = "n",
     ylim = c(5000, 1200000), main = "Regression model with Seasonality (Entire data)", col ="blue",lwd=1)
axis(1, at = seq(1986, 2017, 1), labels = format(seq(1986, 2017,1)) )
lines(Yellowstone.season.pred$fitted , col = "brown", lwd = 2,lty=2)
lines(Yellowstone.season.pred$mean , col = "black", lty = 5, lwd = 1)
legend(1992,1000000+200000, legend = c("Historical data", 
                                       "Forecast of Historical data", "Forecast of future data "), 
       col = c("blue", "brown" , "black"), 
       lty = c(1, 2, 5), lwd =c(1, 2, 1), bty = "n")

#--------------------------Two Level Models---------------------------#

#****Two Level for Regression with Seasonality for Partitioned data ****#

#Residuals of Regression model with seasonality for Partitioned data.
Yellowstone.train.seas.res <- Yellowstone.train.season$residuals
Yellowstone.train.seas.res

#Apply trailing MA to the residuals 
ma.trailing.train.res_12 <- rollmean(Yellowstone.train.seas.res,k=12,align="right")
ma.trailing.train.res_12

#Create forecast for residuals for the validation period.
ma.trailing.train.res_12.pred<- forecast(ma.trailing.train.res_12,h=Yellowstone.nValid,level=0)
ma.trailing.train.res_12.pred 

#Combine regression forecast and trailing ma forecast for residuals
ts.forecast.12 <- Yellowstone.trend.season.pred$mean + ma.trailing.train.res_12.pred$mean
ts.forecast.12

# Create a table with regression forecast, trailing MA for residuals
# and total forecast for the validation period.
total.reg.train.ma.pred <- data.frame(Yellowstone.trend.season.pred$mean, 
                                      ma.trailing.train.res_12.pred$mean, 
                                      ts.forecast.12)
names(total.reg.train.ma.pred) <- c("Regression.Forecast", 
                              "Residuals.Forecast", "Combined.Forecast")
total.reg.train.ma.pred

#*******Two Level for Entire Data ******#

#Apply the two-level forecast with regression and trailing MA 
#for residuals for the Entire Dataset.

#Residuals of Regression with Seasonality for entire data
Yellowstone.season.res <- Yellowstone.season$residuals
Yellowstone.season.res

#Apply trailing MA to the residuals 
ma.trailing.seas.res_12 <- rollmean(Yellowstone.season.res,k=12,align="right")
ma.trailing.seas.res_12

#Create forecast for residuals for the 12 periods in future
ma.trailing.seas.res_12.pred<- forecast(ma.trailing.seas.res_12,h=12,level=0)
ma.trailing.seas.res_12.pred

#Combine regression forecast and trailing ma forecast for residuals
ts.forecast.12 <- Yellowstone.season.pred$mean + ma.trailing.seas.res_12.pred$mean
ts.forecast.12

#Create a table with regression forecast, trailing MA for residuals
#and total forecast for 12 months into the future
total.reg.ma.pred <- data.frame(Yellowstone.season.pred$mean,
                                ma.trailing.seas.res_12.pred$mean,ts.forecast.12)
names(total.reg.ma.pred) <- c("Regression.Forecast", 
                     "Residuals.Forecast", "Combined.Forecast")
total.reg.ma.pred

#Accuracy for Two level model( Regression model with seasonality+trailing MA for residuals)
#for the 12 periods into future
round(accuracy(Yellowstone.season.pred$fitted + ma.trailing.seas.res_12,Yellowstone.ts),3)
round(accuracy((naive(Yellowstone.ts))$fitted,Yellowstone.ts),3)
round(accuracy((snaive(Yellowstone.ts))$fitted,Yellowstone.ts),3)

#Two level forecast(Regression with Seasonality and Trailing MA for residuals)
plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No of Visits", xaxt = "n",
     ylim = c(5000, 1200000), main = "Regression with Seasonality and Trailing MA (Entire data)", col ="blue",lwd=1)
axis(1, at = seq(1986, 2017, 1), labels = format(seq(1986, 2017,1)) )
lines(Yellowstone.season.pred$fitted + ma.trailing.seas.res_12.pred$fitted, col = "brown", lwd = 2,lty=2)
lines(Yellowstone.season.pred$mean + ma.trailing.seas.res_12.pred$mean, col = "black", lty = 5, lwd = 1)
legend(1992,1000000+200000, legend = c("Historical data", 
                                       "Forecast of Historical data", "Forecast of future data "), 
       col = c("blue", "brown" , "black"), 
       lty = c(1, 2, 5), lwd =c(1, 2, 1), bty = "n")

#------------------------------ARIMA Models--------------------------------#

#**********FIT AR(2) MODEL**********#

# Use Arima() function to fit AR(2) model.
# The ARIMA model of order = c(2,0,0) gives an AR(2) model.
# Use summary() to show AR(2) model and its parameters.
Yellowstone.train.ar2 <- Arima(Yellowstone.train.ts, order = c(2,0,0))
summary(Yellowstone.train.ar2)

# Apply forecast() function to make predictions for ts with 
# AR model in validation set.   
Yellowstone.train.ar2.pred <- forecast(Yellowstone.train.ar2, h = Yellowstone.nValid, level = 0)
Yellowstone.train.ar2.pred

#**********FIT MA(2) MODEL**********#

# Use Arima() function to fit MA(2) model.
# The ARIMA model of order = c(0,0,2) gives an MA(2) model.
# Use summary() to show MA(2) model and its parameters.
Yellowstone.train.ma2<- Arima(Yellowstone.train.ts, order = c(0,0,2))
summary(Yellowstone.train.ma2)

# Apply forecast() function to make predictions for ts with 
# MA model in validation set.    
Yellowstone.train.ma2.pred <- forecast(Yellowstone.train.ma2, h = Yellowstone.nValid, level = 0)
Yellowstone.train.ma2.pred

#**********FIT ARMA(2,2) MODEL**********#

# Use Arima() function to fit ARMA(2,2) model.
# The ARIMA model of order = c(2,0,2) gives an ARMA(2,2) model.
# Use summary() to show ARMA model and its parameters.
Yellowstone.train.arma2 <- Arima(Yellowstone.train.ts, order = c(2,0,2))
summary(Yellowstone.train.arma2)

# Apply forecast() function to make predictions for ts with 
# ARMA model in validation set.    
Yellowstone.train.arma2.pred <- forecast(Yellowstone.train.arma2, h = Yellowstone.nValid, level = 0)
Yellowstone.train.arma2.pred

#**********FIT ARIMA(2,1,2)(1,1,2) MODEL**********#

# Use Arima() function to fit ARIMA(2,1,2)(1,1,2) model for trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
Yellowstone.train.arima <- Arima(Yellowstone.train.ts, order = c(2,1,2), seasonal = c(1,1,2))
summary(Yellowstone.train.arima)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
Yellowstone.train.arima.pred <- forecast(Yellowstone.train.arima, h = Yellowstone.nValid, level = 0)
Yellowstone.train.arima.pred

# Use auto.arima() function to fit ARIMA model on the training data
# Use summary() to show auto ARIMA model and its parameters.
Yellowstone.train.auto.arima <- auto.arima(Yellowstone.train.ts)
summary(Yellowstone.train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.
Yellowstone.train.auto.arima.pred <- forecast(Yellowstone.train.auto.arima, h = Yellowstone.nValid, level = 0)
Yellowstone.train.auto.arima.pred

#Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) AR(2) model; (2) MA(2) model; (3) ARMA(2,2) model; (4) ARIMA(2,1,2)(1,1,2) model; and 
# (5) Auto ARIMA model.
round(accuracy(Yellowstone.train.ar2.pred, Yellowstone.valid.ts), 3)
round(accuracy(Yellowstone.train.ma2.pred, Yellowstone.valid.ts), 3)
round(accuracy(Yellowstone.train.arma2.pred, Yellowstone.valid.ts), 3)
round(accuracy(Yellowstone.train.arima.pred, Yellowstone.valid.ts), 3)
round(accuracy(Yellowstone.train.auto.arima.pred, Yellowstone.valid.ts), 3)

#***********Auto arima for entire dataset*********#

# Use auto.arima() function to fit ARIMA model.
# use summary() to show auto ARIMA model and its parameters for entire dataset.
Yellowstone.auto.arima <- auto.arima(Yellowstone.ts)
summary(Yellowstone.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 12 periods. 
Yellowstone.auto.arima.pred <- forecast(Yellowstone.auto.arima, h = 12, level = 0)
Yellowstone.auto.arima.pred

# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 12 future periods.
plot(Yellowstone.ts, 
     xlab = "Time", ylab = "No of Visits", xaxt = "n",
     ylim = c(5000, 1200000), main = "Auto Arima on entire dataset", col ="blue",lwd=1)
axis(1, at = seq(1986, 2019, 1), labels = format(seq(1986, 2019,1)) )

lines(Yellowstone.auto.arima.pred$fitted, col = "brown", lwd = 2,lty=2)
lines(Yellowstone.auto.arima.pred$mean, col = "black", lty = 5, lwd = 1)
legend(1992,1000000+200000, legend = c("Historical data", 
                                       "Forecast of Historical data", "Forecast of future data "), 
       col = c("blue", "brown" , "black"), 
       lty = c(1, 2, 5), lwd =c(1, 2, 1), bty = "n")

#------------------------Model Comparison---------------------------#

#Comparison of Models using accuracy()
round(accuracy(Yellowstone.auto.arima.pred$fitted, Yellowstone.ts),3)
round(accuracy(Hist.HW.ZZZ.Pred$fitted,Yellowstone.ts),3)
round(accuracy(Yellowstone.season.pred$fitted,Yellowstone.ts ),3)
round(accuracy(Yellowstone.season.pred$fitted + ma.trailing.seas.res_12,Yellowstone.ts),3)
round(accuracy((naive(Yellowstone.ts))$fitted,Yellowstone.ts),3)
round(accuracy((snaive(Yellowstone.ts))$fitted,Yellowstone.ts),3)

#-----------------------------End----------------------------------#