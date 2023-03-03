# ==============================
# Time Series Forecasting in R
# Humidity forecasting for Dubai
# Sai Naing Lynn Oo (TP068393)
# ==============================

# Import library
library('readxl')
library('forecast')
library('tseries')
library('TTR')
library('uroot')
library('trend') # mk.test
library('seastests') # is.seasonal
library('lmtest') # coeftest

# Close all plot windows
dev.off()

# Import data
data <- read_excel('datasets/Dubai_TSF.xlsx')
# View(data)

plot(decompose(ts_humidity))
# Explore variables
plot(data$humidity)

# convert to TS
ts_humidity <- ts(data$humidity, start = c(2,1), frequency = 7)
length(ts_humidity)

# TS plot
plot(ts_humidity, main = "Daily Average Humdity in Dubai from 2022-01-01 to 2023-02-24", xlab = "Day", ylab = "Humidity (%)")

# ACF plot for stationary
Acf(ts_humidity, main="Autocorrelation Plot for Humidity")
Acf(diff(ts_humidity,1), main="ACF Plot after first differencing")



# ADF test for unit root (irregular/level) stationary
adf.test(ts_humidity)
# H0: The time series is non-stationary.
# H1: The time series is stationary. <====
# p-value = 0.01 < 0.05 => The TS is Stationary for irregular

# MK Test for trend
mk.test(ts_humidity)
# p-value = 0.0001 < 0.05 => It has trend component.
# H0: There is no trend present in the series.
# H1: There is a trend present in the series. <====

cs.test(ts_humidity)

# KPSS Test for trend (deterministic) stationary
kpss.test(ts_humidity)
kpss.test(diff(ts_humidity,1))
# H0: The time series is stationary.
# H1: The time series is non-stationary. <====

# Number of differencing 
ndiffs(ts_humidity)
# 1 (It has trend)
ndif

# any seasonal ?
isSeasonal(ts_humidity)
# False -> No seasonality exists.

# CH Test for seasonal stationality
ch.test(ts_humidity)

# Proposed techniques for modelling and forecasting
# (i) Simple Exponential Smoothing
# (ii) Holt’s Method
# (iii) Linear,Trend Model

# Data partitioning using 80-20% ratio
training_ts <- ts(ts_humidity[1:(length(ts_humidity)*0.8)], frequency = 7)
testing_ts <- ts(ts_humidity[-c(1:(length(ts_humidity)*0.8))], frequency = 7)

# check the length of train and test sets
length(ts_humidity)
length(training_ts)
length(testing_ts)

# ======= (i) Simple exponential Smoothing =======
ses_model <- ses(training_ts, h=length(testing_ts))
ses_forecast <- forecast(ses_model, h=length(testing_ts))

# plotting train and test 
plot(ses_forecast, main = "Forecasts using Simple Exponential Smoothing", xlab = "Day", ylab = "Humidity (%)",lwd=2 )
lines(fitted(ses_model),col="red",lwd=2)
legend("topleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex =1.3)

# performance matrices
ses_mean_f = ts(ses_forecast$mean, frequency = 7)
rbind(accuracy(ses_model) , accuracy(ses_mean_f, testing_ts))

# ======= (ii) Holts Method =======
holts_model <- holt(training_ts, h=length(testing_ts))
holts_forecast <- forecast(holts_model, h=length(testing_ts))

# Plot forecast and actual test data
plot(holts_forecast , main = "Forecasts using Holts Method", xlab = "Day", ylab = "Humidity (%)",lwd=2)
lines(fitted(holts_model),col="red",lwd=2)
legend("topleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# performance matrices
holts_mean_f <- ts(holts_forecast$mean, frequency = 7)
rbind(accuracy(holts_model) , accuracy(holts_mean_f, testing_ts))

# ======= (iii) Linear Trend Model =======

trend_data <- 1:length(training_ts)
lr_model <- tslm(training_ts ~ trend_data)
new_data <- (length(training_ts) + 1):(length(training_ts) + length(testing_ts))
lr_forecast <- forecast::forecast(lr_model, newdata = data.frame(trend_data = new_data))

# Plot forecast and actual test data
plot(lr_forecast, main = "Forecasts using Linear Trend Model", xlab = "Day", ylab = "Humidity (%)",lwd=2)
lines(lr_forecast$fitted,col="red",lwd=2)
legend("bottomleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# performance matrices
lr_mean_f <- ts(lr_forecast$mean, frequency = 7)
rbind(accuracy(lr_model) , accuracy(lr_mean_f, testing_ts))

# ======== ARIMA model ==========

# plotting ACF and PACF
tsdisplay(ts_humidity, main = "ACF and PACF Plots for original series")
tsdisplay(diff(ts_humidity,1), main = "ACF and PACF Plots after first differencing")

# testing stationary
adf.test(ts_humidity) # Stationary

# getting recommended number of diff.
ndiffs(ts_humidity)
# Recommended --> [1] --> d=1 (characteristics of a liner trend)

# plotting ACF and PACF for transformed data directly
tsdisplay(diff(ts_humidity, 1))

# Auto ARIMA
auto.arima(ts_humidity, trace = T)

# Building ARIMA (0, 1, 1)
ts_humidity_arima200 <- arima(ts_humidity, order = c(2,0,0)) # sig - 0.4398
ts_humidity_arima012 <- arima(ts_humidity, order = c(0,1,2))
ts_humidity_arima212 <- arima(ts_humidity, order = c(2,1,2)) # sig - 0.825
ts_humidity_arima412 <- arima(ts_humidity, order = c(4,1,2)) # sig - 0.661
ts_humidity_arima210 <- arima(ts_humidity, order = c(2,1,0)) 
ts_humidity_arima112 <- arima(ts_humidity, order = c(1,1,2)) # auto.arima  # sig - 0.8667

# to generate the unknown parameters/coefficient(s)
summary(ts_humidity_arima112)

# to test the significant of the coefficients
library('lmtest')
coeftest(ts_humidity_arima200) # sig
coeftest(ts_humidity_arima012) # sig
coeftest(ts_humidity_arima212) # sig - ma only
coeftest(ts_humidity_arima412) # none
coeftest(ts_humidity_arima210) # ar2 only
coeftest(ts_humidity_arima112) # Sig

# to conduct Ljung Box test and Residual Analysis
checkresiduals(ts_humidity_arima200) # sig - 0.4398
checkresiduals(ts_humidity_arima012)
checkresiduals(ts_humidity_arima212) # sig - 0.825
checkresiduals(ts_humidity_arima412) # sig - 0.661
checkresiduals(ts_humidity_arima210)
checkresiduals(ts_humidity_arima112) # sig - 0.8667

# to plot the TS with forecasts
plot(forecast(ts_humidity_arima200, h= 30))
plot(forecast(ts_humidity_arima112, h= 30))

# to add fitted model into the existing plot
lines(fitted(ts_humidity_arima200), col="red", lwd=2)
lines(fitted(ts_humidity_arima112), col="red", lwd=2)

# performance matrices for ARIMA(2,0,0)
accuracy(ts_humidity_arima200) # sig - 0.4398
#accuracy(ts_humidity_arima012)
accuracy(ts_humidity_arima212) # sig - 0.825
accuracy(ts_humidity_arima412) # sig - 0.661
# accuracy(ts_humidity_arima210)
accuracy(ts_humidity_arima112) # sig - 0.8667