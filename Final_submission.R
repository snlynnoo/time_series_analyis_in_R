# ============================================================
# Title: Time Series forecasting for Humidity of Dubai, UAE
# Data source: https://www.wunderground.com/weather/ae/dubai
# Period of training data: 2022-Jan-01 ~ 2023-Feb-24 (420 Obs.)
# Last updated date : 2023-02-07
# Analyst : Sai Naing Lynn Oo
# ============================================================

# ~~~~~~~~~~~~~~~~~ Part (a) ~~~~~~~~~~~~~~~~~

# Import libraries
library('readxl')
library('forecast')
library('tseries')
library('TTR')
library('uroot')
library('trend')
library('seastests') 
library('lmtest')

# Import data
data <- read_excel('datasets/Dubai_TSF.xlsx')

# Convert to TS
ts_humidity <- ts(data$humidity, start = c(2,1), frequency = 7)
length(ts_humidity)

# TS plot
plot(ts_humidity, main = "Daily Average Humdity in Dubai from 2022-01-01 to 2023-02-24",
                  xlab = "Day", ylab = "Humidity (%)")

# MK Test for trend
mk.test(ts_humidity)

# Seasonal component test
isSeasonal(ts_humidity)

# ~~~~~~~~~~~~~~~~~ Part (b, c) ~~~~~~~~~~~~~~~~~

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

# ======= (ii) Holts Method =======
holts_model <- holt(training_ts, h=length(testing_ts))
holts_forecast <- forecast(holts_model, h=length(testing_ts))

# Plot forecast and actual test data
plot(holts_forecast , main = "Forecasts using Holts Method", xlab = "Day", ylab = "Humidity (%)",lwd=2, ylim = c(0,100))
lines(fitted(holts_model),col="red",lwd=2)
legend("topleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# ======= (iii) Linear Trend Model =======
trend_data <- 1:length(training_ts)
lr_model <- tslm(training_ts ~ trend_data)
new_data <- (length(training_ts) + 1):(length(training_ts) + length(testing_ts))
lr_forecast <- forecast::forecast(lr_model, newdata = data.frame(trend_data = new_data))

# Plot forecast and actual test data
plot(lr_forecast, main = "Forecasts using Linear Trend Model", xlab = "Day", ylab = "Humidity (%)",lwd=2)
lines(lr_forecast$fitted,col="red",lwd=2)
legend("bottomleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# ~~~~~~~~~~~~~~~~~ Part (d) ~~~~~~~~~~~~~~~~~

# ======= Performance Evaluation =======
# Simple Exponential Smoothing
ses_mean_f = ts(ses_forecast$mean, frequency = 7)
rbind(accuracy(ses_model) , accuracy(ses_mean_f, testing_ts))

# Holts' Method
holts_mean_f <- ts(holts_forecast$mean, frequency = 7)
rbind(accuracy(holts_model) , accuracy(holts_mean_f, testing_ts))

# Linear Trend model
lr_mean_f <- ts(lr_forecast$mean, frequency = 7)
rbind(accuracy(lr_model) , accuracy(lr_mean_f, testing_ts))

# ~~~~~~~~~~~~~~~~~ Part (e) ~~~~~~~~~~~~~~~~~

# ACF plot 
Acf(ts_humidity, main="Autocorrelation Plot for Humidity")

# ADF test 
adf.test(ts_humidity)

# KPSS Test for trend stationary
kpss.test(ts_humidity)

# Number of differencing 
ndiffs(ts_humidity)

# KPSS Test after 1st differencing
kpss.test(diff(ts_humidity,1))

# ACF plot after 1st differencing  
Acf(diff(ts_humidity,1), main="ACF Plot after first differencing")

# ~~~~~~~~~~~~~~~~~ Part (f) ~~~~~~~~~~~~~~~~~

# ======== ARIMA model ==========

# Plot ACF and PACF
tsdisplay(ts_humidity, main = "ACF and PACF Plots for original series")

# Plot ACF and PACF after 1st differencing
tsdisplay(diff(ts_humidity,1), main = "ACF and PACF Plots after first differencing")

# Auto ARIMA
auto.arima(ts_humidity, trace = T)

# Building ARIMA models
ts_humidity_arima212 <- arima(ts_humidity, order = c(2,1,2))
ts_humidity_arima112 <- arima(ts_humidity, order = c(1,1,2))

# Generate summary to get the coefficient(s)
summary(ts_humidity_arima212)
summary(ts_humidity_arima112)

# For testing the significant of the coefficients
coeftest(ts_humidity_arima212)
coeftest(ts_humidity_arima112)

# Conduct Ljung Box test and Residual Analysis
checkresiduals(ts_humidity_arima212)
checkresiduals(ts_humidity_arima112)

# Performance matrices for both models
accuracy(ts_humidity_arima212)
accuracy(ts_humidity_arima112)

# Plot the TS with forecasts for the best model
plot(forecast(ts_humidity_arima112, h= 30))

# Add fitted model into existing plot
lines(fitted(ts_humidity_arima212), col="red", lwd=2)
legend("bottomleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# ================ END ==================