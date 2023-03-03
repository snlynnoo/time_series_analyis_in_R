# Import data
library("readxl")
data = read_excel('datasets/Dubai_TSF.xlsx')
View(data)

data$date <- as.Date(data$date)

# Explore variables
plot(data$temp)
plot(data$dew)
plot(data$humidity)
plot(data$wind)
plot(data$pressure)

# convert to TS
ts_humidity <- ts(data$humidity, start = c(2,1), frequency = 7)
length(ts_humidity)
View(ts_humidity)

ndiffs(ts_humidity)
# TS plot
plot(ts_humidity)

# ADF test for stationary
library('tseries')
adf.test(ts_humidity)
# p-value - 0.01 < 0.05 => The TS is Stationary

# ADF plot for stationary
Acf(ts_humidity)

# any trend ? 
# install.packages('trend')
library('trend')
mk.test(ts_humidity)
kpss.test(ts_humidity)

mk.test(diff(ts_humidity,1))
# p-value - 0.0001 < 0.05 => It has trend component.
# H0: There is no trend present in the series.
# H1: There is a trend present in the series.

# any seasonal ?
# install.packages('seastests')
library('seastests')
isSeasonal(ts_humidity)
# False -> No seasonality exists.

# (i) Simple Exponential Smoothing
# (ii) Holt’s Method
# (iii) Linear,Trend Model

# Data partitioning using 80-20% ratio

ts_training <- ts_humidity[1:(length(ts_humidity) * 0.8)]
ts_testing  <- ts_humidity[-c(1:(length(ts_humidity) * 0.8))]
length(ts_testing)

# ts_training <- ts(ts_humidity[1:(length(ts_humidity) * 0.8)], frequency = 7)
# ts_testing  <- ts(ts_humidity[-c(1:(length(ts_humidity) * 0.8))], frequency = 7)
# length(ts_testing)

# split the data into training and testing set (AMR)
ts_training <- ts(ts_humidity[1:(length(ts_humidity) * 0.8)],frequency = 7)
ts_testing <- ts(ts_humidity[(length(ts_humidity) * 0.8 + 1):length(ts_humidity)+1], frequency = 7)


# split the data into training and testing set
training_hum_ts <- ts(humidity_ts [1:(length(humidity_ts) * 0.8)],frequency = 7)
testing_hum_ts <- ts(humidity_ts[(length(humidity_ts) * 0.8 ):length(humidity_ts)+1], frequency = 7)


# (i) Simple Exponential Smoothing
library('forecast')
ts_humidity_ses <- ses(ts_training, h=length(ts_training))
ts_humidity_ses_forecast <- ts(forecast(ts_humidity_ses, h=length(ts_testing)), frequency = 7)
accuracy(ts_humidity_ses)
accuracy(ts_humidity_ses_forecast, ts_testing)

ts_humidity_ses$mean
ts_humidity_ses_mean <- ts(ts)


# Plot SES fitted model

plot(ts_humidity_ses, main = "Humidity - Simple Exponential Smoothing", xlab = "day", ylab = "humidity")
lines(ts_humidity_ses_forecast$fitted, col=2, lwd=2)
legend("topleft", legend = c("Actual", "Training", "Testing"), col = c('black', 'red', 'green'))

# lines(ts_humidity_ses_forecast, col=3, lwd=2)
autoplot(ts_humidity_ses)


# (ii) Holt's method
ts_humidity_holts <- holt(ts_training, h=length(ts_training))
ts_humidity_holts_forecast <- forecast(ts_humidity_holts, h=length(ts_testing))
autoplot(ts_humidity_holts) 
summary(ts_humidity_holts)

plot(ts_humidity_holts_forecast, main = "Humidity - Holts", xlab = "day", ylab = "humidity")
lines(ts_humidity_holts$fitted, col=2, lwd=2)
legend("topleft", legend = c("Actual", "Training", "Testing"), col = c('black', 'red', 'green'))

plot(ts_humidity, main = "Humidity - Holt's Method Forecast", xlab = "day", ylab = "'F")
lines(ts_humidity_holts$fitted, col=2, lwd=2)
legend("topleft", c("Acutal", "Fitted", "Forecast"), col = c(1, 2, 4), lwd = 2, cex = 0.5)

# (iii) Linear,Trend Model

# Load the time series data
# data <- ts(c(23, 27, 29, 32, 34, 38, 40, 44, 46, 50), start = c(2010, 1), frequency = 1)

# Fit a linear trend model to the data
ts_humidity_ltm <- lm(ts_humidity ~ time(ts_humidity))

# Print the model summary
summary(ts_humidity_ltm)

# Plot the data and the trend line
plot(ts_humidity, main = "Linear Trend Model", xlab = "day", ylab = "'F")
abline(ts_humidity_ltm, col = "red")

# Create a new time series object for the forecast period
new_data <- ts(rep(0, 84), start = end(data) + 1, frequency = frequency(data))

# Generate a forecast for the next 84 time periods
forecast_data <- forecast::forecast(ts_humidity_ltm, newdata = ts_testing)

# Generate a forecast for the next 5 time periods
forecast_data <- forecast::forecast(ts_humidity_ltm, h = 84)

# Plot the data, fitted line, and forecast values
forecast::autoplot(forecast_data) + autolayer(fitted(ts_humidity_ltm), series = "Fitted Line")

# Load the time series data
data <- ts(c(23, 27, 29, 32, 34, 38, 40, 44, 46, 50), start = c(2010, 1), frequency = 1)

# Fit a linear trend model to the data using dynlm
library(dynlm)
model <- dynlm(data ~ trend)

# Generate forecasts for the next 5 time periods
forecast_data <- predict(model, newdata = data.frame(trend = seq(length(ts_training) + 1, length(ts_training) + 84)))

# ====
ts_humidity_decompose_add <- decompose(ts_training, type = "additive")
forecast::forecast(ts_humidity_decompose_add, h = 84)
plot(ts_humidity_decompose_add) #random means the error plot

decomposition_additive$figure #to give adjusted seasonal variation values # the avg. is nearly 0
plot(ts_humidity)

ts_humidity_decompose_add <- stlf(ts_training)
summary(ts_humidity_decompose_add)

plot(x = data$date, y= data$humidity, type = 'l', main = "Humidity - Decompose Method Forecast", xlab = "day", ylab = "'F")
lines(x = data$date[1:length(ts_training)], y = ts_humidity_decompose_add$fitted, col = 2, lwd = 2)
forecast <- forecast::forecast(ts_humidity_decompose_add, h = 84)
summary(forecast)
length(forecast)
lines(x = data$date[length(ts_training)+1:length(ts_testing)], y = forecast, col = 2, lwd = 2)
length(data$date)

# Linear Trend
trend <- 1:length(ts_training)
lr_humidity= tslm(ts_training ~ trend)
lr_humidity_forecast = forecast(lr_humidity, h=length(ts_testing))

# Plot forecast and actual test data
plot(lr_humidity_forecast , main = "Forecasting using linear trend ", xlab = "Date", ylab = "Humidity",lwd=2)
lines(lr_humidity_forecast$fitted,col="red",lwd=2)
legend("topleft", legend = c("Original","Train", "Test"), col=c("black","red","blue"), lty=2)



# EMA

library("TTR")
ts_humidity_ema7 <- EMA(ts_training, 7)
plot()

# ======== ARIMA model ==========

# plotting ACF and PACF
tsdisplay(ts_humidity)

# testing stationary
library('tseries')
adf.test(ts_humidity) # => Not stationary

# getting recommended number of diff.
ndiffs(ts_humidity)
# Recommended --> [1] --> d=1 (characteristics of a liner trend)

# plotting ACF and PACF for transformed data directly
tsdisplay(diff(ts_humidity, 2))

# Building ARIMA (0, 1, 1)
ts_humidity_arima200 <- arima(ts_humidity, order = c(2,0,0)) # sig - 0.4398
ts_humidity_arima012 <- arima(ts_humidity, order = c(0,1,2))
ts_humidity_arima212 <- arima(ts_humidity, order = c(2,1,2)) # sig - 0.825
ts_humidity_arima412 <- arima(ts_humidity, order = c(4,1,2)) # sig - 0.661
ts_humidity_arima210 <- arima(ts_humidity, order = c(2,1,0)) 
ts_humidity_arima112 <- arima(ts_humidity, order = c(1,1,2)) # auto.arima  # sig - 0.8667
ts_humidity_arima111 <- arima(ts_humidity, order = c(1,1,1))

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
coeftest(ts_humidity_arima111)


# to conduct Ljung Box test and Residual Analysis
checkresiduals(ts_humidity_arima200) # sig - 0.4398
checkresiduals(ts_humidity_arima012)
checkresiduals(ts_humidity_arima212) # sig - 0.825
checkresiduals(ts_humidity_arima412) # sig - 0.661
checkresiduals(ts_humidity_arima210)
checkresiduals(ts_humidity_arima111) # sig - 0.8667

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
accuracy(ts_humidity_arima111) # sig - 0.8667
accuracy(ts_humidity_arima112)

shapiro.test(ts_humidity_arima211$residuals)

# Close all plot windows
dev.off()