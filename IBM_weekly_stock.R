# T4_Q5 - IBM Weekly stock price

# Import libraries
library('forecast')
library('tseries')
library('lmtest')


data_IBM <- read.csv("datasets/T4_Q5_IBM_WeeklyStockPrice.csv")
IBM <- ts(data_IBM$IBM)
tsdisplay(IBM)

# ADF test
adf.test(IBM)
# not stationary

# recommend differencing
ndiffs(IBM)
# output => 1

# ===== Note =====
# n = 1 => Linear trend
# n = 2 => Quadratic trend

# KPSS test
kpss.test(IBM)
kpss.test(diff(IBM,1))

# Building ARIMA model
arima_110 <- arima(IBM , order = c(1,1,0))
arima_111 <- arima(IBM , order = c(1,1,1))

# Test the sig. of coefficients
coeftest(arima_110)
coeftest(arima_111)

# Test the adequacy of the models
checkresiduals(arima_110)
checkresiduals(arima_111)
# both models are adequate

# Measurement of forecast error
accuracy(arima_110)
accuracy(arima_111) # Best model

# Forecasts using the best model
forecast(arima_110, h= 1)

# Plot the forecasts
plot(forecast(arima_110, h= 1), main = "IBM Stock Price", xlab = "Week", ylab = "Price")
lines(fitted(arima_110), col = "red", lwd = 2 )
legend("topleft", c("Actual", "ARIMA(1,1,0)", "Forecast"), col = c("black", "red", "blue"), lwd = 2, cex = 1)
