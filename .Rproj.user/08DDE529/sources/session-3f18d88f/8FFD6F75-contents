# Import library
library('readxl')
library('forecast')
library('tseries')
library('TTR')
library('uroot')
library('trend') # mk.test
library('seastests') # is.seasonal
library('lmtest') # coeftest
library('dplyr')

# Import data
data_meter <- read_excel('datasets/meter_reading.xlsx')
View(data_meter)
plot(data_meter$usage)

# Convert time series data
ts_meter <- ts(data_meter$usage, frequency = 7)
plot(ts_meter)

# ACF plot
Acf(ts_meter)

# ADF test for irregular variation stationarity
adf.test(ts_meter)
# Not stationary

# Get recommended number of diff
ndiffs(ts_meter)

# ADF test after diff=1
adf.test(diff(ts_meter))
# Stationary

# MK test for trend test
mk.test(ts_meter)

# Seasonal component test 
isSeasonal(ts_meter)

# KPSS for trend stationarity
kpss.test(ts_meter)
# Not stationary

# ACF and PACF
tsdisplay(ts_meter)
tsdisplay(diff(ts_meter))

# Building ARIMA model
arima_meter212 <- arima(ts_meter, order = c(2, 1, 2))

# Get suggested best model
auto.arima(ts_meter, trace = T)
# ARIMA (2, 1, 2)

# Checking model's adequacy
checkresiduals(arima_meter212)

# Checking coefficients significant
coeftest(arima_meter212)
# all paramters are significant

# Plot forecast for next 30 days
plot(forecast(arima_meter212, h= 7))

# Adding fitted model into the existing plot
lines(fitted(arima_meter212), col="red", lwd=2)
legend("bottomleft", legend = c("Actual","Fitted", "Forecast"), col=c("black","red","blue"), lty=1, cex = 1.3)

# Check accuracy of the model 
accuracy(arima_meter212)

# Check residual normality
shapiro.test(arima_meter212$residuals)