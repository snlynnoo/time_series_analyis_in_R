# Samsung Stock Price 2022

# Import libraries
library('forecast')
library('tseries')
library('lmtest')


# Import dataset
samsung_data <- read.csv("datasets/Samsung_Stock_2022.csv")
View(samsung_data)
plot(samsung_data$Adj.Close)

# Convert TS
samsung <- ts(samsung_data$Adj.Close) 
#Since not interested in seasonal component no frequency value is specified

# Plot ACF and PACF 
tsdisplay(samsung)

# Check no. of differencing 
ndiffs(samsung) # = 1

# Plot ACF and PACF after 1st diff
tsdisplay(diff(samsung,1))

# Check ADF (Unit root test)
adf.test(diff(samsung, 1))

# Check KPSS trend stationary test
kpss.test(samsung) # -> Not statinary

# Auto ARIMA
auto.arima(samsung, trace = T)
# suggested model -> (0, 1, 0)

# Building ARIMA models
arima_samsung_010 <- arima(samsung, order = c(0, 1, 0))
arima_samsung_110 <- arima(samsung, order = c(1, 1, 0))

# Check adequacy 
checkresiduals(arima_samsung_010)
checkresiduals(arima_samsung_110)

# Check accuracy
accuracy(arima_samsung_010)
accuracy(arima_samsung_110)

# Check coefficient test to support the model
coeftest(arima_samsung_010)
coeftest(arima_samsung_110)
