# Samsung Stock Price 2022

# Import libraries
library('readr')
library('forecast')
library('tseries')
library('lmtest')
library('rugarch')
library('dynlm')

# ========== FITTING ARIMA MODEL (Exercise :1 ) ==========
# Extract the Adjusted Close data, conduct the following analysis: 
# https://finance.yahoo.com/quote/005930.KS/history?p=005930.KS
# Plotting the data 
# Fit an ARIMA model

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

# ========== FITTING ARCH MODEL (Exercise: 2) ==========
# Plotting the data -> Done
# Fit an ARIMA model and checking for white noise 
# Testing for the ARCH effects 
# Model ARCH model 

# Libraries for ARCH model
# install.packages('rugarch')
library('rugarch')
library('dynlm')

# Import dataset
samsung_data <- read.csv("datasets/Samsung_Stock_2022.csv")

# Plot ACF and PACF for residual
tsdisplay(arima_samsung_010$residuals)

# Plot ACF and PACF for residual
tsdisplay(arima_samsung_010$residuals^2)

# Convert TS
rTS <- ts(samsung_data$Adj.Close) 

# Apply log transformation to control the range
log_rTS_samsung <- log(rTS)

# Apply 1st diff to logged series
samsung_rTS <- ts(diff(log_rTS_samsung, 1))
plot(samsung_rTS)
# Note: Apply anti-log to retrive the original value

# Plot ACF and PACF for transformed series. 
tsdisplay(samsung_rTS)
# no spikes in both ACF and PACF -> ignore ARIMA ==> ARMA(0,0)

# ========== ARCH(1) ==========

# (We use GARCH model function to build ARCH model by customizing the parameters)
# {Meaning there is only p process in the model, p = 1,*** garchOrder=c(1,0)}
# No ARMA process: *** armaOrder=c(0,0))

# Specifying ARCH(1) model parameters
garchSpec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,0)),
                        mean.model=list(armaOrder=c(0,0)),distribution.model="std")

# Fitting ARCH(1) model with SAMSUMG data
garchFit <- ugarchfit(spec=garchSpec, data=samsung_rTS)

# Model Analysis Output
garchFit


# ========== GARCH(1) ==========

# Specifying GARCH model parameters
garchSpec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0)),distribution.model="std")

# Fitting GARCH model with SAMSUMG data
garchFit <- ugarchfit(spec=garchSpec, data=samsung_rTS)

# Model Analysis Output
garchFit


# ========== FITTING GARCH MODEL (Exercise: 3) ==========
# Apple Stock Data

# Import data
AAPL <- read_csv("datasets/AAPL.csv")
View(AAPL)

# Transform into TS
AAPL <- ts(AAPL$`Adj Close`)

# ACF and PACF
tsdisplay(AAPL) # Since the values' range is not large, No log transforatation

# Number of differencing
ndiffs(AAPL)

# ACF and PACF after differencing 
tsdisplay(diff(AAPL,1))

# Return model 
AAPL_rTS <- ts(diff(AAPL,1))

# Specifying GARCH model parameters
garchSpec <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(1,1)),distribution.model="std")

# Fitting GARCH model with Apple Stock data
garchFit <- ugarchfit(spec=garchSpec, data=AAPL_rTS)

# Model Analysis Output
garchFit

# Forecasting for next 10 period
forecast.garch <- ugarchforecast(garchFit)

# Plot the forecast
plot(forecast.garch)

# select plot to show using options provided
