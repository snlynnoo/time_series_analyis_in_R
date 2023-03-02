# T4_Q6 - NYC Bank

# Import libraries
library('forecast')
library('tseries')
library('lmtest')
library('uroot')


data_NYC <- read.csv("datasets/T4_Q6_NYCBank_WeeklyInvestments.csv")
NYCBank <- ts(data_NYC$Investment, start = 1970, frequency = 12)
tsdisplay(NYCBank)

ndiffs()

# Building ARIMA
arima_NYCBank_100 <- arima(NYCBank, order = c(1,0,0))
arima_NYCBank_100_200 <- arima(NYCBank, order = c(1,0,0), seasonal = c(2,0,0))

# Sig. of coefficients test


# Adequacy 