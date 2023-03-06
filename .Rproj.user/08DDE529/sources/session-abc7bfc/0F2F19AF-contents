# T4_Q6 - NYC Bank

# Import libraries
library('forecast')
library('tseries')
library('lmtest')
library('uroot')

# Import dataset
data_NYC <- read.csv("datasets/T4_Q6_NYCBank_WeeklyInvestments.csv")

# Convert TS (monthly data)
NYCBank <- ts(T4_Q6_Data$Investment,start=1965,frequency=12)

# ACF and PACF plot
tsdisplay(NYCBank)

# ADF test of unit root
adf.test(NYCBank)

# C.H test for seasonal stationality 
ch.test(NYCBank)

# Get number of differencing
ndiffs(NYCBank) # ordinary differencing
nsdiffs(NYCBank) # seasonal differencing

# Get suggested models from auto arima
auto.arima(NYCBank,trace=TRUE)

# Building ARIMI model
arima_NYCBank_100 <- arima(NYCBank,order=c(1,0,0))

# Building SARMIA model
arima_NYCBank_100_200 <- arima(NYCBank,order=c(1,0,0),seasonal=c(2,0,0))

# Check sig. of coefficients
coeftest(arima_NYCBank_100)
coeftest(arima_NYCBank_100_200)

# Check residuals and Ljung-Box test
checkresiduals(arima_NYCBank_100)
checkresiduals(arima_NYCBank_100_200)

# Forecasts using the best model
forecast(arima_NYCBank_100_200,h=3)

# Plot forecast and fitted model
plot(forecast(arima_NYCBank_100_200,h=3),main="NYC Bank Average Weekly Investment",xlab="Year",ylab="Investment")
lines(fitted(arima_NYCBank_100_200),col="red",lwd=2)
legend("topleft",c("Actual","ARIMA(1,0,0),(2,0,0)[12]","Forecast"),col=c("black","red","blue"),lwd=2,cex=0.4)
