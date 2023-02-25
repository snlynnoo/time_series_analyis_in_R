#--------------------------DAY (1)-----------------------
# Plot time series
# Package List: 'tseries', 'forecast', 'uroot', 'TTR', 'dynlm', 'lmtest' 

# Importing data set
library(readr)
Earthquakes <- read_csv("Earthquakes.csv")
View(Earthquakes)

# Installing 'tseries' package (For TS analysis)
install.packages('tseries')

# Plotting annual TS plot # ts() : time series (in base library, no need to install)
# Transforming TS data types
quakes <- ts(Earthquakes$earthquakes, start = 1960, frequency = 1); quakes
# start: Starting year, end: Ending year (if applicable)
# frequency = 1 (Annual), 4 (Quarterly), 12 (Monthly), pass accordingly, 1 is default

# Easiest and simplest way to plot TS
plot(quakes) 
# main = Chart title, xlab = X-axis Label, ylab = Y-axis Label
plot(quakes, main = "Annual Earthquakes 1960-2058", xlab = "Year", ylab = "Earthquakes Magnitude")


# Importing sales data set
sales <- read.csv("~/OneDrive/OneDrive - Asia Pacific University/Third Semester/02. Time Series/03. Teaching  Learning Materials-20221212/sales.dat", sep="")
View(sales)

# Plotting quarterly TS plot
Sales <- ts(sales$Sales, frequency = 4) # here quarterly, so frequency = 4
plot(Sales, main = "Quarterly Sales", xlab = 'Year', ylab = 'Sales')

#--------------------------DAY (2)-----------------------

# ACF plot, ADF test and Ch Test to check stationary
install.packages('forecast')
library(forecast)
acf(quakes)
acf(quakes, ylim=c(-1,1)) #ylim is optional for y-range
Acf(quakes, ylim=c(-1,1))
Acf(quakes)
plot(quakes)
install.packages('uroot')
# library(uroot)
library(tseries)
adf.test(quakes)
plot(Sales)
Acf(Sales, ylim = c(-1,1), lag.max = 36)

ndiffs(quakes)
Acf(diff(quakes, 1))
adf.test(diff(quakes, 1))

Acf(diff(Sales, 4))
adf.test(Sales) #test before difference 
adf.test(diff(Sales, 4)) #test after difference 

library(readr)
USABeerproduction <- read_csv("~/OneDrive/OneDrive - Asia Pacific University/Third Semester/02. Time Series/03. Teaching  Learning Materials-20221212/USABeerproduction.csv")
View(USABeerproduction)

beer <- ts(USABeerproduction$beerproductionusa, start = 1970, frequency = 12)
# start-which year to start, end-not defining means all, freq = 12 coz monthly
View(beer)

# Differencing
plot(beer)
Acf(beer, lag.max = 50) #lag.max for four year data
adf.test(beer)
ndiffs(beer) #for regular
nsdiffs(beer) #for seasonal
diff_beer <- diff(beer, 1) #for regular
diff12_diff_beer <- diff(diff_beer, 12) #for monthly seasonal
plot(diff12_diff_beer)
adf.test(diff12_diff_beer)
Acf(diff12_diff_beer)
Acf(diff12_diff_beer, lag.max = 50) #show more lags
library('uroot')
ch.test(beer) #from Uroot 
ch.test(diff12_diff_beer)

# ---------------------Tutorial (1-Q7)-------------------
# ACF, ADF, CH tests

library('forecast')
Acf(beer, lag.max=50)
library('tseries')
adf.test(beer)
library('uroot')
ch.test(beer)
# Differencing for both regular & seasonal
# for regular
diff_beer <- diff(beer, 1)

# for monthly seasonal
diff12_diff_beer <- diff(diff_beer, 12) 
plot(diff12_diff_beer)

adf.test(diff12_diff_beer)
Acf(diff12_diff_beer)
Acf(diff12_diff_beer, lag.max = 50) #show more lags
library('uroot')
ch.test(beer) #from Uroot  
ch.test(diff12_diff_beer)

# how to convert monthly data into quarterly?
beer.qtr <-aggregate(beer, 4)
beer.qtr

#---------------------Practical Exercise-----------------------
# Moving Average and Simple Exponential Smoothing Exercise
# Analyse the data using MA and SES
# It doesn't mentioned the alpha values, so we need to optimize it. 

library(forecast)
# Forecast package will not optimize for you
# Function: ma(x, order, center = T) // center=T is default

# Moving average, will try with 3, 5 ,7 periods
# Whenever you put the order number, they will put it in the center.

# Plotting original quaked data
plot(quakes, main = "Annual Earthquateks Data", xlab = "Year", ylab = "Earthquakes")

# 3 period MA
ma3 <- ma(quakes, 3, centre = T) # order=3=period, if 5 period, pass 5.

# Inserting ma3 line to existing original quakes plot
lines(ma3, col=2, lwd=2) #col = color code, lwd = line thickness

# 10 period MA
ma10 <-ma(quakes, 10)

# Inserting ma10 line to existing original quakes plot
lines(ma10, col=3, lwd=2)

# Simple Exponential Smoothing <= in Forecast package
# ses = Simple Exponential Smoothing
# 3 version => Single, Double, Winter

# If everything to be optimized, you just pass the data
ses_quakes <- ses(quakes)

# Want to define "initial value" to be the "first value", pass "simple"
ses_quakes <- ses(quakes, initial = "simple")
# in ses_quakes var. carries lots of data, so we must specify 

# Inserting lines to the plot
lines(ses_quakes, col=4, lwd=2) # wont show anything
lines(ses_quakes$fitted, col=4, lwd=2) # use $ sign and select "fitted"

# Adding labels to the plot
legend("topright", c("Acutal", "3-MA", "10-MA", "SES"), col = 1:4, lwd = 2, cex = 0.5)
# cex= Resize parameter, 0.2 means 20%, you can choose accordingly


# Jan 2023 - MA, WMA, MMA, EMA, SES, LTM, Holt's method
# ---------------------Practical Exercise-----------------------
# Perform time series analysis for the data below. 
# You are required to use THREE of the following forecasting techniques to perform the forecast:
# -> Moving average or Weighted Moving Average  
# -> Modified Moving Average or Exponential Moving Average 
# -> Simple Exponential Smoothing 
# -> Linear, Quadratic or Exponential Trend Model 
# -> Holts Method 
# Holt & SES must be choosen "fitted" model

# install.packages("TTR")
Population <- ts(Population_Malaysia$Total, start = 1972)
# use $Total : we will use total population variable and starting from 1972 to end
# not required 'frequency' parameter since yearly data,if seasonal, we must specify 
# frequency = 1 is for yearly and it is the default value
# Quarterly = 4, Monthly = 12, Yearly = 1

library(TTR)
library(forecast)

# Plotting original data
plot(Population, main = "Malaysian Population 1970-2019", xlab = "Year", ylab = "Total Population")

# Weighted MV with 3 periods
wma3_pop <- WMA(Population, 3) #from TTR package, period=3 

# Modified MV with 3 periods
ema3_pop <- EMA(Population, 3)

# Simple Exponential Smoothing
ses_pop  <- ses(Population, Initial="simple") #from forecast package

# Linear Trend Model
# cannot plot ->linear_pop <- tslm(Population~., Population) #from forecast package
# Use 'dynlm' for LTM
install.packages('dynlm')
library('dynlm')

# Holt method
# It can be used it from either forecast or stat package, same result.
holt_pop <- holt(Population, initial = "simple") #from forecast

# Adding lines to the original plot
lines(wma3_pop, col=2, lwd=2)
lines(ema3_pop, col=3, lwd=2)
lines(ses_pop$fitted, col=4, lwd=2) #must choose fitted
lines(holt_pop$fitted, col=5, lwd=2) #must choose fitted
legend("bottomright", c("Actual","3-WMA","3-EMA","SES", "Holt"), col=1:5, lwd=2, cex=0.5)

#-------------Regression with dummy exercise-----------------

View(sales)
# you can also do with 'dynlm'
library('forecast')
# make sure the data is in Ts type first
time = 1:length(Sales) #setting the time start at 1 until how many rows the data set has
season = seasonaldummy(Sales) #adding dummy

# TS Linear model (depends on the package, confirm the data type to be passed.)
lmM <- tslm(Sales~time+season) # EQ: Sales = time + season
summary(lmM)
# you can extend your forecast from here

# Decomposition model
plot(Sales) #since the seasonal pattern are almost constant -> Additive model is suitable
decomposition_additive <- decompose(Sales, type = "additive")
plot(decomposition_additive) #random means the error plot
decomposition_additive$figure #to give adjusted seasonal variation values # the avg. is nearly 0

# test: multiplicative model
decomposition_multiplicative <- decompose(Sales, type = "multiplicative")
decomposition_multiplicative$figure # the number are around 1
plot(decomposition_multiplicative)

# alternative decomposition function in R - stl()
# stl() - stat package
# stlf() - forecast package
# stlm() - forecast

stlm <- stlm(Sales, s.window = 7)
summary(stl(Sales, s.window = 7))

#----------------------Tutorial(2), last.Q-----------------------
# Use the following forecasting model to forecast the beer production in the next 5 years using R.
# (a) Exponential Smoothing
# (b) Holts Method
# (c) Holts Winter Method -- The first three are in the same family of Exp. smoothing. 
# (d) Decomposition Model (use STL in R)

# In manual calculation, we need Alpha, Beta and Gamma.
# But, in R, you need to find what is the optimized parameter (we can do in R)

# Make sure your data is TS
beer # Monthly seasonal (1970 ~ 1977)
# Forecast ? 1978 ~ 1982 for every month

# SES
library('forecast')
ses_beer <- ses(beer) #to auto-optimize, just leave empty
ses_beer <- ses(beer, h=60, initial = 'simple') # h- forecast duration from function
          # On default, they will generate next 10 values, i.e, up to 1978-Oct if you want more specify 'h'
          # Here 5 yr = 12 months x 5 yrs = 60, therefore, h=60 is provided in the function 
          # 'simple' mean Use the first value as initial value (13.092)
autoplot(ses_beer) # Refer in TSF one notes
summary(ses_beer) # this can check what is the optimized parameter values by R

# Holt's method
holt_beer <- holt(beer, h=60, initial = 'simple')
autoplot(holt_beer) # will generate with large scale range

holt_beer <- holt(beer, h=60) # optimize to zoom in to see the plot
autoplot(holt_beer)
summary(holt_beer)

plot(holt_beer, main = "Beer Production - Holt's Method Forecast", xlab = "year", ylab = "Production")
lines(holt_beer$fitted, col=2, lwd=2)

# Holt Winter's method (Additive model -> Default)
HW_beer <- hw(beer, h=60) #this is from forecast package, just used the optimized one
plot(HW_beer, main='Beer Production - Holt Winter Forecast', xlab = 'Year', ylab = "Production")
lines(HW_beer$fitted, col=2, lwd=2) # overlap the fitted model into the existing plot
summary(HW_beer)

# For multiplicative model
HW_beer_multiplicative <- hw(beer, h=60, seasonal = "multiplicative")
plot(HW_beer_multiplicative, main='Beer Production - Holt Winter Forecast', xlab = 'Year', ylab = "Production")
lines(HW_beer_multiplicative$fitted, col=2, lwd=2) # overlap the fitted model into the existing plot
summary(HW_beer_multiplicative)

# Decomposition model usint STL
# stl() - from stat package
# stlf() -from forecast package (they both are similar, just different in function names)
# Here, we will use from forecast package
decomposition_beer <- stlf(beer, h=60)
summary(decomposition_beer)
autoplot(decomposition_beer)

# Decomposition model from stat package
decompose( ) # they will plot the four individual layouts 
# Refer to decomposition R scripts

#-------------------Data Partition Ex.s-----------------------

# Split the data set into 80 % and 20%
View(Earthquakes)
quakes
# the data is a list 
# R array starts with 1, not starting with 0

# 80% training set
training_quakes <- quakes[1:(length(quakes)*0.8)]

# 20% test set
testing_quakes <- quakes[(length(quakes)*0.8+1):(length(quakes)+1)] # must be add +1 to get till the end
testing_quakes

# SES 
library('forecast')
ses_training_quakes <- ses(training_quakes, initial = 'simple') #if you want to optimize, don't pass any arguments

#-------------------ARIMA MODEL EX(1)-----------------------
# Analyze the following data and formulate the model equation for the ARIMA model you chosen; 
# 1. quakes.dat
# 2. population.csv - average growth of population from 1970 to 2017

# ========== 1.Solution for quake.dat ==========

library('forecast')
tsdisplay(quakes) #plotting ACF and PACF plot

library('tseries')
adf.test(quakes)
# the data is not stationary - a weak trend component detected

# will check how many differencing is required
ndiffs(quakes)
# the result is 0 => not recommended for differencing

# apply 1st differencing and plot ACF & PACF directly
tsdisplay(diff(quakes, 1)) 

# Since ACF is dominant --> MA(q) process
# Tentative ARIMA model : ARIMA(0, 1, 1)

# Building ARIMA model from forecast package
quakes_arima011 <- arima(quakes, order = c(0,1,1)) #need to pass original data (not diff'ed data)

# to track the coefficients 
summary(quakes_arima011)

# to get suggestion for the best recommended arima model
auto.arima(quakes)

# trace back other suggested models
auto.arima(quakes, trace = T) 

# zero mean model     -> not going to have intercept and constant value
# non-zero mean model -> will have constant values

# to decide whether differencing or w/out differencing is better ?
# use performance evaluation matrices 

# Builing ARIMA (1, 0, 1)
quakes_arima101 <- arima(quakes, order = c(1, 0, 1))

# comparing their performance between our model and auto.arima
accuracy(quakes_arima011)
accuracy(quakes_arima101)

# ------------ Model's result -----------------------------------------------------
#                    ME    RMSE      MAE      MPE     MAPE      MASE      ACF1
# Training 011 0.1270341  6.07418 4.669665 -7.35535 25.97658 0.8618214 0.0287003
# Training 101 0.1182301 5.887629 4.555481 -9.677964 26.5983 0.8407479 0.009220841
#----------------------------------------------------------------------------------
# We will use 101 since RMSE, MAE is lower

# ========== 2.Solution for average population growth ==========

# converting TS data (The question said within 1970 ~ 2017 only)
avegrowthpop <- ts (Population_Malaysia$`Average annual population growth rate (%)`, start = 1970, end = 2017)

# plotting ACF and PACF
tsdisplay(avegrowthpop)

# testing stationary
adf.test(avegrowthpop) # => Not stationary

# getting recommended number of diff.
ndiffs(avegrowthpop) 
# Recommended --> [1] --> d=1 (characteristics of a liner trend)

# plotting ACF and PACF for transformed data directly
tsdisplay(diff(avegrowthpop, 1))

# if it is confusing, how should we do ?
# in this case check the original data ACF and PACF
# when we look up it, still not sure
# then use use auto.arima() => recommended model --> arima(011)

# Building ARIMA (0, 1, 1)
avegrowthpop_arima011 <- arima(avegrowthpop, order = c(0,1,1))

# to generate the unknown parameters/coefficient(s)
summary(avegrowthpop_arima011) 

auto.arima(avegrowth, trace = T)
# ARIMA(0,1,1) with drift (recommended)
# what is drift model ?
# drift model do not put the constant mean zero
# this is not our focus in this module, we will only use pure arima

# to get the p-value of t-test in the model
# install.packages('lmtest') 
library('lmtest')
coeftest(avegrowthpop_arima011)
# the coefficient is not significant since p-value is 0.052, > 5% CI

# to conduct Ljung Box test and Residual Analysis
checkresiduals(avegrowthpop_arima011) # from forecast package
checkresiduals(quakes_arima011)
checkresiduals(quakes_arima101)

# to conduct forecasts (forecast pack)
# this is a yearly data and will forecast for next five years
forecast(quakes_arima101, h = 5) 

# to plot the TS with forecasts
plot(forecast(quakes_arima101, h= 5))

# to add fitted model into the existing plot
lines(fitted(quakes_arima101), col=8, lwd=2)