# ===== Seasonal ARIMA model Exercise =====
# Split the below data into training (80%) and testing data (20%).
# Analyse the training data and formulate the model equation for the ARIMA model you chosen:

# sales.dat – quarterly sales data (in $’000) starting 01-	01-2007 
# USABeerproduction.csv

# Then, compute the accuracy of the model in the testing data.
# Check the residuals and test whether the model you chosen is satisfactory. 

# plot ACF and PACF for Sales data
library('forecast')
tsdisplay(Sales, lag.max = 36)

# to check the stationary for seasonality 
library('tseries')
library('uroot')

ch.test(Sales)
# the series is not stationary

# to get recommended number of differencing
# regular differencing
ndiffs(Sales) # --> [1]

# seasoal differencing 
nsdiffs(Sales) # --> [1]

# ----- Check the codes from here again #TBR

# applying differencing both regular and seasonal 
tsdisplay(diff(diff(Sales, 1), 4)
tsdisplay(diff(diff(Sales, 4), 1) # the same result

# trainig set - 80%
traning_sales <- ts(Sales[1:(0.8*length(Sales))], frequency = 4)

# testing set - 20%
testing_sales <- Sales[(0.8*length(Sales+1)):(length((Sales)+1))] # put '-' also ok

# plot ACF and PACF for training set
tsdisplay(traning_sales)

# regular differencing
ndiffs(traning_sales) # --> [1]

# seasoal differencing 
nsdiffs(traning_sales) # --> [1]

tsdisplay(diff(traning_sales,4))
adf.test(traning_sales)
ch.test(traning_sales)

auto.arima(traning_sales, trace = TRUE)

# construct ARIMA model
arima_sales_001_011 <- arima(training_sales)
accuracy()

training_beer <- ts(beer[1:(0.8*length(beer))], frequency =12)
testting_beer <- beer[(0.8*length(beer))+1 : (length(beer)+1)]

tsdisplay(training_beer, lag.max = 60)
adf.test(training_beer)
ch.test(training_beer)

ndiffs(training_beer)
nsdiffs(training_beer)

tsdisplay(diff(diff(training_beer,1),12))


          