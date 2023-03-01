# reading the weather data
library(readxl)
hum_average<- read_excel("D:/My Master matrials at APU/Semester 3 final/Time series analysis/Assigment/Japan_Weather.xlsx")
View(hum_average)
library(dplyr)

ts_humidity 
#check the missing values
sum(is.na(hum_average))
colSums(is.na(hum_average))

# General overviwe of the data, in addition to find any missing values
glimpse(hum_average)
summary(hum_average)

# the range of the data based on the date
range(hum_average$Date)

# to convert the data into a proper foramt such as 2022-01-01
hum_average$Date <- as.Date(hum_average$Date)

# to plot the selected orginal varaible humidity
plot(hum_average$Avg_Humidity)

#Time series plot for the humidity
humidity_ts <- ts(hum_average$Avg_Humidity,c(2,1), frequency = 7)
humidity_ts <- ts(data$humidity, start = c(2,1), frequency = 7)

plot(humidity_ts , col=2,
ylab="Humidity",
xlab="Date", main="Humidity of Itami,Hyogo,Japan from 01/01/2022 – 31/01/2023")

# check the seasonlity and the trend in the time seires
library(trend)
mk.test(humidity_ts)
cs.test(humidity_ts)
#mk test, P < 0.05 => has trend

#install.packages("seastests")
library(seastests)
isSeasonal(humidity_ts)

# split the data into training and testing set
training_hum_ts <- ts(humidity_ts [1:(length(humidity_ts) * 0.8)],frequency = 7)
testing_hum_ts <- ts(humidity_ts[(length(humidity_ts) * 0.8 ):length(humidity_ts)+1], frequency = 7)

#check the length of original, training and testing set
length(humidity_ts)
length(training_hum_ts)
length(testing_hum_ts)

# SES (Simple exponential Smoothing)
ses_hum_mod= ses(training_hum_ts, h=length(training_hum_ts))
ses_hum_for = forecast(ses_hum_mod, h=length(testing_hum_ts))

# Plot forecast and actual test data
plot(ses_hum_for , main = "Simple Exponential Smoothing", xlab = "Date", ylab = "Humidity",lwd=2)
lines(ses_hum_mod$fitted,col="red",lwd=2)
legend("topleft", legend = c("Original","Train", "Test"), col=c("black","red","blue"), lty=2)

# holts method
holt_hum_mod= holt(training_hum_ts, h=length(training_hum_ts), initial = "simple")
holt_hum_for = forecast(holt_hum_mod, h=length(testing_hum_ts))

# Plot forecast and actual test data
plot(holt_hum_for , main = "Forecasting using Holt Smoothing", xlab = "Date", ylab = "Humidity",lwd=2)
lines(holt_hum_mod$fitted,col="red",lwd=2)
legend("topleft", legend = c("Original","Train", "Test"), col=c("black","red","blue"), lty=2)

model.accuracy <- rbind(accuracy(as.vector(model.stlm$mean), data.test),
                        accuracy(as.vector(f.tbats$mean), data.test),
                        accuracy(as.vector(exp(f.arima$mean)), data.test)
)

rownames(model.accuracy) <- c("STLM", "TBATS", "ARIMA")
model.accuracy <- as.data.frame(model.accuracy)
model.accuracy


ts_training <- ts(ts_humidity[1:(length(ts_humidity) * 0.8)],frequency = 7)
ts_testing <- ts(ts_humidity[(length(ts_humidity) * 0.8 + 1):length(ts_humidity)+1], frequency = 7)


# to be done last
holt_s <- ts_training %>%
  holt(h=length(ts_training), initial = "simple")%>%
  forecast(h= length(ts_testing))

my_colors <- c("Actual" = "black", "Train" = "green", "Test" = "red")

ts_training %>%
  autoplot(series = "Actual") +
  autolayer(holt_s$fitted,series = "Train") +
  autolayer(holt_s$mean,series = "Test")+
  scale_color_manual(values = my_colors)

plot(ts_)

library(ggplot2)
# to be done last
holt_s <- training_hum_ts %>%
  holt(h=length(training_hum_ts), initial = "simple")%>%
  forecast(h= length(testing_hum_ts))

training_hum_ts %>%
  autoplot(series = "Actual") +
  autolayer(holt_s$fitted,series = "Train") +
  autolayer(holt_s$mean,series = "Test") +
  scale_color_manual(values = my_colors)

# Plot the data and specify line colors
training_hum_ts %>%
  autoplot(series = "Actual") +
  autolayer(holt_s$fitted, series = "Train") +
  autolayer(holt_s$mean, series = "Test") +
  scale_color_manual(values = my_colors)

model.accuracy <- rbind(accuracy(as.vector(ts_humidity_ses$mean), ts_testing),
                        accuracy(as.vector(ts_humidity_holts$mean), ts_testing),
                        accuracy(as.vector(lr_humidity$mean), ts_testing)
                        )

rownames(model.accuracy) <- c("STLM", "TBATS", "ARIMA")
model.accuracy <- as.data.frame(model.accuracy)
model.accuracy