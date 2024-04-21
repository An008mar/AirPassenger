#############################################################
# AirPassenger Data - ARIMA Model
#############################################################

# Data
AirPassengers

# Common command to eyeball data
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers) #cycle will print the cycle across years

# Plot
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
    #the distance btwn the top of lines in graph (sparkline) and btwn the respective trendline - abline is increasing over trend. This is called variance
      #when variance is not stationary, you can't do TSA
      #mean is also increasing here,which shd also be stationary
      #both shd be constant

time(AirPassengers)

acf(AirPassengers)
pacf(AirPassengers)

# both variance and mean need to be constant

plot(aggregate(AirPassengers,FUN = mean))  #general trend before further analysis, breaking data into yearwise. It shows uptrend

#box plot to measure seasonality of the data
boxplot(AirPassengers~cycle(AirPassengers))

#making the data stationary, by removing the variance --> by taking log values of the data
# this makes variance equal (Homogenizing the mean)

plot(log(AirPassengers)) #removed variance
acf(log(AirPassengers))
plot(diff(log(AirPassengers))) #mean is stationary

# now data is stationary to do TSA

# ARIMA

library(tseries)
acf(AirPassengers) #autocorrelation factor
acf(diff(log(AirPassengers)))
    # lines within the range only will be considered
    # acf will give q value.
    # the first line which is qualified is considered. The q value will be the line before this. Here, line 1 is disqualified. the 2nd is considered. therefore line before that is q value
    # q val = 1

pacf(diff(log(AirPassengers))) #p=0

#p d q values
# d is the no of times you differentiate to make the mean stationary
plot(diff(log(AirPassengers)))

#Fitting ARIMA model for predicting next 10 years
fit = arima(log(AirPassengers),c(0,1,1),seasonal=list(order=c(0,1,1)))
fit
pred <- predict(fit,n.ahead = 10*12)  #10 years * 12 months
    #the prediction values are in log form
    #Converting them to original form, we need to transform them using e value which is equal to 2.718
pred

pred1 <- round(2.718^pred$pred,0) #these are e raised to pred values, round off digits = 0

pred1

ts.plot(AirPassengers, pred1,log="y",lty=c(1,3))

#Testing Model
datawide = ts(AirPassengers, frequency = 12, start = c(1949,1), end = c(1959,12))

fit <- arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period = 12))

pred = predict(fit,n.ahead = 10*12)

pred1 = 2.718^pred$pred

data1 <- round(head(pred1,12),0) #predicted value

data2 <- round(tail(AirPassengers,12),0)  #original values

plot(data1,col="red",type="l")
lines(data2,col="blue")

#extending the original plot
ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))
