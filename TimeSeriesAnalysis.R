

##### Time Series Analysis #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

# install the package needed
install.packages("zoo")

# load the package
library(zoo)

# load the data
data_tablet = read.csv("data_tablet_sale.csv", header = T, sep = ",", stringsAsFactors = T)

data_tablet

# create week 18 to forecast
week = data.frame(week = 18,
                   unitsold = NA)

# create a predicted dataframe
pred_tablet = rbind(data_tablet, week)

# calculate moving average with k = 3
k = 3
ma3 = rollmean(data_tablet$unitsold, k)

# take a look 
ma3

# put the moving average sequence to the predicted dataframe
pred_tablet$ma3[(k+1):nrow(pred_tablet)] = ma3   

# now calculate moving average with k = 5
k = 5
ma5 = rollmean(data_tablet$unitsold, k)

# put the moving average sequence to the predicted dataframe
pred_tablet$ma5[(k+1):nrow(pred_tablet)] = ma5

# forecast week18's sale
# when use k=3 moving average
pred_tablet$ma3[18]

# when use k=5 moving average
pred_tablet$ma5[18]

# plot the time series
plot(unitsold ~ week, data = data_tablet, type = "l", xlim = c(0,18))

# add the moving average lines
lines(pred_tablet$ma3, type = "l", col = "red")
lines(pred_tablet$ma5, type = "l", col = "blue")

# add legend
legend("bottomright", c("Time Series", "Moving Average k=3", "Moving Average k=5"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))

# save the plot
jpeg("moving_avg.jpeg")
plot(unitsold ~ week, data = data_tablet, type = "l", xlim = c(0,18))
lines(pred_tablet$ma3, type = "l", col = "red")
lines(pred_tablet$ma5, type = "l", col = "blue")
legend("bottomright", c("Time Series", "Moving Average k=3", "Moving Average k=5"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))
dev.off()


### forecast using exponential smoothing 
# create a time series object from dataset
ts_tablet = ts(data_tablet$unitsold)

# create exponential smoothing, set smoothing constant alpha = 0.2
exps2 = HoltWinters(ts_tablet, alpha = 0.2, beta = F, gamma = F)

# take a look at the exponential smoothing fitted value sequence
exps2$fitted

# put fitted values from exponential smoothing back to dataframe
pred_tablet$exps2 = NA
pred_tablet$exps2[2:17] = exps2$fitted[,1]

# make forecast of week18
pred_tablet$exps2[18] = predict(exps2, n.ahead = 1)

# create exponential smoothing, set smoothing constant alpha = 0.6
exps6 = HoltWinters(ts_tablet, alpha = 0.6, beta = F, gamma = F)

# put fitted values from exponential smoothing back to dataframe
pred_tablet$exps6 = NA
pred_tablet$exps6[2:17] = exps6$fitted[,1]

# make forecast of week18
pred_tablet$exps6[18] = predict(exps6, n.ahead = 1)


# plot the time series and exponential smoothing
plot(unitsold ~ week, data = data_tablet, type = "l", xlim = c(0,18))
lines(pred_tablet$exps2, type = "l", col = "red")
lines(pred_tablet$exps6, type = "l", col = "blue")
legend("bottomright", c("Time Series", "Exponential Smoothing alpha=0.2", "Exponential Smoothing alpha=0.6"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))


# plot time series, moving average and exponential smoothing
plot(unitsold ~ week, data = data_tablet, type = "l", xlim = c(0,18))
lines(pred_tablet$ma3, type = "l", col = "red")
lines(pred_tablet$exps6, type = "l", col = "blue")
legend("bottomright", c("Time Series", "Moving Average k=3", "Exponential Smoothing alpha=0.6"), lty = 1, 
       cex = 0.5, col = c("black", "red", "blue"))

