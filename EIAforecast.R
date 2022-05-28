### R script for forecasting of oil prices based on historical data 

# We can use various methods in order to forecast future prices based on historical data
# In this case, we will be using the auto arima method in R. 
# In another jupyter notebook, we will be using a LSTM neural network in order to forecast stock prices. 

#Loading necessary packages#


# install.packages("forecast")
library(TTR) # for moving averages
library(forecast) # for auto.arima function 
library(readxl) # in order to read the excel file 
library(tidyverse) # in order for ggtitle()

?readxl

#Setting working directory#
setwd("~/Desktop/LevelUp!")

#Loading in the dataframe#
df <- read_excel("RWTCd.xls", sheet = "Data 1")
View(df)
# Looking at this dataframe, the first 2 rows are unnecessary. 
# We also need to change the column names to something that makes more sense. 
# We also need to change date number into something more realistic.

## Removing the first 2 rows 
df <- df[3:nrow(df),]
View(df)

## Renaming the columns 
colnames(df) <- c("Date","Price")
View(df)

## Changing the date number into something more realistic.
# Converting date from character into integer
df$Date <- lapply(df$Date, as.integer)

# Applying as.Date function to the 
df$Date <- lapply(df$Date, as.Date, origin = "1899-12-30")
df$Date <- lapply(df$Date, as.character)
View(df)
#Now, my dataframe consists of dates and the stock price of the respective day.

#Additionally, I want to round the Spot Prices of crude oil into 2 decimal places so that it is easier to work with.
df$Price <- lapply(df$Price, as.numeric)

#Now, my dataframe has 2 columns of price and date in more acceptable formats.
View(df)

#ARIMA forecasting 
lapply(df,class)
# 1 problem we encounter in this method is that the class of both these objects are lists, and we are unable to project them in this manner.


View(df)

ts <- ts(df$Price, frequency = 365, start = c(1986,1,1))

plot.ts(ts, ylab = "Spot Price of Crude Oil", xlab = "Year",
        main = "Spot Prices of Crude Oil over time")

# there is a slight error here because there are days in which the market closes. 
# the time series object does not take into account this
# and hence the data is only projected up until what appears to be 2010+

View(ts)

# Moving averages 
m.ma3 <- SMA(ts, n = 3*365)
plot(m.ma3, main = "Moving Avg Span 3", ylab = "MA3 Forecast")

autoplot(ts)

# In order to avoid the problem where there is "too much data", since ARIMA is more fit for a short term forecasting,

# We can subset the data such that we only use the previous 3 months of data in order to determine the future prices. 
length <- nrow(df)
months_cutoff <- 3*36
stop <- length - months_cutoff
df_trunc <- df[stop:length,]
# Now, the dataset only considers 108 data points. 
View(df_trunc)

# Basically just copying the data 
truncated <- df_trunc

# Splitting the truncated dataframe into trainset and testset 

y <- dim(truncated)[1]

trainSet <- truncated[1:(y-4),]
View(trainSet)

testSet <- truncated[(y-3):(y),]
View(testSet)


# Transforming the dataframe to timeseries
train_trunc_ts <- ts(trainSet[,'Price'],start=c(2021,12,9),frequency=1)


# Training the model 
model <- auto.arima(train_trunc_ts, seasonal = F )
print(summary(model))
checkresiduals(model) 
# Based on AIC, the model is more acceoptable as compared to training on the whole dataframe 


# Forecasting using ARIMA model
forecast_number <- nrow(testSet)
fcast <- forecast(model, h = forecast_number)

plot(fcast)
# Model is not very good as it is forecasting a "straight line" 
# In reality variance between each point forecasted is just very low
autoplot(fcast) + ggtitle("Forecasted spot prices of crude oil") + ylab("Price Sales") + xlab("Days")

print(summary(fcast))






