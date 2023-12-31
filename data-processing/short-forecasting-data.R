#load in libraries that are needed to restructure the data to extract features for training
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(openair)

#- [ ]  Create new data frame where each row is a day. Columns are:
#- [x]  Y - demand on that day
#- [x]  X1 - demand of day before
#- [x]  X2 - demand 7 days earlier
#- [x]  X3 - demand of 1 month earlier
#- [x]  X4 - demand of 1 year earlier
#- [x]  X5 - mean of last 7 days
#- [x]  X6 - mean of last month
#- [x]  X7 - mean of last 6 months
#- [x]  X8 - temperature on the day
#- [x]  X9 - (1) if work week (-1) if weekend

#reading data from the daily activity for each postcode and weather data obtained from here https://www.visualcrossing.com/weather-api
dailyactivity <- read.csv("data/daily-activity-by-postcode.csv")
weatherdata <- read.csv("data/weather-data.csv")

#function to exponentially smooth data
smooth_data <- function(data,alpha,starting_value){
  
  #define parameters
  aa <- alpha
  l0 <- starting_value
  
  #initiate vector to store smoothed data
  smoothed_data <- numeric(length(data))
  
  #loop through the data and perform exponential smoothing
  for (i in 1:length(smoothed_data)) {
    smoothed_data[i] <- aa * data[i] + (1 - aa) * l0
    l0 <- smoothed_data[i]
  }
  return(smoothed_data)
}

#loop through all the postcodes in the defined training data and smooth each column
#with exponential smoothing
for (i in 3:8){
  dailyactivity[[i]] <- smooth_data(dailyactivity[[i]],0.1,1250)
}

#training data starts from 2020-06-01, test data ends on 2022-07-05,
#split out data after june 2020 to start training
dailyactivity.train.begin <- which(dailyactivity$Date=="2020-06-01")
dailyactivity.train.end <- which(dailyactivity$Date=="2022-07-05")
dailyactivity.train<- dailyactivity[dailyactivity.train.begin:dailyactivity.train.end,]

#weather forecasts end on 2024-12-31 from the weather data, remove all the rows that have
#dates on the 29th and beyond in each month to fit with the training and test data
weather_train_data <- weatherdata[1:765,]
weather_train_data.1_28 <- selectByDate(weather_train_data, day = seq(1,28,1))

#creating a empty template dataframe
Location.features <- data.frame(matrix(NA, nrow = nrow(dailyactivity.train), ncol = 13))
colnames(Location.features) <- c("Date", "Daydemand", "Demand1dayago", "Demand7daysago",
                                 "Demand30daysago", "Demand365daysago", "Meanpast7days",
                                 "Meanpast30days", "Meanpast365days", "Tempofday",
                                 "Wind","Raincover","dayofweek")

#fill in template with features that are common to all postcodes
#the dates and weather data
Location.features$Date <- dailyactivity.train$Date
Location.features$Tempofday <- weather_train_data.1_28$temp
Location.features$Raincover <- weather_train_data.1_28$precipcover
Location.features$Wind <- weather_train_data.1_28$windspeed

# function which phase shifts an input data by amount of days infills blank space
#with mean of entire dataset.
dataimputation <- function(inputdata,days){
  ls <- inputdata
  imputationdata <- rep(list(mean(inputdata)),days)
  ls <- append(ls, imputationdata, after = 0)
  ls <- ls[dailyactivity.train.begin:dailyactivity.train.end]
  
  return(ls)
}

#function that takes an index and a number of days
#returns the mean of that number of days before that index in the data set
find_mean <- function(index,data,days){
  values_to_check = numeric()
  for (i in 1:days){
    values_to_check <- c(values_to_check,data[index-i])
  }
  return(mean(values_to_check))
}

#function that takes a postcode number of days and returns an array that is the mean
#for those days previous days for each data point in the training data for that postcode
mean_array <- function(data, days){
  means <- numeric()
  for (i in 1:length(data)){
    means <- c(means, find_mean((dailyactivity.train.begin-1+i),data,days))
  }
  means <- means[1:length(dailyactivity.train$EC1)]
}

###creating wc1###
Location.features.WC1 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.WC1$Daydemand <- dailyactivity.train$WC1
Location.features.WC1$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.WC1$Demand1dayago <- dataimputation(dailyactivity$WC1,1)
Location.features.WC1$Demand7daysago <- dataimputation(dailyactivity$WC1,7)
Location.features.WC1$Demand30daysago <- dataimputation(dailyactivity$WC1,28)
Location.features.WC1$Demand365daysago <- dataimputation(dailyactivity$WC1,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.WC1$Meanpast7days <- mean_array(dailyactivity$WC1,7)
Location.features.WC1$Meanpast30days <- mean_array(dailyactivity$WC1,28)
Location.features.WC1$Meanpast365days <- mean_array(dailyactivity$WC1,365)

#outputs the data to a csv file
Location.features.WC1 <- apply(Location.features.WC1,2,as.character)
write.csv(Location.features.WC1, "data/feature-data/WC1-feature-data.CSV", row.names=FALSE)

###creating WC2###
Location.features.WC2 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.WC2$Daydemand <- dailyactivity.train$WC2
Location.features.WC2$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.WC2$Demand1dayago <- dataimputation(dailyactivity$WC2,1)
Location.features.WC2$Demand7daysago <- dataimputation(dailyactivity$WC2,7)
Location.features.WC2$Demand30daysago <- dataimputation(dailyactivity$WC2,30)
Location.features.WC2$Demand365daysago <- dataimputation(dailyactivity$WC2,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.WC2$Meanpast7days <- mean_array(dailyactivity$WC2,7)
Location.features.WC2$Meanpast30days <- mean_array(dailyactivity$WC2,30)
Location.features.WC2$Meanpast365days <- mean_array(dailyactivity$WC2,365)

#outputs the data to a csv file
Location.features.WC2 <- apply(Location.features.WC2,2,as.character)
write.csv(Location.features.WC2, "data/feature-data/WC2-feature-data.CSV", row.names=FALSE)

###creating EC1###
Location.features.EC1 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC1$Daydemand <- dailyactivity.train$EC1
Location.features.EC1$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC1$Demand1dayago <- dataimputation(dailyactivity$EC1,1)
Location.features.EC1$Demand7daysago <- dataimputation(dailyactivity$EC1,7)
Location.features.EC1$Demand30daysago <- dataimputation(dailyactivity$EC1,30)
Location.features.EC1$Demand365daysago <- dataimputation(dailyactivity$EC1,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC1$Meanpast7days <- mean_array(dailyactivity$EC1,7)
Location.features.EC1$Meanpast30days <- mean_array(dailyactivity$EC1,30)
Location.features.EC1$Meanpast365days <- mean_array(dailyactivity$EC1,365)

#outputs the data to a csv file
Location.features.EC1 <- apply(Location.features.EC1,2,as.character)
write.csv(Location.features.EC1, "data/feature-data/EC1-feature-data.CSV", row.names=FALSE)

###creating EC2###
Location.features.EC2 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC2$Daydemand <- dailyactivity.train$EC2
Location.features.EC2$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC2$Demand1dayago <- dataimputation(dailyactivity$EC2,1)
Location.features.EC2$Demand7daysago <- dataimputation(dailyactivity$EC2,7)
Location.features.EC2$Demand30daysago <- dataimputation(dailyactivity$EC2,30)
Location.features.EC2$Demand365daysago <- dataimputation(dailyactivity$EC2,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC2$Meanpast7days <- mean_array(dailyactivity$EC2,7)
Location.features.EC2$Meanpast30days <- mean_array(dailyactivity$EC2,30)
Location.features.EC2$Meanpast365days <- mean_array(dailyactivity$EC2,365)

#outputs the data to a csv file
Location.features.EC2 <- apply(Location.features.EC2,2,as.character)
write.csv(Location.features.EC2, "data/feature-data/EC2-feature-data.CSV", row.names=FALSE)

###creating EC3###
Location.features.EC3 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC3$Daydemand <- dailyactivity.train$EC3
Location.features.EC3$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC3$Demand1dayago <- dataimputation(dailyactivity$EC3,1)
Location.features.EC3$Demand7daysago <- dataimputation(dailyactivity$EC3,7)
Location.features.EC3$Demand30daysago <- dataimputation(dailyactivity$EC3,30)
Location.features.EC3$Demand365daysago <- dataimputation(dailyactivity$EC3,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC3$Meanpast7days <- mean_array(dailyactivity$EC3,7)
Location.features.EC3$Meanpast30days <- mean_array(dailyactivity$EC3,30)
Location.features.EC3$Meanpast365days <- mean_array(dailyactivity$EC3,365)

#outputs the data to a csv file
Location.features.EC3 <- apply(Location.features.EC3,2,as.character)
write.csv(Location.features.EC3, "data/feature-data/EC3-feature-data.CSV", row.names=FALSE)

###creating EC4###
Location.features.EC4 <- Location.features

Location.features.EC4$Daydemand <- dailyactivity.train$EC4
Location.features.EC4$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC4$Demand1dayago <- dataimputation(dailyactivity$EC4,1)
Location.features.EC4$Demand7daysago <- dataimputation(dailyactivity$EC4,7)
Location.features.EC4$Demand30daysago <- dataimputation(dailyactivity$EC4,30)
Location.features.EC4$Demand365daysago <- dataimputation(dailyactivity$EC4,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC4$Meanpast7days <- mean_array(dailyactivity$EC4,7)
Location.features.EC4$Meanpast30days <- mean_array(dailyactivity$EC4,30)
Location.features.EC4$Meanpast365days <- mean_array(dailyactivity$EC4,365)

#outputs the data to a csv file
Location.features.EC4 <- apply(Location.features.EC4,2,as.character)
write.csv(Location.features.EC4, "data/feature-data/EC4-feature-data.CSV", row.names=FALSE)


