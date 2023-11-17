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

#reading data in
dailyactivity <- read.csv("data/daily-activity-by-postcode.csv")
weatherdata <- read.csv("data/weather-data.csv")

dailyactivity.train.begin <- which(dailyactivity$Date=="2020-06-01")

dailyactivity.train.end <- which(dailyactivity$Date=="2022-07-05")

dailyactivity.train<- dailyactivity[dailyactivity.train.begin:dailyactivity.train.end,]

#training data starts from 2020-06-01, test data ends on 2022-07-05, forecast dates end on 2024-12-31
#from the weather data, remove all the rows that have dates on the 29th and beyond in each month
#to fit with the training and test data
weather_train_data <- weatherdata[1:765,]

weather_train_data.1_28 <- selectByDate(weather_train_data, day = seq(1,28,1))

#creating a empty template dataframe
Location.features <- data.frame(matrix(NA, nrow = nrow(dailyactivity.train), ncol = 13))
colnames(Location.features) <- c("Date", "Daydemand", "Demand1dayago", "Demand7daysago",
                                 "Demand30daysago", "Demand365daysago", "Meanpast7days",
                                 "Meanpast30days", "Meanpast365days", "Tempofday",
                                 "Wind","Raincover","dayofweek")

Location.features$Date <- dailyactivity.train$Date
Location.features$Tempofday <- weather_train_data.1_28$temp
Location.features$Raincover <- weather_train_data.1_28$precipcover
Location.features$Wind <- weather_train_data.1_28$windspeed

# function which phase shifts an input data by amount of days infills blank space
#with mean of entire dataset.
dataimputaion <- function(inputdata,days){
  ls <- inputdata
  imputationdata <- rep(list(mean(inputdata)),days)
  ls <- append(ls, imputationdata, after = 0)


  ls <- ls[dailyactivity.train.begin:dailyactivity.train.end]

  return(ls)
  }

# function which phase shifts an input data by amount of days infills blank space
#with mean of entire dataset, as its phase shifting it fills the new dataset 
#with the mean values of the priot "days" number of days#
meaninputs <- function(inputdata,days){
  output <- ls()
  i <- days
  ls <- inputdata
  output <- rep(list(mean(inputdata)),days)
  
  for (i in days:nrow(Location.features.WC1)) {
    output[i] <- mean(ls[c((i-days):i)])
  }
  print(output)
  output <- output[dailyactivity.train.begin:dailyactivity.train.end]
  return(output)
}

###creating wc1###
Location.features.WC1 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.WC1$Daydemand <- dailyactivity.train$WC1[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.WC1$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.WC1$Demand1dayago <- dataimputaion(dailyactivity.train$WC1,1)
Location.features.WC1$Demand7daysago <- dataimputaion(dailyactivity.train$WC1,7)
Location.features.WC1$Demand30daysago <- dataimputaion(dailyactivity.train$WC1,28)
Location.features.WC1$Demand365daysago <- dataimputaion(dailyactivity.train$WC1,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.WC1$Meanpast7days <- meaninputs(dailyactivity.train$WC1,7)
Location.features.WC1$Meanpast30days <- meaninputs(dailyactivity.train$WC1,28)
Location.features.WC1$Meanpast365days <- meaninputs(dailyactivity.train$WC1,365)

#outputs the dat toa csv file
Location.features.WC1 <- apply(Location.features.WC1,2,as.character)
write.csv(Location.features.WC1, "data\\WC1-feature-data.CSV", row.names=FALSE)

###creating WC2###
Location.features.WC2 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.WC2$Daydemand <- dailyactivity.train$WC2[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.WC2$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.WC2$Demand1dayago <- dataimputaion(dailyactivity.train$WC2,1)
Location.features.WC2$Demand7daysago <- dataimputaion(dailyactivity.train$WC2,7)
Location.features.WC2$Demand30daysago <- dataimputaion(dailyactivity.train$WC2,30)
Location.features.WC2$Demand365daysago <- dataimputaion(dailyactivity.train$WC2,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.WC2$Meanpast7days <- meaninputs(dailyactivity.train$WC2,7)
Location.features.WC2$Meanpast30days <- meaninputs(dailyactivity.train$WC2,30)
Location.features.WC2$Meanpast365days <- meaninputs(dailyactivity.train$WC2,365)

#outputs the dat toa csv file
Location.features.WC2 <- apply(Location.features.WC2,2,as.character)
write.csv(Location.features.WC2, "data\\WC2-feature-data.CSV", row.names=FALSE)

###creating EC1###
Location.features.EC1 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC1$Daydemand <- dailyactivity.train$EC1[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.EC1$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC1$Demand1dayago <- dataimputaion(dailyactivity.train$EC1,1)
Location.features.EC1$Demand7daysago <- dataimputaion(dailyactivity.train$EC1,7)
Location.features.EC1$Demand30daysago <- dataimputaion(dailyactivity.train$EC1,30)
Location.features.EC1$Demand365daysago <- dataimputaion(dailyactivity.train$EC1,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC1$Meanpast7days <- meaninputs(dailyactivity.train$EC1,7)
Location.features.EC1$Meanpast30days <- meaninputs(dailyactivity.train$EC1,30)
Location.features.EC1$Meanpast365days <- meaninputs(dailyactivity.train$EC1,365)

#outputs the dat toa csv file
Location.features.EC1 <- apply(Location.features.EC1,2,as.character)
write.csv(Location.features.EC1, "data\\EC1-feature-data.CSV", row.names=FALSE)

###creating EC2###
Location.features.EC2 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC2$Daydemand <- dailyactivity.train$EC2[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.EC2$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC2$Demand1dayago <- dataimputaion(dailyactivity.train$EC2,1)
Location.features.EC2$Demand7daysago <- dataimputaion(dailyactivity.train$EC2,7)
Location.features.EC2$Demand30daysago <- dataimputaion(dailyactivity.train$EC2,30)
Location.features.EC2$Demand365daysago <- dataimputaion(dailyactivity.train$EC2,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC2$Meanpast7days <- meaninputs(dailyactivity.train$EC2,7)
Location.features.EC2$Meanpast30days <- meaninputs(dailyactivity.train$EC2,30)
Location.features.EC2$Meanpast365days <- meaninputs(dailyactivity.train$EC2,365)

#outputs the dat toa csv file
Location.features.EC2 <- apply(Location.features.EC2,2,as.character)
write.csv(Location.features.EC2, "data\\EC2-feature-data.CSV", row.names=FALSE)

###creating EC3###
Location.features.EC3 <- Location.features
#inserts daily demand of the area and the day of the week
Location.features.EC3$Daydemand <- dailyactivity.train$EC3[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.EC3$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC3$Demand1dayago <- dataimputaion(dailyactivity.train$EC3,1)
Location.features.EC3$Demand7daysago <- dataimputaion(dailyactivity.train$EC3,7)
Location.features.EC3$Demand30daysago <- dataimputaion(dailyactivity.train$EC3,30)
Location.features.EC3$Demand365daysago <- dataimputaion(dailyactivity.train$EC3,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC3$Meanpast7days <- meaninputs(dailyactivity.train$EC3,7)
Location.features.EC3$Meanpast30days <- meaninputs(dailyactivity.train$EC3,30)
Location.features.EC3$Meanpast365days <- meaninputs(dailyactivity.train$EC3,365)

#outputs the dat toa csv file
Location.features.EC3 <- apply(Location.features.EC3,2,as.character)
write.csv(Location.features.EC3, "data\\EC3-feature-data.CSV", row.names=FALSE)

###creating EC4###
Location.features.EC4 <- Location.features

Location.features.EC4$Daydemand <- dailyactivity.train$EC4[dailyactivity.train.begin:dailyactivity.train.end]
Location.features.EC4$dayofweek <- dailyactivity.train$Weekday
#inserts data for linking the current days demand with the day 7,30 and 365 days
#ago's demand
Location.features.EC4$Demand1dayago <- dataimputaion(dailyactivity.train$EC4,1)
Location.features.EC4$Demand7daysago <- dataimputaion(dailyactivity.train$EC4,7)
Location.features.EC4$Demand30daysago <- dataimputaion(dailyactivity.train$EC4,30)
Location.features.EC4$Demand365daysago <- dataimputaion(dailyactivity.train$EC4,365)
#inserts data for linking the current days demand with the past 7,30 and 365 days
#means
Location.features.EC4$Meanpast7days <- meaninputs(dailyactivity.train$EC4,7)
Location.features.EC4$Meanpast30days <- meaninputs(dailyactivity.train$EC4,30)
Location.features.EC4$Meanpast365days <- meaninputs(dailyactivity.train$EC4,365)

#outputs the dat toa csv file
Location.features.EC4 <- apply(Location.features.EC4,2,as.character)
write.csv(Location.features.EC4, "data\\EC4-feature-data.CSV", row.names=FALSE)

