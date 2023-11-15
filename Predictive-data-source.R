library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/Rory.Bateman/OneDrive/Documents/GitHub/TFL-Bike-Hire-Forecaster")

#- [ ]  Create new data frame where each row is a day. Columns are:
#  - [ ]  Y - demand on that day
# - [ ]  X1 - demand 7 days earlier
#- [ ]  X2 - demand of 1 month earlier
#- [ ]  X3 - demand of 1 year earlier
#- [ ]  X4 - mean of last 7 days
#- [ ]  X5 - mean of last month
#- [ ]  X6 - mean of last 6 months
#- [ ]  X7 - temperature on the day
#- [ ]  X7 - day of week

dailyactivity <- read.csv("daily-activity-by-postcode.csv")

weatherdata <- read.csv("weather-data.csv")

Location.features <- data.frame(matrix(NA, nrow = nrow(dailyactivity), ncol = 10))

colnames(Location.features) <- c("Date", "Daydemand", "Demand7daysago", "Demand30daysago", "Demand365daysago", "Meanpast7days", "Meanpast30days", "Meanpast365days", "Tempofday","dayofweek")

Location.features$Date <- dailyactivity$Date



dataimputaion <- function(inputdata,days){
  ls <- inputdata
  imputationdata <- rep(list(mean(inputdata)),days)
  ls <- append(ls, imputationdata, after = 0)
  ls <- ls[-c((1328-(days-1)):1328)]
  return(ls)
  }


meaninputs <- function(inputdata,days){
  output <- ls()
  i <- days
  ls <- inputdata
  output <- rep(list(mean(inputdata)),days)
  
  for (i in days:nrow(Location.features.WC1)) {
    output[i] <- mean(ls[c((i-days):i)])
  }
  return(output)
}

###creating wc1###
Location.features.WC1 <- Location.features

Location.features.WC1$Daydemand <- dailyactivity$WC1

Location.features.WC1$dayofweek <- dailyactivity$Weekday

Location.features.WC1$Demand7daysago <- dataimputaion(Location.features.WC1$Daydemand,7)
Location.features.WC1$Demand30daysago <- dataimputaion(Location.features.WC1$Daydemand,30)
Location.features.WC1$Demand365daysago <- dataimputaion(Location.features.WC1$Daydemand,365)

Location.features.WC1$Meanpast7days <- meaninputs(Location.features.WC1$Daydemand,7)
Location.features.WC1$Meanpast30days <- meaninputs(Location.features.WC1$Daydemand,30)
Location.features.WC1$Meanpast365days <- meaninputs(Location.features.WC1$Daydemand,365)


Location.features.WC1 <- apply(Location.features.WC1,2,as.character)
write.csv(Location.features.WC1, "Location-feature-data\\WC1-feature-data.CSV", row.names=FALSE)

###creating WC2###
Location.features.WC2 <- Location.features

Location.features.WC2$Daydemand <- dailyactivity$WC2

Location.features.WC2$dayofweek <- dailyactivity$Weekday

Location.features.WC2$Demand7daysago <- dataimputaion(Location.features.WC2$Daydemand,7)
Location.features.WC2$Demand30daysago <- dataimputaion(Location.features.WC2$Daydemand,30)
Location.features.WC2$Demand365daysago <- dataimputaion(Location.features.WC2$Daydemand,365)

Location.features.WC2$Meanpast7days <- meaninputs(Location.features.WC2$Daydemand,7)
Location.features.WC2$Meanpast30days <- meaninputs(Location.features.WC2$Daydemand,30)
Location.features.WC2$Meanpast365days <- meaninputs(Location.features.WC2$Daydemand,365)


Location.features.WC2 <- apply(Location.features.WC2,2,as.character)
write.csv(Location.features.WC2, "Location-feature-data\\WC2-feature-data.CSV", row.names=FALSE)

###creating EC1###
Location.features.EC1 <- Location.features

Location.features.EC1$Daydemand <- dailyactivity$EC1

Location.features.EC1$dayofweek <- dailyactivity$Weekday

Location.features.EC1$Demand7daysago <- dataimputaion(Location.features.EC1$Daydemand,7)
Location.features.EC1$Demand30daysago <- dataimputaion(Location.features.EC1$Daydemand,30)
Location.features.EC1$Demand365daysago <- dataimputaion(Location.features.EC1$Daydemand,365)

Location.features.EC1$Meanpast7days <- meaninputs(Location.features.EC1$Daydemand,7)
Location.features.EC1$Meanpast30days <- meaninputs(Location.features.EC1$Daydemand,30)
Location.features.EC1$Meanpast365days <- meaninputs(Location.features.EC1$Daydemand,365)


Location.features.EC1 <- apply(Location.features.EC1,2,as.character)
write.csv(Location.features.EC1, "Location-feature-data\\EC1-feature-data.CSV", row.names=FALSE)

###creating EC2###
Location.features.EC2 <- Location.features

Location.features.EC2$Daydemand <- dailyactivity$EC2

Location.features.EC2$dayofweek <- dailyactivity$Weekday

Location.features.EC2$Demand7daysago <- dataimputaion(Location.features.EC2$Daydemand,7)
Location.features.EC2$Demand30daysago <- dataimputaion(Location.features.EC2$Daydemand,30)
Location.features.EC2$Demand365daysago <- dataimputaion(Location.features.EC2$Daydemand,365)

Location.features.EC2$Meanpast7days <- meaninputs(Location.features.EC2$Daydemand,7)
Location.features.EC2$Meanpast30days <- meaninputs(Location.features.EC2$Daydemand,30)
Location.features.EC2$Meanpast365days <- meaninputs(Location.features.EC2$Daydemand,365)


Location.features.EC2 <- apply(Location.features.EC2,2,as.character)
write.csv(Location.features.EC2, "Location-feature-data\\EC2-feature-data.CSV", row.names=FALSE)

###creating EC3###
Location.features.EC3 <- Location.features

Location.features.EC3$Daydemand <- dailyactivity$EC3

Location.features.EC3$dayofweek <- dailyactivity$Weekday

Location.features.EC3$Demand7daysago <- dataimputaion(Location.features.EC3$Daydemand,7)
Location.features.EC3$Demand30daysago <- dataimputaion(Location.features.EC3$Daydemand,30)
Location.features.EC3$Demand365daysago <- dataimputaion(Location.features.EC3$Daydemand,365)

Location.features.EC3$Meanpast7days <- meaninputs(Location.features.EC3$Daydemand,7)
Location.features.EC3$Meanpast30days <- meaninputs(Location.features.EC3$Daydemand,30)
Location.features.EC3$Meanpast365days <- meaninputs(Location.features.EC3$Daydemand,365)


Location.features.EC3 <- apply(Location.features.EC3,2,as.character)
write.csv(Location.features.EC3, "Location-feature-data\\EC3-feature-data.CSV", row.names=FALSE)

###creating EC4###
Location.features.EC4 <- Location.features

Location.features.EC4$Daydemand <- dailyactivity$EC4

Location.features.EC4$dayofweek <- dailyactivity$Weekday

Location.features.EC4$Demand7daysago <- dataimputaion(Location.features.EC4$Daydemand,7)
Location.features.EC4$Demand30daysago <- dataimputaion(Location.features.EC4$Daydemand,30)
Location.features.EC4$Demand365daysago <- dataimputaion(Location.features.EC4$Daydemand,365)

Location.features.EC4$Meanpast7days <- meaninputs(Location.features.EC4$Daydemand,7)
Location.features.EC4$Meanpast30days <- meaninputs(Location.features.EC4$Daydemand,30)
Location.features.EC4$Meanpast365days <- meaninputs(Location.features.EC4$Daydemand,365)


Location.features.EC4 <- apply(Location.features.EC4,2,as.character)
write.csv(Location.features.EC4, "Location-feature-data\\EC4-feature-data.CSV", row.names=FALSE)

