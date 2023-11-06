library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)

#set the wd 
#setwd("C:/Users/Rory.Bateman/OneDrive/Documents/GitHub/TFL-Bike-Hire-Forecaster")

dailyactivity <- read.csv("tfl-bike-daily-activity-central-london.csv")

#Getting a list of all stations within London along with their postcodes
stationinfo <- read.csv("station-postcodes-central.csv")

#defining which postcodes are to be included in the output dataset c("W1","WC","EC")(all central london postcode first two letters)
twol <- c("EC1","EC2","EC3","EC4","WC1","WC2")

# inputs a list of stations with their postcodes and a list of the postcode
#areas you would like to include outputs a data frame of the stations within those
# postcodes along with a shortened form of their postcode
stationexrtraction.postcode <- function(stationset,postcodes){
  # reformatting postcodes to only include first three digits
  stationset <- mutate(stationset,Postcode = substr(Postcode,0,nchar(postcodes[1])))
  stationset.postcode <-stationset$Postcode
  
  # creating a blank dataset to be filled with only the stations within the predifned postcode list
  stationset.filtered <- data.frame(X=character(), 
                             Station=character(),
                             Postcode=character(),
                             Number=character(),
                             stringsAsFactors=FALSE)
  
  # runs through each station within station info and adding ones that lie within
  # the predifed area to the new station info list
  n  <- 0
  for (i in 1:nrow(stationset)) {
    if (stationset.postcode[i] %in% postcodes){
      n <- n +1
      stationset.filtered[nrow(stationset.filtered) + 1,] = stationset[i,]
    }
    else{
    }
  }
  return(stationset.filtered)
}

### inputs a list of stations with their postcodes and a list of the postcode
#areas you would like to include outputs a list of the defined postcode 
#areas wach with a sub list of all stations within that area###
stationexrtraction.list <- function(stationset,postcodes){
  
  postcodestations <- list()
  for(i in 1:length(postcodes)){
    postcodestations[postcodes[i]] <- list(make.names(stationexrtraction.postcode(stationset,postcodes[i])$Station, unique=TRUE))
  }
  return(postcodestations)
}

stationinfon <- stationexrtraction.list(stationinfo,twol)
### splits a data frame into a new data frame containing only the locations within a 
#a postcode area###

listofdataframes <- list()
ls <- list()
i = 1
for( i in 1:length(stationinfon)){
  ls <- append(stationinfon[[i]], "Date", after = 0)
  listofdataframes[[i]] <- dailyactivity[ls]
}

i = 1
postcode.activity<- data.frame(matrix(NA, nrow = 1321, ncol = 0))
dailyactivity.date <- data.frame()
for( i in 1:length(stationinfon)){
  df <- listofdataframes[[i]]
  dailyactivity.date <- as.Date(df$Date,format = "%d/%m/%Y")
  postcode.activity["Date"] <- dailyactivity.date
  df <- df[-c(1)]
  listofdataframes[[i]]
  df <- rowSums(df)
  postcode.activity[twol[i]] <- df
}

postcode.activity.means <-colMeans(postcode.activity[-c(1,8)]) 

postcode.activity.standarddeviation <- sapply(postcode.activity[-c(1,8)], sd)

postcode.activity["weekday"] <- weekdays(postcode.activity$Date)
ggplot(postcode.activity) +geom_point(aes(Date, WC1,color=ifelse(weekday %in% c("Friday","Saturday","Sunday"), 'red', 'black')))+ theme(legend.position="none")


