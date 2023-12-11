#load in libraries needed to process the data
library(lubridate)
library(data.table)
library(dplyr)

#use the data from the large dara set with station by station activity
dailyactivity <- read.csv("data/tfl-bike-daily-activity-central-london.csv")

#Getting a list of all stations within London along with their postcodes
stationinfo <- read.csv("data/station-postcodes-central.csv")

#defining which postcodes are to be included in the output dataset c("W1","WC","EC")(all central london postcode first two letters)
twol <- c("EC1","EC2","EC3","EC4","WC1","WC2")

# inputs a list of stations with their postcodes and a list of the postcode
#areas you would like to include outputs a data frame of the stations within those
# postcodes along with a shortened form of their postcode
stationexrtraction.postcode <- function(stationset,postcodes){
  
  # reformatting postcodes to only include first three digits
  stationset <- mutate(stationset,Postcode = substr(Postcode,0,nchar(postcodes[1])))
  stationset.postcode <-stationset$Postcode
  
  # creating a blank dataset to be filled with only the stations within the predefined postcode list
  stationset.filtered <- data.frame(X=character(), 
                             Station=character(),
                             Postcode=character(),
                             Number=character(),
                             stringsAsFactors=FALSE)
  
  # runs through each station within station info and adding ones that lie within
  # the predefined area to the new station info list
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

#inputs a list of stations with their postcodes and a list of the postcode
#areas you would like to include. Outputs a list of the defined postcode 
#areas which with a sub list of all stations within that area
stationexrtraction.list <- function(stationset,postcodes){
  
  postcodestations <- list()
  for(i in 1:length(postcodes)){
    postcodestations[postcodes[i]] <- list(make.names(stationexrtraction.postcode(stationset,postcodes[i])$Station, unique=TRUE))
  }
  return(postcodestations)
}

#create the list of postcodes and their stations
stationinfon <- stationexrtraction.list(stationinfo,twol)

#splits a data frame into a new data frame containing only the locations within a postcode area
listofdataframes <- list()
ls <- list()
i = 1
for( i in 1:length(stationinfon)){
  ls <- append(stationinfon[[i]], "Date", after = 0)
  listofdataframes[[i]] <- dailyactivity[ls]
}

#loop through all the postcode stations and count their activity
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

# find means and standard deviation
postcode.activity.means <-colMeans(postcode.activity[-c(1,8)]) 
postcode.activity.standarddeviation <- sapply(postcode.activity[-c(1,8)], sd)

#add the weekday information to the data frame
postcode.activity["Weekday"] <- weekdays(postcode.activity$Date)

#export the data that now has a column for each central london postcode and the daily activity
#this data will be used by all other data processing files
write.csv(postcode.activity, "data/daily-activity-by-postcode.csv")

