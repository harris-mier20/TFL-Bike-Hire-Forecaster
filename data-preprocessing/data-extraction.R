#install libraries to process data
library(lubridate)
library(data.table)
library(dplyr)

# Days and month variables for formatting requist csv
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
days <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

#Getting a list of all stations within London along with their postcodes
stationinfo <- read.csv("station-postcodes.csv")

#defining which postcodes are to be included in the output dataset c("W1","WC","EC")(all central london postcode first two letters)
twol <- c("WC")

# reformatting postcodes to only include first two digits
stationinfo <- mutate(stationinfo,Postcode = substr(Postcode,0,2))
stationinfo.postcode <-stationinfo$Postcode

# creating a blank dataset to be filled with only the stations within the predifned postcode list
stationinfon <- data.frame(X=character(), 
                           Station=character(),
                           Postcode=character(),
                           Number=character(),
                           stringsAsFactors=FALSE)

# runs through each station within station info and adding ones that lie within
# the predifed area to the new station info list
n  <- 0
for (i in 1:nrow(stationinfo)) {
  if (stationinfo.postcode[i] %in% twol){
    n <- n +1
    stationinfon[nrow(stationinfon) + 1,] = stationinfo[i,]
    print(i)
  }
  else{
  }
}
stationinfon.station <-stationinfon$Station        

# reformats dlpr date format into the format required to call the csv
datetostr <- function(date_in){
  paste(as.character(days[day(date_in)]),as.character(months[month(date_in)]),as.character(year(date_in)), sep ="")
}

# function that takes an input date(12Sep2022) and converts it to a date format
#that dplr recognises
strtodate <- function(z){
  z <- tolower(z) 
  as.Date(z, "%d%b%Y") # converts to date(year,month(jan,feb,mar,...),day)
}

# takes "12Sep2022" input date and generates the following link from
#https://cycling.data.tfl.gov.uk/usage-stats
urlgen <- function(strin,num){
  # generating next start date of csv
  ns <- strtodate(strin) + days(7)
  # generating next end date of csv
  ne <- strtodate(strin) + days(13)
  
  nstart <- datetostr(ns)
  nend <- datetostr(ne)
  #combining start and end dates
  range <- paste(nstart,nend,sep = "-")
  #stitching date into csv request format
  paste("https://cycling.data.tfl.gov.uk/usage-stats/",num,"JourneyDataExtract",range,".csv", sep = "")
}

#pulling the start date out of link
dateextract <- function(link){
  substr(link,nchar(link)-22,nchar(link)-14)
}

#pulling the link id out of link
numextract <- function(link){
  as.integer(substr(link,nchar(link)-43,nchar(link)-41))
}

#a function for extracting all data from the new data format at
#https://cycling.data.tfl.gov.uk/usage-stats takes a starting link as an 
#input and outputs a dataframe of combined data
newdataformatextraction <- function(startinglink){
  #extracting starting id form link
  startnum <- numextract(startinglink) +1
  #defining list to contain all pull urls
  labels <- list()
  newlink <- startinglink
  
  #generates a list fo csv urls between the string link and link 375
  for (i in startnum:375) {
    labels <- append(labels, newlink, after = length(labels))
    curlink <- newlink
    newlink <- urlgen(dateextract(curlink),i)
  }
  
  # format an empty data frame for the combined data to be stored in
  dataframe <- data.frame(#Number=character(), 
                          Start.date=character(),
                          #Start.station.number=character(),
                          Start.station=character(),
                          End.date=character(),
                          #End.station.number=character(),
                          End.station=character(),
                          #Bike.number=character(),
                          #Total.duration..ms.=character(),
                          stringsAsFactors=FALSE)
  
  # loops through every item in the csv list and downloads the csv before 
  #reformatting the titles and subsisting unnecessary rows so that it cna be 
  # combined with the new data format
  
  for (i in 1:((375-startnum)+1)) {
    nframe <- read.csv(labels[[i]])
    
    # loops through every item in the csv list and downloads the csv bbefore 
    #reformatting the titles and subseting uneceseery rows so that it cna be 
    # combined with the new data format
    nframe <- nframe %>%  filter((Start.station %in% c(stationinfon.station))== TRUE | (End.station %in% c(stationinfon.station))== TRUE)
    
    
    nframe = subset(nframe, select = -c(Bike.model,Total.duration,Total.duration..ms.,Bike.number,End.station.number,Start.station.number,Number))
    dataframe = rbind(dataframe,nframe)
    print(i)
  }
  return(dataframe)
}

newdata <- newdataformatextraction("https://cycling.data.tfl.gov.uk/usage-stats/335JourneyDataExtract12Sep2022-18Sep2022.csv")

#a function for extracting all data from the old data format at
#https://cycling.data.tfl.gov.uk/usage-stats takes a starting link as an 
#input and outputs a dataframe of combined data
olddataformatextraction <- function(startinglink){
  #extracting starting id form link
  startnum <- numextract(startinglink) +1
  #defining list to contain all pull urls
  csvlist <- list()
  holdlink <- startinglink
   
  # the data formatted at file with the id 246 has a repeated id
  # meaning that every datpoint after has to be pulled back an id
  lock <- 246
  
  #generates a list fo csv urls between the string link and link 335
  for (n in startnum:335) {
    csvlist <- append(csvlist, holdlink, after = length(csvlist))
    templink <- holdlink
    if (n > lock){
      n <- n -1
    }
    holdlink <- urlgen(dateextract(templink),n)
  }
  
  # format an empty data frame for the combined data to be stored in
  olddata <- data.frame(Number=character(), 
                         Start.date=character(),
                         #Start.station.number=character(),
                         Start.station=character(),
                         End.date=character(),
                         #End.station.number=character(),
                         End.station=character(),
                         #Bike.number=character(),
                         #Total.duration..ms.=character(),
                         stringsAsFactors=FALSE)
  
  # a list of titles for the old data fromat to have its titles relabled to
  titles <- c("Number","Start.date","Start.station.number","Start.station",
              "End.date","End.station.number","End.station","Bike.number",
              "Total.duration..ms.")
  
  # loops through every item in the csv list and downloads the csv bbefore 
  #reformatting the titles and subseting uneceseery rows so that it cna be 
  # combined with the new data format
  for (x in 1:((335-startnum)+1)) {
    frame <- read.csv(csvlist[[x]])
    
    # the formatting of file 206 raised errors so it had to be skippped
    if (x != 206){
      
      # reordering columns to suit new format
      # a way to subset and rorder rows
      nframe <- frame[, c(1,7,8,9,4,5,6,3,2)]
      
      #Chaning colmun names to match new format
      colnames(nframe) <- titles
      
      # filter function to remove any data that does not have at least a 
      #1 stop or start station within the predifined post codes of interest
      nframe <- nframe %>%  filter((Start.station %in% c(stationinfon.station))== TRUE
                                   | (End.station %in% c(stationinfon.station))== TRUE)
      
      # removing unecessery columns to reduce dataframe size
      nframe <- subset(nframe, select = -c(Total.duration..ms.,Bike.number,End.station.number,Start.station.number,Number))
      
      #combining edited dataframe with prior cumulated dataframe
      olddata <- rbind(olddata,nframe)
    }
    
    # just a ticker to inform operator how far through the download is
    print(x)
  }

  return(olddata)
}

# run old data extract
olddata <- olddataformatextraction("https://cycling.data.tfl.gov.uk/usage-stats/121JourneyDataExtract01Aug2018-07Aug2018.csv")

#combining old dtaa format with new data fromat downloads
#although they are now the same format
Finalset <- rbind(olddata,newdata)

# output as one csv
write.csv(Finalset, file = "data/WC_data.csv")
