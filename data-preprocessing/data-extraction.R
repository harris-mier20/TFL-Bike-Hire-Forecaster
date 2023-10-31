library(lubridate)
library(data.table)
library(dplyr)

rorymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
rorydays <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

stationinfo <- read.csv("station-postcodes.csv")

#twol <- c("W1","WC","EC")
twol <- c("WC")

stationinfo <- mutate(stationinfo,Postcode = substr(Postcode,0,2))
stationinfo.postcode <-stationinfo$Postcode

stationinfon <- data.frame(X=character(), 
                           Station=character(),
                           Postcode=character(),
                           Number=character(),
                           stringsAsFactors=FALSE)
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


datetostr <- function(date_in){
  
  paste(as.character(rorydays[day(date_in)]),as.character(rorymonths[month(date_in)]),as.character(year(date_in)), sep ="")
}

strtodate <- function(z){
  z <- tolower(z) 
  as.Date(z, "%d%b%Y") # converts to date(year,month(jan,feb,mar,...),day)
}

urlgen <- function(strin,num){
  ns <- strtodate(strin) + days(7)
  ne <- strtodate(strin) + days(13)
  
  nstart <- datetostr(ns)
  nend <- datetostr(ne)
  range <- paste(nstart,nend,sep = "-")
  paste("https://cycling.data.tfl.gov.uk/usage-stats/",num,"JourneyDataExtract",range,".csv", sep = "")
}

dateextract <- function(link){
  substr(link,nchar(link)-22,nchar(link)-14)
}
numextract <- function(link){
  as.integer(substr(link,nchar(link)-43,nchar(link)-41))
}


newdataformatextraction <- function(startinglink){
  startnum <- numextract(startinglink) +1
  labels <- list()
  newlink <- startinglink

  for (i in startnum:375) {
    labels <- append(labels, newlink, after = length(labels))
    curlink <- newlink
    newlink <- urlgen(dateextract(curlink),i)
  }


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
  #startnum - 375
  for (i in 1:((375-startnum)+1)) {
    nframe <- read.csv(labels[[i]])
    #nframe = subset(nframe, select = -c(Bike.model,Total.duration,Total.duration..ms.,Bike.number,End.station.number,Start.station.number))

    nframe <- nframe %>%  filter((Start.station %in% c(stationinfon.station))== TRUE | (End.station %in% c(stationinfon.station))== TRUE)
    
    nframe = subset(nframe, select = -c(Bike.model,Total.duration,Total.duration..ms.,Bike.number,End.station.number,Start.station.number,Number))
    dataframe = rbind(dataframe,nframe)
    print(i)
  }
  return(dataframe)
}

newdata <- newdataformatextraction("https://cycling.data.tfl.gov.uk/usage-stats/335JourneyDataExtract12Sep2022-18Sep2022.csv")

testdata <- newdata

olddataformatextraction <- function(startinglink){
  startnum <- numextract(startinglink) +1
  csvlist <- list()
  holdlink <- startinglink
   
  
  lock <- 246
  
  for (n in startnum:335) {
    csvlist <- append(csvlist, holdlink, after = length(csvlist))
    templink <- holdlink
    if (n > lock){
      n <- n -1
    }
      
    holdlink <- urlgen(dateextract(templink),n)
  }
  
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
  
  titles <- c("Number","Start.date","Start.station.number","Start.station","End.date","End.station.number","End.station","Bike.number","Total.duration..ms.")

  for (x in 1:((335-startnum)+1)) {
    frame <- read.csv(csvlist[[x]])
    if (x != 206){
      nframe <- frame[, c(1,7,8,9,4,5,6,3,2)] # a way to subset and rorder rows
      colnames(nframe) <- titles
      nframe <- nframe %>%  filter((Start.station %in% c(stationinfon.station))== TRUE | (End.station %in% c(stationinfon.station))== TRUE)
      

      nframe <- subset(nframe, select = -c(Total.duration..ms.,Bike.number,End.station.number,Start.station.number,Number))
      
      olddata <- rbind(olddata,nframe)
    }
    print(x)
  }

  return(olddata)
}

olddata <- olddataformatextraction("https://cycling.data.tfl.gov.uk/usage-stats/121JourneyDataExtract01Aug2018-07Aug2018.csv")


Finalset <- rbind(olddata,newdata)

# 112 listing on the web has june not jun for some reason.
write.csv(Finalset, file = "C:/Imperial/WC_data.csv")
