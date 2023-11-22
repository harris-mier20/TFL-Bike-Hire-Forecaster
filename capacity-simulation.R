library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ggplot2)
#Getting a list of all stations within London along with their postcodes
stationinfo <- read.csv("data/station-postcodes-central.csv")

#defining which postcodes are to be included in the output dataset c("W1","WC","EC")(all central london postcode first two letters)
twol <- c("EC1","EC2","EC3","EC4","WC1","WC2")

source("data-preprocessing/get-postcode-usage.R")

stationinfon <- stationexrtraction.list(stationinfo,twol)

tfl_bike_daily_activity_central_london <- read_csv("data/tfl-bike-daily-activity-central-london.csv")

names(tfl_bike_daily_activity_central_london)<-make.names(names(tfl_bike_daily_activity_central_london),unique = TRUE)

df <- read_csv("data/daily-activity-by-postcode.csv")

selectedpostcode <- tfl_bike_daily_activity_central_london[stationinfon[[2]]]

for ( i in 1:length(stationinfon)){
  selectedpostcode <- tfl_bike_daily_activity_central_london[stationinfon[[i]]]
  X <- ncol(selectedpostcode)
  selectedpostcode$Total_0s<-rowSums(selectedpostcode==0)
  selectedpostcode$Total_0s<-X - selectedpostcode$Total_0s
  X <- ncol(df) + 1
  df$X <- selectedpostcode$Total_0s
  names(df)[X] <- paste(names(stationinfon)[i],".num.of.sattions")
}

selectedpostcode$Date<-tfl_bike_daily_activity_central_london$Date




#define simulation parameters
n_stations = 20
docks_per_station = 27 #ref https://content.tfl.gov.uk/developer-guidance-for-santander-cycles.pdf
active_hours = 10
initial_fill_percentage = 0.15 #25% of the central London docking points are full in the morning

#adjust the number of daily transactions of the model
activity_per_station = 280

#derive simulation parameters from variables
total_docks = n_stations*docks_per_station
initial_full_docks = round((initial_fill_percentage*total_docks), digits = 0)
daily_activity = n_stations*activity_per_station

#create array that randomly represents the number of transactions that occur in
#each of the 10 active hours of the day, most transactions will occur in hour 3 and hour 8
hourly <- abs(rnorm(active_hours))
hourly[3] <- max(abs(hourly))
hourly[8] <- max(abs(hourly))
hourly <- round(hourly / sum(hourly) * daily_activity)

#create an array of 0s or 1s where each represents a user action
#(1) represents a user arriving at a station
#(-1) represents a user taking a bike from a station
#for central london, in earlier hours the the probability of a user arriving is higher
#in later hours the probability of a user leaving is higher
#transaction
probability = seq(0.7, 0.3, length.out = active_hours)
activity=numeric()
for (i in 1:length(hourly)){
  random_array <- sample(c(1, -1), size = hourly[i], replace = TRUE,
                         prob = c(probability[i], 1 - probability[i]))
  activity=c(activity,random_array)
}

#loop through the all the activity, add one to the capacity if a user brings in a bike,
#subtract one from the capacity if a bike is taken 
capacity=numeric()
capacity=c(capacity,initial_full_docks)
for (i in 1:length(activity)){
  new_capacity = capacity[length(capacity)] + activity[i]
  capacity=c(capacity,new_capacity)
}

#create array to plot the maximum capacity
max_capacity = rep(total_docks, times = length(capacity))

simulation_parameters = data.frame(
  "NumberStations" = n_stations,
  "ActivityPerStation" = activity_per_station,
  "MaxCapacity" = total_docks
)

simulation_results = data.frame(
  "MaxCapacity" = max_capacity,
  "Capacity" = capacity
)

#save these parameters and results in csvs to reference in the dashboard
write.csv(simulation_parameters, "data/capacity-simulation/simulation-parameters.csv", row.names=FALSE)
write.csv(simulation_results, "data/capacity-simulation/simulation-results.csv", row.names=FALSE)

#plot the data
plot(capacity, type = 'l', xlab="Journey Made In/Out Postcode", 
     ylab="Bikes Docked in Postcode", ylim = c(0,600))
lines(max_capacity, color = 'red')

