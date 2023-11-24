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
  names(df)[X] <- paste0(names(stationinfon)[i],".num.of.stations")
}

selectedpostcode$Date<-tfl_bike_daily_activity_central_london$Date




### a function which outputs simulation results based of input

capacity.simulation <- function(n_stations,activity_per_station,initial_fill_percentage,active_hours,docks_per_station){
  
  #derive simulation parameters from variables
  total_docks = n_stations*docks_per_station
  initial_full_docks = round((initial_fill_percentage*total_docks), digits = 0)
  daily_activity = n_stations*activity_per_station
  
  #create array that randomly represents the number of transactions that occur in
  #each of the 10 active hours of the day, most transactions will occur in hour 3 and hour 8
  print(active_hours)
  hourly <- abs(rnorm(active_hours))
  hourly[3] <- max(abs(hourly))
  hourly[8] <- max(abs(hourly))
  print(hourly)
  hourly <- round(hourly / sum(hourly) * daily_activity)
  print("hourly")
  print(hourly)
  print("capacity")
  
  #create an array of 0s or 1s where each represents a user action
  #(1) represents a user arriving at a station
  #(-1) represents a user taking a bike from a station
  #for central london, in earlier hours the the probability of a user arriving is higher
  #in later hours the probability of a user leaving is higher
  #transaction
  # set probability based on this https://www.tomtom.com/traffic-index/london-traffic/
  #probability = seq(0.7, 0.3, length.out = active_hours)
  proportion <- c(1.44,1.32,1.12,0.87,0.49,0.30,0.82,4.91,12.66,11.96,4.02,2.88,
    2.96,3.05,3.37,6.05,8.19,10.13,9.26,4.99,3.00,2.16,1.97,2.08)
  # probability based from chart 10 here https://assets.publishing.service.gov.uk/media/5b57023440f0b63391c87ff6/rail-passengers-crowding-2017.pdf
  probability <- c(0.50,0.40,0.71,0.77,0.86,0.83,0.46,0.90,0.83,0.73,0.69,0.62,0.54,
                   0.46,0.44,0.41,0.31,0.17,0.14,0.17,0.14,0.18,0.14,0.17)
  print("probability")
  print(probability)
  activity=numeric()
  print("capacity")
  
  for (i in 1:active_hours){
    hour_activity=numeric()
    for (j in 1:((proportion[i]*daily_activity)%/%100)){
      hourly[i] = 1
      random_array <- sample(c(1, -1), size = hourly[i], replace = TRUE,
                            prob = c(probability[i], 1 - probability[i]))
      hour_activity=c(hour_activity,random_array)
      hour_activity = sum(hour_activity)
    }
    activity=c(activity,hour_activity)
  }
  print("activity")
  print(activity)
  print("capacity")
  #loop through the all the activity, add one to the capacity if a user brings in a bike,
  #subtract one from the capacity if a bike is taken 
  capacity=numeric()
  capacity=c(capacity,initial_full_docks)
  print("capacity")
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
  return(list(simulation_parameters,simulation_results,capacity,max_capacity))
}

# function which takes in the date, postcode area, infil percentage active hours
# and docks per station as inputs for the simulation
# and returns a list of the parameters in following order:
# n_stations,
#activity_per_station
#infill_percentage
#active_hours
#docks_per_station
# it pulls the number of stations and activity per station from the dataframe 
# based on an input date and chosen area
input.parameter.pull <- function(date,postcode,df,infill_percentage,active_hours,docks_per_station){
  #pull the input parameters from the dashboard
  df <- df
  infill_percentage = infill_percentage
  active_hours = active_hours
  docks_per_station = docks_per_station
  n_stations = df[df$Date == date, paste0(postcode,".num.of.stations")]
  activity_per_station = (df[df$Date == date, postcode])/n_stations
  return(list(n_stations,activity_per_station,infill_percentage,active_hours,docks_per_station))
}

#define simulation parameters
n_stations = 20
docks_per_station = 27 #ref https://content.tfl.gov.uk/developer-guidance-for-santander-cycles.pdf
active_hours = 24
initial_fill_percentage = 0.15 #25% of the central London docking points are full in the morning

#adjust the number of daily transactions of the model
activity_per_station = 250


par <- input.parameter.pull("2018-08-01","WC1",df,0.15,24,27)

#save these parameters and results in csvs to reference in the dashboard
#simulation_parameters = capacity.simulation(n_stations,docks_per_station,active_hours,initial_fill_percentage,activity_per_station)[1]
#write.csv(simulation_parameters, "data/capacity-simulation/simulation-parameters.csv", row.names=FALSE)
#simulation_results = capacity.simulation(n_stations,docks_per_station,active_hours,initial_fill_percentage,activity_per_station)[2]
#write.csv(simulation_results, "data/capacity-simulation/simulation-results.csv", row.names=FALSE)

#plot the data

capacity = capacity.simulation(n_stations,activity_per_station,initial_fill_percentage ,active_hours,docks_per_station)[3]
max_capacity = capacity.simulation(n_stations,activity_per_station,initial_fill_percentage ,active_hours,docks_per_station)[4]



plot(seq(0,length(capacity[[1]])-1),capacity[[1]], type = 'l', xlab="Journey Made In/Out Postcode", ylab="Bikes Docked in Postcode", ylim = c(0,1800))
print(max_capacity)
lines(seq(0,length(capacity[[1]])-1),max_capacity[[1]], color = 'red')

#lines(max_capacity, color = 'red')







