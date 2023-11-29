# Read the data from the CSV file that has data on each postcode
data <- read.csv("data/daily-activity-by-postcode.csv")

#load in the file that handles the short forecasting with linear regression
source("data-processing/short-forecasting.R")


### - Descriptive Analytics and Smoothing - ###

#define function to smooth the data with exponential smoothing
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

#create a data frame with the data for WC1
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_wc1 <- data$WC1
smooth_wc1 <- smooth_data(raw_wc1, 0.1, 2000)
wc1_data <- data.frame("Date" = dates,
                       "Raw" = raw_wc1,
                       "Smooth" = smooth_wc1)

#create a data frame with the data for WC2
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_wc2 <- data$WC2
smooth_wc2 <- smooth_data(raw_wc2, 0.1, 2000)
wc2_data <- data.frame("Date" = dates,
                       "Raw" = raw_wc2,
                       "Smooth" = smooth_wc2)

#create a data frame with the data for EC1
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_ec1 <- data$EC1
smooth_ec1 <- smooth_data(raw_ec1, 0.1, 2000)
ec1_data <- data.frame("Date" = dates,
                       "Raw" = raw_ec1,
                       "Smooth" = smooth_ec1)

#create a data frame with the data for EC2
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_ec2 <- data$EC2
smooth_ec2 <- smooth_data(raw_ec2, 0.1, 2000)
ec2_data <- data.frame("Date" = dates,
                       "Raw" = raw_ec2,
                       "Smooth" = smooth_ec2)

#create a data frame with the data for EC3
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_ec3 <- data$EC3
smooth_ec3 <- smooth_data(raw_ec3, 0.1, 2000)
ec3_data <- data.frame("Date" = dates,
                       "Raw" = raw_ec3,
                       "Smooth" = smooth_ec3)

#create a data frame with the data for EC4
#one column for the data, one column for the smoothed data
dates <- data$Date
raw_ec4 <- data$EC4
smooth_ec4 <- smooth_data(raw_ec4, 0.1, 2000)
ec4_data <- data.frame("Date" = dates,
                       "Raw" = raw_ec4,
                       "Smooth" = smooth_ec4)

#collect data on all stations including a calculation of the activity per station
#for each postcode
postcode_labels <- c("ec1","ec2","ec3","ec4","wc1","wc2")
activity_means <-colMeans(data[3:8])
activity_sd <- sapply(data[3:8], sd)
activity_max <- numeric()

#find the max value in each column
for (i in postcode_labels){
  data_string = paste0("smooth_",i)
  activity_max = c(activity_max, round(max(get(data_string)),digits=0))
}

#hard code the number of stations in each postcode and calculate the
#ratio of activity to number of stations in each postcode
n_stations <- c(29,23,9,14,29,23)
activity_aps <- unlist(Map("/", activity_max, n_stations))
activity_sd <- round(activity_sd, digits = 0)

#round the data
activity_means <- round(activity_means, digits = 0)
activity_aps <- round(activity_aps, digits = 1)

#create data frame with statistics on each station
postcode_statistics <- data.frame("Postcode" = postcode_labels,
                                  "Stations" = n_stations,
                                  "Mean" = activity_means,
                                  "sd" = activity_sd,
                                  "max" = activity_max,
                                  "Ratio"= activity_aps)

#create empty list to fill with emojis and colours to describe data
emojis <- list()
colours <- list()

#add emoji and colour information to describe the data - for rendering on the UI
for (i in 1: length(postcode_statistics$Ratio)){
  if (postcode_statistics$Ratio[i] >= 200){
    emojis[i] <- "angry"
    colours[i] <- "red"
  } else if (postcode_statistics$Ratio[i] >= 125){
    emojis[i] <- "disappointed"
    colours[i] <- "orange"
  } else {
    emojis[i] <- "smile"
    colours[i] <- "green"
  }
}

#append the new info to the data frame
postcode_statistics$Emoji <- emojis
postcode_statistics$Colour <- colours



### - Capacity Simulation of App - ###

#create a function simulates the daily capacity of the postcode given system parameters
capacity.simulation <- function(n_stations,activity_per_station,initial_fill_percentage,active_hours,docks_per_station){
  
  #derive simulation parameters from input variables
  total_docks = n_stations*docks_per_station
  initial_full_docks = round((initial_fill_percentage*total_docks), digits = 0)
  daily_activity = n_stations*activity_per_station
  
  #create array that randomly represents the number of transactions that occur in
  #each of the active hours of the day, most transactions will occur in hour 3 and hour 8
  hourly <- abs(rnorm(active_hours))
  hourly[3] <- max(abs(hourly))
  hourly[8] <- max(abs(hourly))
  hourly <- round(hourly / sum(hourly) * daily_activity)
  
  #create an array of (-1)s or (1)s where each represents a user action
  #(1) represents a user arriving at a station
  #(-1) represents a user taking a bike from a station
  #for central london, in earlier hours the the probability of a user arriving is higher
  #in later hours the probability of a user leaving is higher transaction
  
  # set proportion of journeys into the postcode based on traffic data https://www.tomtom.com/traffic-index/london-traffic/
  proportion <- c(1.44,1.32,1.12,0.87,0.49,0.30,0.82,4.91,11.66,11.96,4.02,2.88,
                  2.96,3.05,3.37,6.05,8.19,10.13,9.26,4.99,3.00,2.16,1.97,2.08)
  
  # set probability of action being an arrival based on data here https://assets.publishing.service.gov.uk/media/5b57023440f0b63391c87ff6/rail-passengers-crowding-2017.pdf
  probability <- c(0.50,0.50,0.71,0.82,0.82,0.73,0.42,0.85,0.84,0.78,0.68,0.66,0.54,
                   0.46,0.44,0.41,0.31,0.17,0.14,0.17,0.14,0.18,0.14,0.17)
  
  #create an empty list to fill with the simulated activity
  activity=numeric()
  
  #loop through the active hours of the day
  for (i in 1:active_hours){
    
    #create an empty list to store all the actions that occur probalistically in each hour
    hour_activity=numeric()
    
    #within each hour, create a loop that repeats for every journey that is made in that hour
    #defined by the proportions above
    for (j in 1:((proportion[i]*daily_activity)%/%100)){
      
      #take a random sample of 1 or -1 for each action based on the probability above
      hourly[i] = 1
      random_array <- sample(c(1, -1), size = hourly[i], replace = TRUE,
                             prob = c(probability[i], 1 - probability[i]))
      
      #add 1 or subtract 1 from the number of bikes in the postcode in this hour
      #depending on if it is a 1 or a -1 (arrive or leave)
      hour_activity=c(hour_activity,random_array)
      hour_activity = sum(hour_activity)
    }
    
    #append the number of bikes in the postcode in this hour
    activity=c(activity,hour_activity)
  }
  
  #create new lists to determine how many bikes are in the postcode at any one time
  capacity=numeric()
  capacity=c(capacity,initial_full_docks)
  
  #loop through the all the hours do cumulative addition to find the total number of bikes in the postcode at any one time
  for (i in 1:length(activity)){
    new_capacity = capacity[length(capacity)] + activity[i]
    capacity=c(capacity,new_capacity)
  }
  
  #create array to plot the maximum capacity
  max_capacity = rep(total_docks, times = length(capacity))
  
  #create a data frame to return teh system parameters back if they need to be referenced
  simulation_parameters = data.frame(
    "NumberStations" = n_stations,
    "ActivityPerStation" = activity_per_station,
    "MaxCapacity" = total_docks
  )
  
  #create a data frame to return the results
  simulation_results = data.frame(
    "MaxCapacity" = max_capacity,
    "Capacity" = capacity
  )
  
  #return the result
  return(list(simulation_parameters,simulation_results,capacity,max_capacity))
}



### - Long term Holt Winters forecasting of the app - ###

#create function that interpolates data to restore the full length of data points from an array that has been compressed
interpolate_data <- function(original_data, new_length) {
  original_length <- length(original_data)
  
  # Create a sequence of indices for the original data
  original_indices <- seq(1, original_length, length.out = original_length)
  
  # Create a sequence of indices for the new data
  new_indices <- seq(1, original_length, length.out = new_length)
  
  # Use the approx function for linear interpolation
  interpolated_data <- approx(original_indices, original_data, xout = new_indices)$y
  
  return(interpolated_data)
}

#function that takes data and completes Holt Winter forecasting over a compressed array
holtwinter <- function(data){
  
  #define model parameters for Holt winters coefficients
  m  <- 28
  aa <- 0.5
  bb <- 0.1
  gg <- 0.9
  ti <- 50
  lv <- 2000
  bv <- 0
  sv <- rep(0,m+1)
  fc <- 60
  cut <- 50
  
  #reduce the data down to make processing easier for the model
  reduced <- data[seq(1, length(data), length.out = cut)]
  yt <- reduced
  
  #complete holts winter smoothing from discrete time equations
  for (i in 1:m) {
    lv[i+1] <- aa*yt[i] + (1-aa)*(lv[i]+bv[i]) 
    bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
  }
  for (i in (m+1):ti) {
    lv[i+1] <- aa*(yt[i]-sv[i+1-m]) + (1-aa)*(lv[i]+bv[i]) 
    bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
    sv[i+1] <- gg*(yt[i]-lv[i]-bv[i]) + (1-gg)*sv[i+1-m]
  }
  
  #forecast for a duration defined in the simulation parameters above
  kk <- seq(1,fc,1)
  ik <- floor((kk-1)/m)
  yf <- lv[i+1] + bv[i+1] * kk + sv[i+1+kk-m*(ik+1)]
  
  #combine the known and forecast data and use a function to scale it up proportionately to the needed for the app
  modelled_data <- c(lv,yf)
  return_data <- interpolate_data(modelled_data, round(length(data)*((cut+fc)/cut)))
  return(return_data)
}

#function to fill in space with future dates
get_dates_sequence <- function(start_date, length_of_list) {
  date_sequence <- seq(as.Date(start_date), by = "days", length.out = length_of_list)
  return(date_sequence)
}

#define the dates for which we are fitting the holt-winters forecast
dates <- data$Date
start_date_index <- which(data$Date == '2020-07-01')
end_date_index <- length(data$Date)

#prepare all the data for the forecast plot and store it in variables that can be accessed by the app
#WC1
wc1.obs <- smooth_data(data$WC1,0.025,1250)[start_date_index:end_date_index]
wc1.model <- holtwinter(wc1.obs)
pad <- rep(NaN,length(wc1.model)-length(wc1))
wc1.obs <- c(wc1.obs,pad)

#wc2
wc2.obs <- smooth_data(data$WC2,0.025,1250)[start_date_index:end_date_index]
wc2.model <- holtwinter(wc2.obs)
wc2.obs <- c(wc2.obs,pad)

#ec1
ec1.obs <- smooth_data(data$EC1,0.025,1250)[start_date_index:end_date_index]
ec1.model <- holtwinter(ec1.obs)
ec1.obs <- c(ec1.obs,pad)

#ec2
ec2.obs <- smooth_data(data$EC2,0.025,1250)[start_date_index:end_date_index]
ec2.model <- holtwinter(ec2.obs)
ec2.obs <- c(ec2.obs,pad)

#ec3
ec3.obs <- smooth_data(data$EC3,0.025,1250)[start_date_index:end_date_index]
ec3.model <- holtwinter(ec3.obs)
ec3.obs <- c(ec3.obs,pad)

#ec4
ec4.obs <- smooth_data(data$EC4,0.025,1250)[start_date_index:end_date_index]
ec4.model <- holtwinter(ec4.obs)
ec4.obs <- c(ec4.obs,pad)

#Date
date.fc <- data$Date[start_date_index:end_date_index]
date_pad <- as.character((get_dates_sequence(date.fc[length(date.fc)], ((length(wc1.model)-length(date.fc))+1)))[-1])
date.fc <- append(date.fc,date_pad)


### - Using the long term forecast to optimize the number of stations needed in 2024 - ###

#function that finds the optimal number of stations for a given set of data given the max allowed daily activity per station
find_optimal <- function(data,max.station){
  
  #define an embedded function that can be used as a loss function for the optimiser.
  #each demanded journey exceeding capacity is a loss of £1.65 and
  #each jounney that doesn't meet capacity is a loss proportional to the cost of the station
  #spread of a year and the maintence costs, it is assumed to be £4.78
  loss <- function(error){
    if (error>0){
      loss <- error*1.65
    } else {
      loss <- error*4.78
    }
    return (abs(loss))
  }
  
  #function get the total loss for the error across the year
  total_loss <- function(n.stations){
    error <- data - n.stations*max.station
    loss <- sapply(error, loss)
    return(sum(loss))
  }
  
  #use Brent optimisation and the loss function defined above to minimise the total loss over the set duration
  #by finding the optimal number of stations in the postcode
  optimal <- optim(par = 10, fn = total_loss, method = "Brent", lower = 0, upper = 50)
  return(round(optimal[1][[1]],digits=0))
}

#repeat the previous function but return the total loss when number of stations is not optimal
loss_n_station <- function(data, n.stations, max.station){
  
  #this function uses the same loss function per day
  loss <- function(error){
    if (error>0){
      loss <- error*1.65
    } else {
      loss <- error*4.78
    }
    return (abs(loss))
  }
  
  #this function uses the same loss function over the whole duration of data
  total_loss <- function(n.stations){
    error <- data - n.stations*max.station
    loss <- sapply(error, loss)
    return(sum(loss))
  }
  
  return (total_loss(n.stations))
}


