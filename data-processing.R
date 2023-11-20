# Read the data from the CSV files and extract the station names
data <- read.csv("data/daily-activity-by-postcode.csv")
sim_parameters <- read.csv("data/capacity-simulation/simulation-parameters.csv")
sim_results <- read.csv("data/capacity-simulation/simulation-results.csv")

#collect data on all stations including a calculation of the activity per station
#for each postcode
postcode_labels <- c("ec1","ec2","ec3","ec4","wc1","wc2")
activity_means <-colMeans(data[3:8])
activity_sd <- sapply(data[3:8], sd)
activity_max <- numeric()

#find the max value in each column
for (i in 3:8){
  activity_max = c(activity_max, max(data[[i]]))
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
  if (postcode_statistics$Ratio[i] >= 225){
    emojis[i] <- "angry"
    colours[i] <- "red"
  } else if (postcode_statistics$Ratio[i] >= 200){
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


### - handle the long term forecasting of the app - ###

#create function that interpolates data to restore the full length of data points from a string
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

#function that takes data and completes Holt Winter forecasting
holtwinter <- function(data){
  
  #define model parameters
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
  
  #reduce the data down for the model
  reduced <- data[seq(1, length(data), length.out = cut)]
  yt <- reduced
  
  #complete holts winter smoothing
  for (i in 1:m) {
    lv[i+1] <- aa*yt[i] + (1-aa)*(lv[i]+bv[i]) 
    bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
  }
  for (i in (m+1):ti) {
    lv[i+1] <- aa*(yt[i]-sv[i+1-m]) + (1-aa)*(lv[i]+bv[i]) 
    bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
    sv[i+1] <- gg*(yt[i]-lv[i]-bv[i]) + (1-gg)*sv[i+1-m]
  }
  
  #forecast for a duration defined above
  kk <- seq(1,fc,1)
  ik <- floor((kk-1)/m)
  yf <- lv[i+1] + bv[i+1] * kk + sv[i+1+kk-m*(ik+1)]
  
  #combine and interpolate data to return it to the previous resolution
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

#prepare all the data for the forecast plot
#WC1
wc1 <- smooth_data(data$WC1,0.025,1250)[start_date_index:end_date_index]
wc1.model <- holtwinter(wc1)
pad <- rep(NaN,length(wc1.model)-length(wc1))
wc1 <- c(wc1,pad)

#wc2
wc2 <- smooth_data(data$WC2,0.025,1250)[start_date_index:end_date_index]
wc2.model <- holtwinter(wc2)
wc2 <- c(wc2,pad)

#ec1
ec1 <- smooth_data(data$EC1,0.025,1250)[start_date_index:end_date_index]
ec1.model <- holtwinter(ec1)
ec1 <- c(ec1,pad)

#ec2
ec2 <- smooth_data(data$EC2,0.025,1250)[start_date_index:end_date_index]
ec2.model <- holtwinter(ec2)
ec2 <- c(ec2,pad)

#ec3
ec3 <- smooth_data(data$EC3,0.025,1250)[start_date_index:end_date_index]
ec3.model <- holtwinter(ec3)
ec3 <- c(ec3,pad)

#ec4
ec4 <- smooth_data(data$EC4,0.025,1250)[start_date_index:end_date_index]
ec4.model <- holtwinter(ec4)
ec4 <- c(ec4,pad)

#Date
date <- data$Date[start_date_index:end_date_index]
date_pad <- as.character((get_dates_sequence(date[length(date)], ((length(wc1.model)-length(date))+1)))[-1])
date <- append(date,date_pad)

#create a data frame with the date and then the measured and forecast activity
#each postcode
forecast <- data.frame("Date"=date, "WC1measured"=wc1, "WC1modelled"=wc1.model,
                       "WC2measured"=wc2, "WC2modelled"=wc2.model,
                       "EC1measured"=ec1, "EC1modelled"=ec1.model,
                       "EC2measured"=ec2, "EC2modelled"=ec2.model,
                       "EC3measured"=ec3, "EC3modelled"=ec3.model,
                       "EC4measured"=ec4, "EC4modelled"=ec4.model)


