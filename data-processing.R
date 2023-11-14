# Read the data from the CSV files and extract the station names
data <- read.csv("daily-activity-by-postcode.csv")

#collect data on all stations including a calculation of the activity per station
#for each postcode
postcode_labels <- c("ec1","ec2","ec3","ec4","wc1","wc2")
activity_means <-colMeans(data[3:8])

#hard code the number of stations in each postcode and calculate the
#ratio of activity to number of stations in each postcode
n_stations <- c(29,23,9,14,29,23)
activity_aps <- unlist(Map("/", activity_means, n_stations))

#round the data
activity_means <- round(activity_means, digits = 0)
activity_aps <- round(activity_aps, digits = 1)

#create data frame with statistics on each station
postcode_statistics <- data.frame("Postcode" = postcode_labels,
                                  "Stations" = n_stations,
                                  "Mean" = activity_means,
                                  "Ratio"= activity_aps)

#create empty list to fill with emojis and colours to describe data
emojis <- list()
colours <- list()

#add emoji and colour information to describe the data - for rendering on the UI
for (i in 1: length(postcode_statistics$Ratio)){
  if (postcode_statistics$Ratio[i] >= 90){
    emojis[i] <- "angry"
    colours[i] <- "red"
  } else if (postcode_statistics$Ratio[i] >= 80){
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

#for each postcode create a data frame that for every day starting from 2020-06, the columns are:
#activity for that day, activity 1 day before, activity 7 days before, mean activity for that week,
#mean activity for that month, activity one year earlier, temperature on that day, rain fall on that day

#weather data is collected from weather-data.csv




