# Read the data from the CSV files and extract the station names
data <- read.csv("WC_W1_EC_data.csv")
stations <- read.csv("station-postcodes-central.csv")
data$Start.date <- as.character(data$Start.date)
station_names <- stations$`Station`

#create a function reformat the date so it is consistent throughout
#in the format '2018-11-21 06:54'
reformat_date <- function(date_str) {
  if (grepl("/", date_str)) {
    year_characters <- substr(date_str, 7, 10)
    month_characters <- substr(date_str, 4, 5)
    day_characters <- substr(date_str, 1, 2)
    time_part <- substr(date_str, 12, 16)
    reformatted_date <- paste0(year_characters, "-", month_characters, "-", day_characters, " ", time_part)
  } else {
    # Date string doesn't contain a "/", keep it as is
    reformatted_date <- date_str
  }
  return(reformatted_date)
}

#run the date reformatting function on the data
data$Start.date <- sapply(data$Start.date, reformat_date)

#sort all the rows so they appear in chronological order
sorted_indices <- order(data$Start.date)
data <- data[sorted_indices, ]

#create a function that takes a start date as an argument,
#looks at all the associated rows and counts the activity of each station
count_hour <- function(hour, data, station_names) {
  
  #get a boolean list that returns true if a row matches the start date
  matching_rows <- grepl(hour, data$Start.date)
  
  # Find the index of the first TRUE in the boolean list
  start_idx <- which(matching_rows)[1]
  
  # Find the index of the last TRUE in the boolean list
  end_idx <- tail(which(matching_rows), n = 1)
  
  #loop through the rows set by the previous ranges and count the station names
  #this assumes the rows are grouped together in chronological order
  if (start_idx <= end_idx && end_idx <= nrow(data)) {
    subset_data <- data[start_idx:end_idx, ]
    station_counts <- integer(length(station_names))
    names(station_counts) <- station_names
    
    for (station in station_names) {
      station_counts[station] <- sum(subset_data$Start.station == station, subset_data$End.station == station)
    }
    
    return(station_counts)
  } else {
    
    #handle the error
    cat("Invalid start or end indices provided.\n")
    return(NULL)
  }
}

#define a new function that runs the hour activity counting function for a given time
#and appends it to the data set defined by the argument df
add_data <- function(hour,df) {
  station_counts <- count_hour(hour, data, station_names)
  station_counts <- unname(station_counts)
  
  # Convert 'hour' to a list so it can be appended
  hour_as_list <- list(hour)
  
  # Combine 'hour_as_list' and 'station_counts' into one list
  new_row <- c(hour_as_list, station_counts)
  
  #bind the new list to a new row in the data and return the new data set
  df <- rbind(df, new_row)
  return(df)
}

# Create an empty data frame for the reformatted data
df <- data.frame()

#create a list of all available times in the data
#find the first and last date and hour in the data set
first_date <- sub(":.*", "", data$Start.date[1])
last_date <- sub(":.*", "", data$Start.date[nrow(data)])

#set up a variable to iterate the date to append it to a list of available dates
date <- ""

#set up a list to fill with the available date and times, the rows of the new data set.
available_dates <- list()
available_dates <- append(available_dates, substr(first_date, 1, 10))

#find a day to start with and reformat it as a number
day_chars <- substr(first_date, 9, 10)
day <- as.numeric(day_chars)

#find a month to start with and reformat it as a number
month_chars <- substr(first_date, 6, 7)
month <- as.numeric(month_chars)

#find a year to start with and reformat it as a number
year_chars <- substr(first_date, 1, 4)
year <- as.numeric(year_chars)

#repeat for the last date
last_day_chars <- substr(last_date, 9, 10)
last_day <- as.numeric(last_day_chars)
last_month_chars <- substr(last_date, 6, 7)
last_month <- as.numeric(last_month_chars)
last_year_chars <- substr(last_date, 1, 4)
last_year <- as.numeric(last_year_chars)
goal_hours <- (last_year * 8064) + (last_month * 672) + (last_day*24)

# set up while loop to iterate with an hour at a time until the total hours
#is greater than the total hours defined by the end date
while (((year * 8064) + (month * 672) + (day*24)) < goal_hours) {
  
  #logic to increment days and hours
  if (day < 28) {
    day <- day + 1
    time <- 0
  } else if (month < 12) {
    month <- month + 1
    day <- 1
    time <- 0
  } else {
    year <- year + 1
    month <- 1
    day <- 1
    time <- 0
  }
  
  #reformat each date as a string in the correct format for the data
  formatted_day <- sprintf("%02d", day)
  formatted_month <- sprintf("%02d", month)
  date <- paste0(year,"-",formatted_month,"-",formatted_day)
  available_dates <- append(available_dates, date)
}

#loop through all the hours in the list of available hours and run add_data to count and populate
#the new data set df
for (i in available_dates) {
  df <- add_data(i,df)
  print(i)
}

#add the relevant headers and export the csv
column_names <- c("Hour", station_names)
colnames(df) <- column_names
write.csv(df, "hourly-data-central.csv")


