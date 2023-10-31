# Install and load the ggmap package
library(ggmap)
library(opencage)

#provide API access keys, they are removed for security reasons,
#but can be accessed via a valid google or opencage account
api <- "-"
register_google(key=api)
api_key <- "-"

# Read the data from a single week long data file
# provided by TFL here:
#https://cycling.data.tfl.gov.uk/usage-stats/371JourneyDataExtract22May2023-28May2023.csv
data <- read.csv("371JourneyDataExtract22May2023-28May2023.csv")

# Get unique values from the 'start station' column
start_station_values <- unique(data$`Start.station`)

# Get unique values from the 'end station' column
end_station_values <- unique(data$`End.station`)

# Find the common unique values
common_values <- intersect(start_station_values, end_station_values)

#create a new row that copies the station names but adds ',London' to assist
#the API postcode searching function
london_names <- paste(common_values, ", London", sep = "")

# create a new data frame with this infomation
common_values_df <- data.frame(Station = common_values, London = london_names)

#create a new function to find the postcode of a given location
#using its index within the new data set
get_postcode <- function(index) {
  
  #look up the address using the index and create the geocode
  address <- common_values_df$London[index]
  geocode_result <- ggmap::geocode(address)
  
  # Define the latitude and longitude from the previous geocoding result
  lat <- geocode_result$lat
  lon <- geocode_result$lon
  
  #check if the longitude and latitude are valid
  if (!is.na(lat) && !is.na(lon)) {
    
    #reverse geocode using opencage and extract the postcode from that result
    result <- opencage_reverse(lat = lat, lon = lon, key = api_key)
    postcode <- result$results$components.postcode
    return(postcode)
    
    #just return the word "empty" if a postcode can't be found.
    #They can be filled in manually later
  } else {
    return('Empty')
  }
  
}

#loop through the data frame, finding the postcode and appending it each time
for (i in 1:length(common_values)) {
  common_values_df$Postcode[i] <- get_postcode(i)
}

#create a new numeric list to assign the station numbers
station_numbers <- numeric(length(common_values))

# Iterate through the common station names
for (i in 1:length(common_values)) {
  station_name <- common_values[i]
  
  # Find the corresponding station number in the 'start station number' or 'end station number' column
  if (any(data$`Start.station` == station_name)) {
    station_numbers[i] <- data[data$`Start.station` == station_name, "Start.station.number"]
  } else if (any(data$`End.station` == station_name)) {
    station_numbers[i] <- data[data$`End.station` == station_name, "End.station.number"]
  } else {
    # Handle the case when no match is found
    station_numbers[i] <- NA
  }
}

#add a new column to the data frame with the new station numbers
common_values_df$Number <- station_numbers

#remove the London column, it is no longer needed
common_values_df <- common_values_df[, !names(common_values_df) == "London"]

#filter out the stations that have postcodes starting with EC or WC
df_filtered <- subset(common_values_df, grepl("^EC|^WC", Postcode))

#export csv files for both data sets
write.csv(df_filtered, "station-postcodes-central.csv")
write.csv(common_values_df, "station-postcodes.csv")


