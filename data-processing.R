# Read the data from the CSV files and extract the station names
data <- read.csv("daily-activity-by-postcode.csv")

#collect data on all stations
postcode_labels <- c("ec1","ec2","ec3","ec4","wc1","wc2")
activity_means <-colMeans(data[3:8])
activity_sd <- sapply(data[3:8], sd)
n_stations <- c(29,23,9,14,29,23)

#round the data
activity_means <- round(activity_means, digits = 0)
activity_sd <- round(activity_sd, digits = 0)

#create data frame with statistics on each station
postcode_statistics <- data.frame("Postcode" = postcode_labels,
                                  "Stations" = n_stations,
                                  "Mean" = activity_means,
                                  "SD"=activity_sd)
