library(forecast)

data <- read.csv("data/daily-activity-by-postcode.csv")

#function to smooth data
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

#define the dates for which we are fitting the holt-winters forecast
dates <- data$Date
start_date_index <- which(data$Date == '2019-09-01')
end_date_index <- length(data$Date)

#prepare all the vectors for the model
wc1 <- smooth_data(data$WC1,0.1,1250)[start_date_index:end_date_index]
wc2 <- smooth_data(data$WC2,0.1,1250)[start_date_index:end_date_index]
ec1 <- smooth_data(data$EC1,0.1,1250)[start_date_index:end_date_index]
ec2 <- smooth_data(data$EC2,0.1,1250)[start_date_index:end_date_index]
ec3 <- smooth_data(data$EC3,0.1,1250)[start_date_index:end_date_index]
ec4 <- smooth_data(data$EC4,0.1,1250)[start_date_index:end_date_index]

#function that takes array and model parameters for Holt-Winters additive model
yt <- wc1

m  <- 125
aa <- 0.2
bb <- 0.1
gg <- 0.1
ti <- 1500
lv <- 200
bv <- 0
sv <- rep(0,m+125)

for (i in 1:m) {
  lv[i+1] <- aa*yt[i] + (1-aa)*(lv[i]+bv[i]) 
  bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
}

for (i in (m+1):ti) {
  lv[i+1] <- aa*(yt[i]-sv[i+1-m]) + (1-aa)*(lv[i]+bv[i]) 
  bv[i+1] <- bb*(lv[i+1]-lv[i]) + (1-bb)*bv[i] 
  sv[i+1] <- gg*(yt[i]-lv[i]-bv[i]) + (1-gg)*sv[i+1-m]
}

kk <- seq(1,125,1)
ik <- floor((kk-1)/m)
yf <- lv[i+1] + bv[i+1] * kk + sv[i+1+kk-m*(ik+1)]

plot(yt, type = 'l')

