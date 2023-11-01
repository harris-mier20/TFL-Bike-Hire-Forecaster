# Read data from a CSV file named "temperature.csv" and store it in the 'temp' data frame.
temp <- read.csv("temperature.csv")

# Replace missing values in the 'Boston' column with the mean of non-missing values.
temp$Boston[which(is.na(temp$Boston))] = mean(temp$Boston, na.rm = TRUE)

# Find the index of the row where 'datetime' is "2016-01-01 00:00:00".
testbound <- which(temp$datetime == "2016-01-01 00:00:00")

# Create a new data frame 'boston' containing 'datetime' and 'Boston' columns.
boston <- temp[, c("datetime", "Boston")]

# Create 'bostontrain' data frame containing data from the beginning to the 'testbound' index.
bostontrain <- temp[1:testbound, c("datetime", "Boston")]

# Create 'bostontest' data frame containing data from 'testbound' to the end of 'temp'.
bostontest <- temp[testbound:nrow(temp), c("datetime", "Boston")]

# Calculate the number of forecasts and the number of time lags.
nf <- nrow(temp) - 24
nk <- 24

# Initialize empty matrices 'yfp' and 'ym'.
#future prediction dataser
yfp <- matrix(, nf - testbound + 1, nk)
# mean predictor
ym <- matrix(, nf - testbound + 1, nk)

# Loop through the rows starting from 'testbound' to 'nf'.
for (i in testbound:nf) {
  # replicates the last value 24 times as the next datapoint
  yfp[i - testbound + 1, ] <- rep(boston$Boston[i], nk)
  
  # replicates the last value 24 times as the next datapoint based on the mean of the previsu row
  ym[i - testbound + 1, ] <- boston$Boston[(i + 1):(i + nk)]
}

# Calculate the error by subtracting 'yfp' from 'ym'.
error <- ym - yfp

# Calculate the bias, mean absolute error (MAE), and root mean squared error (RMSE) for each time lag.
bias <- colMeans(error)
mae <- colMeans(abs(error))
rmse <- (colMeans(error^2))^0.5

# Create a plot of bias, MAE, and RMSE against the lead time in hours.
plot(1:24, bias, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "black",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)
axis(1, seq(1, 24, 1))
axis(2, seq(-1, 9, 1))
par(new = TRUE)

# Add MAE to the existing plot in green.
plot(1:24, mae, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "green",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)
par(new = TRUE)

# Add RMSE to the existing plot in red.
plot(1:24, rmse, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "red",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)


yc <- mean(boston$Boston[1:testbound-1])

nf <- length(boston$Boston)-24
nk <- 24
yfc <- matrix(,nf-testbound+1,nk)
ym <- matrix(,nf-testbound+1,nk)
for (i in testbound:nf) {
  yfc[i-testbound+1,] <- rep(yc,nk)
  ym[i-testbound+1,] <- boston$Boston[(i+1):(i+nk)]
}
# Calculate the error by subtracting 'yfp' from 'ym'.
error <- ym - yfp

# Calculate the bias, mean absolute error (MAE), and root mean squared error (RMSE) for each time lag.
bias <- colMeans(error)
mae <- colMeans(abs(error))
rmse <- (colMeans(error^2))^0.5

# Create a plot of bias, MAE, and RMSE against the lead time in hours.
plot(1:24, bias, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "black",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)
axis(1, seq(1, 24, 1))
axis(2, seq(-1, 9, 1))
par(new = TRUE)

# Add MAE to the existing plot in green.
plot(1:24, mae, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "green",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)
par(new = TRUE)

# Add RMSE to the existing plot in red.
plot(1:24, rmse, type = "l", xlim = c(1, 24), ylim = c(-1, 9), col = "red",
     xlab = "lead time [h]", ylab = "error [deg]", axes = FALSE)
