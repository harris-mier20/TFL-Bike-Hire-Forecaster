#load in all the available feature data for training the model
wc1 <- read.csv("data/feature-data/WC1-feature-data.CSV")
wc2 <- read.csv("data/feature-data/WC2-feature-data.CSV")
ec1 <- read.csv("data/feature-data/EC1-feature-data.CSV")
ec2 <- read.csv("data/feature-data/EC2-feature-data.CSV")
ec3 <- read.csv("data/feature-data/EC3-feature-data.CSV")
ec4 <- read.csv("data/feature-data/EC4-feature-data.CSV")
weatherdata <- read.csv("data/weather-data.csv")

#split the data for training, validation and testing
wc1.train <- wc1[1:423,]
wc1.valid <- wc1[500:564,]
wc1.test <- wc1[565:705,]

wc2.train <- wc2[1:423,]
wc2.valid <- wc2[500:564,]
wc2.test <- wc2[565:705,]

ec1.train <- ec1[1:423,]
ec1.valid <- ec1[500:564,]
ec1.test <- ec1[565:705,]

ec2.train <- ec2[1:423,]
ec2.valid <- ec2[500:564,]
ec2.test <- ec2[565:705,]

ec3.train <- ec3[1:423,]
ec3.valid <- ec3[500:564,]
ec3.test <- ec3[565:705,]

ec4.train <- ec4[1:423,]
ec4.valid <- ec4[500:564,]
ec4.test <- ec4[565:705,]

#function that takes training data and validation data as arguments and list of features
#fits a regression model and return the rmse
fit_model <- function(train,valid,features){
  
  #create a string with the chosen variables to input into the regression model
  formula_string <- paste("Daydemand ~", paste(features, collapse = " + "))
  
  #create the model
  model <- lm(formula_string, train)
  predict <- predict(model, valid)
  
  #find and return the rmse
  err <- valid$Daydemand - predict
  rmse <- sqrt(mean(err^2))
  print(rmse)
  return(model)
}

#to prevent over-fitting, a function that takes in regression model parameters and removes features that
#have a coefficients with parameters less than a curtain magnitude
feature_selection <- function(model){
  
  #create empty list of features
  selected_features = list()
  
  #loop through coefficients
  for (i in 2:length(model[1]$coefficients)){
    
    #add if coefficient is bigger than 0.05
    if (abs(model[1]$coefficients[[i]]) > 0.01){
      selected_features <- c(selected_features,names(model[1]$coefficients)[i])
    }
  }
  return(selected_features)
}

#function that takes an index and a number of days
#returns the mean of that number of days before that index in the data set
find_mean <- function(index,data,days){
  values_to_check = numeric()
  for (i in 1:days){
    values_to_check <- c(values_to_check,data[index-i])
  }
  return(mean(values_to_check))
}

#create a function that uses the selected features and runs the model day by day
#it iteratively fills in the data frame treating a forecast as if it actually occurred
future_forecast <- function(available_data, desired_dates, model){
  
  #set up the data frame
  df <- data.frame("Date" = available_data$Date, "Daydemand"=available_data$Daydemand,
                   "Demand1dayago"=available_data$Demand1dayago, "Demand7daysago"=available_data$Demand7daysago,
                   "Meanpast7days"=available_data$Meanpast7days, "Meanpast365days"= available_data$Meanpast365days,
                   "Tempofday"=available_data$Tempofday, "Wind"=available_data$Wind, "Raincover"=available_data$Raincover)
  
  #record when the new data starts
  start_index <- length(df$Date)
  
  #loop through all the new dates and fit the model every day.
  for (i in 1:length(desired_dates)){
    
    new_entry = data.frame(
      "Date" = desired_dates[i],
      "Demand1dayago" = df$Daydemand[start_index+i-1],
      "Demand7daysago" = df$Daydemand[start_index+i-7],
      "Meanpast7days" = find_mean(start_index+i,df$Daydemand,7),
      "Meanpast365days" = find_mean(start_index+i,df$Daydemand,365),
      "Tempofday" = weatherdata$temp[weatherdata$datetime == desired_dates[i]],
      "Wind" = weatherdata$windspeed[weatherdata$datetime == desired_dates[i]],
      "Raincover" = weatherdata$precipcover[weatherdata$datetime == desired_dates[i]]
      )
    
    #use the data to make a forecast  
    predicted_demand <- predict(model, new_entry)
    
    #append the new data to the row ready to repeat
    new_entry$"Daydemand" <- predicted_demand
    df <- rbind(df,new_entry)
  }
  
  #an offset of 500 is needed to remove the bias in the predicted demand
  df[[2]] <- df[[2]] + 500
  return(df)
}


#test the rmse on different combinations of input features
test1 <- fit_model(wc1.train,wc1.test,c("Demand7daysago","Wind","Raincover"))
test2 <- fit_model(wc1.train,wc1.test,c("Demand1dayago","Tempofday"))

#get a list of all the available features and fit a model and find the best features
features <- colnames(wc1)[3:(length(colnames(wc1))-1)]
model <- fit_model(wc1.train,wc1.valid,features)
selected_features <- feature_selection(model)

### - The selected features are:
#"Demand1dayago",
#"Demand7daysago",
#"Meanpast7days",
#"Meanpast365days",
#"Tempofday",
#"Wind",
#"Raincover"

#use the selected features to create a model on the test data and plot it against the data itself.
#start by defining the selected features for the model
formula_string <- paste("Daydemand ~", paste(selected_features, collapse = " + "))

#create the model for wc1
wc1.model <- lm(formula_string, rbind(wc1.train,wc1.valid))
wc1.predict <- predict(model, wc1.test)
#plot(wc1.test$Daydemand,type='l',ylim=c(0,3500))
#lines(wc1.predict,type='l',col='red')

#create models for the other postcodes
wc1.model <- lm(formula_string, rbind(wc1.train,wc1.valid))
wc2.model <- lm(formula_string, rbind(wc2.train,wc2.valid))
ec1.model <- lm(formula_string, rbind(ec1.train,ec1.valid))
ec2.model <- lm(formula_string, rbind(ec2.train,ec2.valid))
ec3.model <- lm(formula_string, rbind(ec3.train,ec3.valid))
ec4.model <- lm(formula_string, rbind(ec4.train,ec4.valid))

#create the future forecasts
wc1.fc <- future_forecast(rbind(wc1.train,wc1.valid), wc1.test$Date, wc1.model)
wc2.fc <- future_forecast(rbind(wc2.train,wc2.valid), wc2.test$Date, wc2.model)
ec1.fc <- future_forecast(rbind(ec1.train,ec1.valid), ec1.test$Date, ec1.model)
ec2.fc <- future_forecast(rbind(ec2.train,ec2.valid), ec2.test$Date, ec2.model)
ec3.fc <- future_forecast(rbind(ec3.train,ec3.valid), ec3.test$Date, ec3.model)
ec4.fc <- future_forecast(rbind(ec4.train,ec4.valid), ec4.test$Date, ec4.model)

#plot the forecast model against the test data
#plot(wc1.test$Daydemand,type='l',ylim=c(0,3500))
#lines(wc1.fc$Daydemand,type='l',col='red')

# define the list of future dates
start_date_index <- which(weatherdata$datetime == '2022-07-06')
end_date_index <- length(weatherdata$datetime)
future_dates <- weatherdata[start_date_index:end_date_index,]$datetime

#create a long term forecast for future bike usage
ec2.fc.long <- future_forecast(ec2, future_dates, ec2.model)
plot(ec2.fc.long$Daydemand,type='l')




