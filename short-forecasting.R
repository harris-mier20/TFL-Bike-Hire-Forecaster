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
wc1.fc <- lm(formula_string, rbind(wc1.train,wc1.valid))
wc1.predict <- predict(wc1.fc, wc1.test)
wc1.err <- wc1.test$Daydemand - wc1.predict
wc1.rmse <- round(sqrt(mean(wc1.err^2)), digits =0)
wc1.values <- unname(wc1.fc[[1]])

#wc2
wc2.fc <- lm(formula_string, rbind(wc2.train,wc2.valid))
wc2.predict <- predict(wc2.fc, wc2.test)
wc2.err <- wc2.test$Daydemand - wc2.predict
wc2.rmse <- round(sqrt(mean(wc2.err^2)), digits =0)
wc2.values <- unname(wc2.fc[[1]])

#ec1
ec1.fc <- lm(formula_string, rbind(ec1.train,ec1.valid))
ec1.predict <- predict(ec1.fc, ec1.test)
ec1.err <- ec1.test$Daydemand - ec1.predict
ec1.rmse <- round(sqrt(mean(ec1.err^2)), digits =0)
ec1.values <- unname(ec1.fc[[1]])

#ec2
ec2.fc <- lm(formula_string, rbind(ec2.train,ec2.valid))
ec2.predict <- predict(ec2.fc, ec2.test)
ec2.err <- ec2.test$Daydemand - ec2.predict
ec2.rmse <- round(sqrt(mean(ec2.err^2)), digits =0)
ec2.values <- unname(ec2.fc[[1]])

#ec3
ec3.fc <- lm(formula_string, rbind(ec3.train,ec3.valid))
ec3.predict <- predict(ec3.fc, ec3.test)
ec3.err <- ec3.test$Daydemand - ec3.predict
ec3.rmse <- round(sqrt(mean(ec3.err^2)), digits =0)
ec3.values <- unname(ec3.fc[[1]])

#ec4
ec4.fc <- lm(formula_string, rbind(ec4.train,ec4.valid))
ec4.predict <- predict(ec4.fc, ec4.test)
ec4.err <- ec4.test$Daydemand - ec4.predict
ec4.rmse <- round(sqrt(mean(ec4.err^2)), digits =0)
ec4.values <- unname(ec4.fc[[1]])




