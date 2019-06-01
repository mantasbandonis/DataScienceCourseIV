# Implementing a demand Model

# Load all necessary Libraries
library(prophet)
library(lubridate)
library(Metrics)
library(dplyr)

# Read in Datasets and store them in Variables (Four different items)
item1 <- read.csv(file="D_00001", header=TRUE, sep=",", row.name=1)
item2 <- read.csv(file="D_00002", header=TRUE, sep=",", row.name=1)
item3 <- read.csv(file="D_00003", header=TRUE, sep=",", row.name=1)
item4 <- read.csv(file="D_00004", header=TRUE, sep=",", row.name=1)

# change Variable Names according to the Package Prothet, so it works
names(item1) <- c("ds", "price", "promotion", "y")
names(item2) <- c("ds", "price", "promotion", "y")
names(item3) <- c("ds", "price", "promotion", "y")
names(item4) <- c("ds", "price", "promotion", "y")

# change ds from integer to date object, so Prophet Package can handle it (starting in 2014 due to no more information on specific date)
item1$ds <- lubridate::ymd( "2014-01-01" ) + lubridate::weeks( item1$ds - 1 )
item2$ds <- lubridate::ymd( "2014-01-01" ) + lubridate::weeks( item2$ds - 1 )
item3$ds <- lubridate::ymd( "2014-01-01" ) + lubridate::weeks( item3$ds - 1 )
item4$ds <- lubridate::ymd( "2014-01-01" ) + lubridate::weeks( item4$ds - 1 )

# Clean the data bevor fitting the model (remove all rows with wrong datapoints)
## Remove Rows where promotion is not 0 or 1
item1 <- subset(item1, item1$promotion == 1 | item1$promotion == 0)
item2 <- subset(item2, item2$promotion == 1 | item2$promotion == 0)
item3 <- subset(item3, item3$promotion == 1 | item3$promotion == 0)
item4 <- subset(item4, item4$promotion == 1 | item4$promotion == 0)

## remove rows where sales are under 0
item1 <- subset(item1, item1$y >= 0 )
item2 <- subset(item2, item2$y >= 0 )
item3 <- subset(item3, item3$y >= 0 )
item4 <- subset(item4, item4$y >= 0 )

### create Reference Prices
#### Function to add Reference Price to Dataframe
refPrice <- function(df){
  rows <- nrow(df)
  for(i in 1:rows){
    if(i == 1) {
      df[i, 5] <- df[i,2]
    }else {
      df[i, 5] <- (df[i-1,5] + 0.1 * max(0,df[i-1,2] - df[i-1,5]) + 0.3 * min(0,df[i-1,2] - df[i-1,5]))
    }
  }
  names(df) <- c("ds", "price", "promotion", "y", "RefPrice")
  return(df[,c(1,2,5,3,4)])
}

item1 <- refPrice(item1)
item2 <- refPrice(item2)
item3 <- refPrice(item3)
item4 <- refPrice(item4)


# Split Data into Train (First two years) and Test (Last Year) set
item1_train <- item1[1:which(item1[,1] == '2016-01-06') - 1, ,]
item1_test <- item1[which(item1[,1] == '2016-01-06'):nrow(item1), ,]
item2_train <- item2[1:which(item2[,1] == '2016-01-06') - 1, ,]
item2_test <- item2[which(item2[,1] == '2016-01-06'):nrow(item2), ,]
item3_train <- item3[1:which(item3[,1] == '2016-01-06') - 1, ,]
item3_test <- item3[which(item3[,1] == '2016-01-06'):nrow(item3), ,]
item4_train <- item4[1:which(item4[,1] == '2016-01-06') - 1, ,]
item4_test <- item4[which(item4[,1] == '2016-01-06'):nrow(item4), ,]



# Build Models
##  Facebook prophet model only with time as a predictor (2a)
ts_a_1 <- prophet(yearly.seasonality=TRUE)
ts_a_2 <- prophet(yearly.seasonality=TRUE)
ts_a_3 <- prophet(yearly.seasonality=TRUE)
ts_a_4 <- prophet(yearly.seasonality=TRUE)

## Facebook Prophet Model with additional Price and Promotion as predictors (2b)
### create dataframes for Facebook Prophet model indicating the promotions
### promotion = seperate dataframe with only time and promotion indicator as variables
promotion1 <- data_frame(
  holiday = 'promotion',
  ds = item1[item1$promotion == 1, ]$ds
)
promotion2 <- data_frame(
  holiday = 'promotion',
  ds = item2[item2$promotion == 1, ]$ds
)
promotion3 <- data_frame(
  holiday = 'promotion',
  ds = item3[item3$promotion == 1, ]$ds
)
promotion4 <- data_frame(
  holiday = 'promotion',
  ds = item4[item4$promotion == 1, ]$ds
)

### build models with Time and Promotion
ts_b_1 <- prophet(yearly.seasonality=TRUE, holidays = promotion1)
ts_b_2 <- prophet(yearly.seasonality=TRUE, holidays = promotion2)
ts_b_3 <- prophet(yearly.seasonality=TRUE) # not using promotions because there are not any
ts_b_4 <- prophet(yearly.seasonality=TRUE, holidays = promotion4)

### Add price as additional Regressor Variable to the Model
ts_b_1 <- add_regressor(ts_b_1, 'price')
ts_b_2 <- add_regressor(ts_b_2, 'price')
ts_b_3 <- add_regressor(ts_b_3, 'price')
ts_b_4 <- add_regressor(ts_b_4, 'price')

## Facebook Prophet Model with Price, Promotion and Reference Price as predictive variables
### build models
### use models from 2b and just add Reference Price as another Regressor Variable
ts_c_1 <- add_regressor(ts_b_1, 'RefPrice')
ts_c_2 <- add_regressor(ts_b_2, 'RefPrice')
ts_c_3 <- add_regressor(ts_b_3, 'RefPrice')
ts_c_4 <- add_regressor(ts_b_4, 'RefPrice')



# Fit all three models for every item with the corresponding data
## Item 1
ts_a_1 <- fit.prophet(ts_a_1, item1_train)
ts_b_1 <- fit.prophet(ts_b_1, item1_train)
ts_c_1 <- fit.prophet(ts_c_1, item1_train)

## Item 2
ts_a_2 <- fit.prophet(ts_a_2, item2_train)
ts_b_2 <- fit.prophet(ts_b_2, item2_train)
ts_c_2 <- fit.prophet(ts_c_2, item2_train)

# Item 3
ts_a_3 <- fit.prophet(ts_a_3, item3_train)
ts_b_3 <- fit.prophet(ts_b_3, item3_train)
ts_c_3 <- fit.prophet(ts_c_3, item3_train)

# Item4
ts_a_4 <- fit.prophet(ts_a_4, item4_train)
ts_b_4 <- fit.prophet(ts_b_4, item4_train)
ts_c_4 <- fit.prophet(ts_c_4, item4_train)


# create future series which includes the third year
## Necessary for Facebook Prophet to predict Values for every time given in 'Futute'
future1 <- item1[,c('ds','price','RefPrice')]
future2 <- item2[,c('ds','price','RefPrice')]
future3 <- item3[,c('ds','price','RefPrice')]
future4 <- item4[,c('ds','price','RefPrice')]


# make prediction which prothet for all three models per Item
## Predict for Item 1
forecast_a_1 <- predict(ts_a_1, future1)
forecast_b_1 <- predict(ts_b_1, future1)
forecast_c_1 <- predict(ts_c_1, future1)

## Predict for Item 2
forecast_a_2 <- predict(ts_a_2, future2)
forecast_b_2 <- predict(ts_b_2, future2)
forecast_c_2 <- predict(ts_c_2, future2)

## Predict for Item 3
forecast_a_3 <- predict(ts_a_3, future3)
forecast_b_3 <- predict(ts_b_3, future3)
forecast_c_3 <- predict(ts_c_3, future3)

## Predict for Item 4
forecast_a_4 <- predict(ts_a_4, future4)
forecast_b_4 <- predict(ts_b_4, future4)
forecast_c_4 <- predict(ts_c_4, future4)


# Evaluate Models using Mean Absolute Error (MAE),  Root Meand Squared Error (RMSE) and Mean Absolute Standardized Error (MASE) and the Absolute Error
## Create an evaluation function which return MAE, RMSE and MAPE
evaluate_model <- function(actual, predictions, in_sample = TRUE){
  index_holdout_start <- which(actual[,1] == '2016-01-06') # create a variable which indicates the first row of the third year
  if(in_sample == TRUE){ # Use only the rows of the first two years to evalute the model 
    mae <- round(mae(actual[1:index_holdout_start - 1, ncol(actual)], predictions[1:index_holdout_start - 1, ncol(predictions)]),3)
    rmse <- round(rmse(actual[1:index_holdout_start - 1, ncol(actual)], predictions[1:index_holdout_start - 1, ncol(predictions)]),3)
    mase <- round(mase(actual[1:index_holdout_start - 1, ncol(actual)], predictions[1:index_holdout_start - 1, ncol(predictions)]),3)
    ae <- sum(actual[1:index_holdout_start - 1, ncol(actual)]) - sum(predictions[1:index_holdout_start - 1, ncol(predictions)])
  }else if (in_sample == FALSE){ # Use only the third year to evalute the model
    mae <- round(mae(actual[index_holdout_start:nrow(actual), ncol(actual)], predictions[index_holdout_start: nrow(predictions), ncol(predictions)]),3)
    rmse <- round(rmse(actual[index_holdout_start:nrow(actual), ncol(actual)], predictions[index_holdout_start: nrow(predictions), ncol(predictions)]),3)
    mase <- round(mase(actual[index_holdout_start:nrow(actual), ncol(actual)], predictions[index_holdout_start: nrow(predictions), ncol(predictions)]),3)
    ae <- sum(actual[index_holdout_start:nrow(actual), ncol(actual)]) - sum(predictions[index_holdout_start: nrow(predictions), ncol(predictions)])
  }
  results <- matrix(c(mae, rmse, mase, ae), ncol = 4)
  colnames(results) <- c("MAE","RMSE","MASE","AE")
  results <- as.table(results)
  return(results)
}

## Evaluate Model with only Time as Predictor Variable
### In sample Performance (Use only first two years, which where already used for fitting the model)
evaluate_model(item1, forecast_a_1)
evaluate_model(item2, forecast_a_2)
evaluate_model(item3, forecast_a_3)
evaluate_model(item4, forecast_a_4)

### Holdout Period Performance (Use only the third year which was not used during model creation)
evaluate_model(item1, forecast_a_1, in_sample = FALSE)
evaluate_model(item2, forecast_a_2, in_sample = FALSE)
evaluate_model(item3, forecast_a_3, in_sample = FALSE)
evaluate_model(item4, forecast_a_4, in_sample = FALSE)

## Evaluate Model with Time, Price and Promotion as Predictor Variables
### In sample Performance
evaluate_model(item1, forecast_b_1)
evaluate_model(item2, forecast_b_2)
evaluate_model(item3, forecast_b_3)
evaluate_model(item4, forecast_b_4)

### Holdout Period Performance
evaluate_model(item1, forecast_b_1, in_sample = FALSE)
evaluate_model(item2, forecast_b_2, in_sample = FALSE)
evaluate_model(item3, forecast_b_3, in_sample = FALSE)
evaluate_model(item4, forecast_b_4, in_sample = FALSE)

## Evalutate Model with Time, Price, Reference Price and Promotion as Predictor Variables
### In sample Performance
evaluate_model(item1, forecast_c_1)
evaluate_model(item2, forecast_c_2)
evaluate_model(item3, forecast_c_3)
evaluate_model(item4, forecast_c_4)

### Holdout Period Performance
evaluate_model(item1, forecast_c_1, in_sample = FALSE)
evaluate_model(item2, forecast_c_2, in_sample = FALSE)
evaluate_model(item3, forecast_c_3, in_sample = FALSE)
evaluate_model(item4, forecast_c_4, in_sample = FALSE)



# Plot Time Series and different Components of the Models
## Plots for Item 1
plot(ts_a_1, forecast_a_1, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_a_1, forecast_a_1)

plot(ts_b_1, forecast_b_1, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_b_1, forecast_b_1)

plot(ts_c_1, forecast_c_1, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_c_1, forecast_c_1)

## Plots for Item 2
plot(ts_a_2, forecast_a_2, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_a_2, forecast_a_2)

plot(ts_b_2, forecast_b_2, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_b_2, forecast_b_2)

plot(ts_c_2, forecast_c_2, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_c_2, forecast_c_2)

## Plots for Item 3
plot(ts_a_3, forecast_a_3, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_a_3, forecast_a_3)

plot(ts_b_3, forecast_b_3, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_b_3, forecast_b_3)

plot(ts_c_3, forecast_c_3, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_c_3, forecast_c_3)

##Plots for Item 4
plot(ts_a_4, forecast_a_4, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_a_4, forecast_a_4)

plot(ts_b_4,forecast_b_4, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_b_4, forecast_b_4)

plot(ts_c_4, forecast_c_4, ylab= "Sales", xlab="Timeframe")
prophet_plot_components(ts_c_4, forecast_c_4)
