  
  #====================================================================================================
  #
  # Time series cross validation
  # 
  #====================================================================================================
  
    rm(list=ls())
    library(here)
    library(caret)
    library(forecast)
    library(tidyverse)

    # Data
    jobs <- readRDS("C:/Users/a.kelly/Documents/Projects/_Github/geelong-jobs/Data/ABS_geelong_jobs_raw.RDS")
  
    # Forecast horizon
    h = 16
    
    # Timeslices
    timeslices <- createTimeSlices(jobs[,1], initialWindow = 40, fixedWindow = TRUE, horizon = h)
    
    # Matrix to store errors
    arima.error <- ets.error <- ave.error <- matrix(NA, length(timeslices$train), h)
    
    # Loop
    for (i in 1:length(timeslices$train)) {
    
      # Subset data
      train <- jobs[timeslices$train[i] %>% unlist() %>% unname(), 1] # <--- First column of data only, need additional loop to do other columns of data
      test <- jobs[timeslices$test[i] %>% unlist() %>% unname(), 1]   # <--- First column of data only, need additional loop to do other columns of data
    
      # Forecasts
      arima <- forecast(auto.arima(train), h = h)["mean"] %>% unlist() %>% unname()
      ets <- forecast(ets(train), h = h)["mean"] %>% unlist() %>% unname()
      ave <- rowMeans(cbind(arima, ets))
      
      # Calculatet error and store
      arima.error[i, 1:length(arima)] <- abs(arima - test)
      ets.error[i, 1:length(arima)] <- abs(ets - test)
      ave.error[i, 1:length(arima)] <- abs(ave - test)   
  
    }
    
    # Plot
    plot(colMeans(arima.error), type = "l", col = 2, xlab = "Forecast horizon", ylab = "Mean absolute error")
    lines(colMeans(ets.error), col = 3)
    lines(colMeans(ave.error), col = 4)
    legend("topleft", legend = c("ARIMA", "ETS", "Average"), col = 2:4, lty = 1)
    
    